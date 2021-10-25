/**************************************************************
 This file is part of
        Logo Inpainting for AviSynth by Wolfgang Boiger
 Copyright (C)  2007, 2008  Wolfgang Boiger, Berlin
 
 This program is free software; you can redistribute it and/or
 modify it under the terms of the GNU General Public License
 version 2, as published by the Free Software Foundation in
 June 1991 (see 'GNUGPLv2.txt').
 
 There is no guaranty that the rights of third parties
 (including software patents) are not infringed upon by the
 execution or distribution of this program.
 Referring to section 8 of the General Public License, any use
 of this program (including all activities mentioned in
 section 0 and including execution) is restricted to countries
 where any of these activities do not infringe upon any such
 rights of third parties. It lies with the user to verify the
 compliance of his or her use (especially concerning but not
 limited to distribution) of this program with the rights of
 any third parties in the respective countries or regions.
 
 This program is distributed in the hope that it will be
 useful, but WITHOUT ANY WARRANTY; without even the implied
 warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
 PURPOSE.  See the GNU General Public License for more details.
 
 You should have received a copy of the GNU General Public
 License 2 along with this program; if not, write to the
   Free Software Foundation, Inc.,
   51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 **************************************************************/

// 2008.02.23 - Original version
// 20190624
// change to AVS+ headers
// change avs_is_yuy to avs_is_yuv (Function name changed, it is not the same as avs_is_yuy2!)
// Add resource file for DLL versioning, version 1.1
// For newer changes: see readme

//#define  VersionString  "2008.02.23"
#define  VersionString  "2019.06.27 1.2"
#define  LegalInfoString  "* Logo Inpainting for AviSynth by Wolfgang Boiger *\nVersion " VersionString " ;  built  " __DATE__ "  " __TIME__ " .\nCopyright (C)  2007, 2008  Wolfgang Boiger, Berlin.\nLicensed under GNU GPL version 2 with possible geographic restrictions.\nThis software comes without any warranty, even without warranty\nof merchantability or fitness for a particular purpose.\nSee files 'AVSInpaint.htm' and 'GNUGPLv2.txt' for details."


#define  BlurStandardDeviation  1  /* 1:blur-intensity=standard-deviation of Gauss-Kernel  0:blur-intensity=scale of autonomous heat equation u_t=tr(u_xx) */
#define  MaskThreshold  127  /* When the position of a logo (or something else) is given by a Y8-mask, the logo is supposed to be where  Sign(MT)*Mask>MT  holds */

#define  InpaintMinRadiusXY  1.001 /* Will consider at least the 5-point-stencil */
#define  InpaintMinPreBlurXYKernelSize  0    /* It should hold  InpaintMinPreBlurXYKernelSize+InpaintMinPostBlurXYKernelSizeShould>=2  */
#define  InpaintMinPostBlurXYKernelSize  2
#define  InpaintMinPreBlurXYStandardDeviation  0.0
#define  InpaintMinPostBlurXYStandardDeviation  0.1
#define  InpaintFullMaskError  1  /* What to do if the whole frame should be inpainted?  0: Leave it unchanged,  1: Throw error on static logo,  2: Throw error even on dynamic logo */
#define  InpaintFailError  1  /* What if inpainting a certain pixel failed? 0: Leave it unchanged,  1: Throw error */
#define  InpaintChromaFailError  0  /* What if inpainting a certain subsampled chroma pixel failed? 0: Leave it unchanged (even if InpaintFailError==1),  1: Throw error */

#define  FMMHeapDelta  8  /* If heap size needs to be increased (= reallocation of memory), how many elemnts to add */
#define  FMMHeapInitialSize  (4*(Width+Height+FMMHeapDelta))  /* Initial heap size: variables "Width" and "Height" are allowed */


typedef  signed long int  Analyze_Sum1Type;  /* Data types used for summation in Analyze function */
typedef  unsigned long int  Analyze_Sum2Type;
#define  Analyze_Sum1Max  ((LONG_MAX+LONG_MIN>0)?(-LONG_MIN):(+LONG_MAX))  /* Maximal values for summation data types (summation will stop here) */
#define  Analyze_Sum2Max  ULONG_MAX

#define  Analyze_Result_OK  0
#define  Analyze_Result_GeneralError  -1
#define  Analyze_Result_SrcClipError  -2
#define  Analyze_Result_DestFrameError  -3
#define  Analyze_Result_MemoryError  -4

#define  InpaintMask_Result_NoMask  1
#define  InpaintMask_Result_OK  0
#define  InpaintMask_Result_GeneralError  -1
#define  InpaintMask_Result_FrameError  -2
#define  InpaintMask_Result_MemoryError  -3
#define  InpaintMask_Result_FullMask  -4

#define  FMMFarMarker  ((float)(32.0*INT_MAX))  /* Should be bigger than the maximal diameter of a frame */

#define  MaxChannelCount  4  /* Maximal number of color channels for all color spaces */



#if  (MaskThreshold>0)
#define  MaskThresholdSign  (+1)
#else
#define  MaskThresholdSign  (-1)
#endif



#include  <stdlib.h>
#include  <math.h>
#include  <limits.h>
#include  <float.h>
#include  <string.h>

#include  "AviSynth_C.h"



/* Structures used for inpaint data (including pre-computed data for static logos) */
typedef struct
{
  int  OffsetX, OffsetY, Width, Height, MaskPixelCount;
  unsigned int * MaskPixelOrder;
  BYTE * chiLuma, * chiChroma;
  float * GchiLuma, * GchiChroma;
}
InpaintMaskDataStruct;
  
typedef struct
{
  AVS_Clip * MaskClip;  /* 0 means static mask */
  int  MaskFrameCount, MaxSteps;
  /* Values given by the user (a bit processed) */
  double  PixelWidth, PixelHeight;  /* defined by pixel aspect ratio, product of both should be 1 !! */
  double  LumaWeight, ChromaWeight;  /* weights for structure tensor */
  double  LumaRadiusX, LumaRadiusY, ChromaRadiusX, ChromaRadiusY, Sharpness;  /* Different X/Y-radi will give an ellipse (due to PAR) */
  int  ChromaTensor;
  /* Color space dependent values */
  int  PixelType, Width, Height, ChromaPixelWidth, ChromaPixelHeight, LumaStride, ChromaStride, ChromaChannelCount, MaskPixelType, MaskStride, MaskChannelNo, FlipMask;  /* ChromaPixelWidth, ChromaPixelHeight -> chroma channel subsampling */
  /* Blurring stuff */
  double * LumaPreBlurXKernel, * LumaPreBlurYKernel, * ChromaPreBlurXKernel, * ChromaPreBlurYKernel;  /* (Preblurred data will be buffered later) */
  double * Luma4LumaPostBlurXKernel, * Luma4LumaPostBlurYKernel;  /* Postblurring will also convert between different samplings */
  double * Chroma4ChromaPostBlurXKernel, * Chroma4ChromaPostBlurYKernel;
  double * Luma4ChromaPostBlurXKernel, * Luma4ChromaPostBlurYKernel;  /* If ChromaPixelW/H is even, this points to (the weight of) the pixel *before* the center line */
  double ** Chroma4LumaPostBlurXKernels, ** Chroma4LumaPostBlurYKernels;
  double * LumaPreBlurXKernelVector, * LumaPreBlurYKernelVector, * ChromaPreBlurXKernelVector, * ChromaPreBlurYKernelVector;
  double * Luma4LumaPostBlurXKernelVector, * Luma4LumaPostBlurYKernelVector;
  double * Chroma4ChromaPostBlurXKernelVector, * Chroma4ChromaPostBlurYKernelVector;
  double * Luma4ChromaPostBlurXKernelVector, * Luma4ChromaPostBlurYKernelVector;
  double ** Chroma4LumaPostBlurXKernelVectors, ** Chroma4LumaPostBlurYKernelVectors;  /* Blurring Chroma to transport luma -> Different kernels for each luma pixel (inside one chroma pixel) */
  int  LumaPreBlurXKernelSize, LumaPreBlurYKernelSize, ChromaPreBlurXKernelSize, ChromaPreBlurYKernelSize;
  int  Luma4LumaPostBlurXKernelSize, Luma4LumaPostBlurYKernelSize;
  int  Chroma4ChromaPostBlurXKernelSize, Chroma4ChromaPostBlurYKernelSize;
  int  Luma4ChromaPostBlurXKernelSize, Luma4ChromaPostBlurYKernelSize;
  int  Chroma4LumaPostBlurXKernelSize, Chroma4LumaPostBlurYKernelSize;
  /* For static mask */
  InpaintMaskDataStruct * StaticMaskData;  /* 0 means dynamic mask */
}
InpaintDataStruct;


/* Heap used by fast marching method */
typedef  struct
{
  int  Size, MaxSize, Delta;
  int * MemPointer, * Heap;
}
FMMHeapStruct;


/* We need double bytes for deblending - ints are too large (needs too much memory) */
typedef struct
{
  BYTE  L, H;
}
DoubleByte;


/* Structure for deblending - with pre-computed data for static logo */
typedef struct
{
  int  PixelType, Width, Height, Stride[MaxChannelCount], PixelWidth[MaxChannelCount], PixelHeight[MaxChannelCount];
  /* Static logo */
  DoubleByte * StaticLogoDataChannels[MaxChannelCount];
  BYTE * StaticAlphaDataChannels[MaxChannelCount];
  /* Dynamic logo */
  AVS_Clip * LogoClip, * AlphaClip;
  int  LogoPixelType, AlphaPixelType, LogoFrameCount, AlphaFrameCount, LogoStride[MaxChannelCount], AlphaStride, FlipAlpha;
  /* Is static? Check (LogoClip)     Separate alpha clip? Check (AlphaClip) */
}
DeblendDataStruct;


/* Some data for the distance function filter */
typedef struct
{
  int  PixelType, LumaChannelNo, LumaStride, Width, Height;
  double  Scale, PixelWidth, PixelHeight;
  signed int  ScaleSign;
}
DistanceFunctionDataStruct;


/*****  Function declaration  *****/

/** AviSynth stuff **/
/* Inpaint callbacks */
AVS_Value  AVSC_CC  Inpaint_Create(AVS_ScriptEnvironment *  Env,  AVS_Value  Args,  void *  Data);
AVS_VideoFrame *  AVSC_CC  Inpaint_GetFrame(AVS_FilterInfo *  FilterInfo,  int  FrameNo);
void  AVSC_CC  Inpaint_Free(AVS_FilterInfo *  FilterInfo);
/* Deblend callbacks */
AVS_Value  AVSC_CC  Deblend_Create(AVS_ScriptEnvironment *  Env,  AVS_Value  Args,  void *  Data);
AVS_VideoFrame *  AVSC_CC  Deblend_Static_GetFrame(AVS_FilterInfo *  FilterInfo,  int  FrameNo);
AVS_VideoFrame *  AVSC_CC  Deblend_Dynamic_GetFrame(AVS_FilterInfo *  FilterInfo,  int  FrameNo);
void  AVSC_CC  Deblend_Free(AVS_FilterInfo *  FilterInfo);
/* Analyze callbacks */
AVS_Value  AVSC_CC  Analyze_Create(AVS_ScriptEnvironment *  Env,  AVS_Value  Args,  void *  Data);
AVS_VideoFrame *  AVSC_CC  Analyze_GetFrame(AVS_FilterInfo *  FilterInfo,  int  FrameNo);
void  AVSC_CC  Analyze_Free(AVS_FilterInfo *  FilterInfo);
/* DistanceFunction callbacks */
AVS_Value  AVSC_CC  DistanceFunction_Create(AVS_ScriptEnvironment *  Env,  AVS_Value  Args,  void *  Data);
AVS_VideoFrame *  AVSC_CC  DistanceFunction_GetFrame(AVS_FilterInfo *  FilterInfo,  int  FrameNo);
void  AVSC_CC  DistanceFunction_Free(AVS_FilterInfo *  FilterInfo);

/** Own functions **/
/* Common functions */
int  GetFramePointers(AVS_VideoFrame *  VideoFrame,  BYTE **  WritePtrs,  const BYTE **  ReadPtrs,  int *  Pitch,  int *  Stride,  int *  PixelWidth,  int *  PixelHeight,  int  PixelType);  /* Will simply return information/pointers about the frame given */
int  FastMarching(float  u[],  unsigned int *  Order,  int  Width,  int  Height,  double  PixelWidth,  double  PixelHeight);  /* Computes the signed distance function (and optionally returns a pixel order list) */
int  FMMUpdateCloseValue(float  u[],  int  Close[],  int  x,  int  y,  int  Width,  int  Height,  double  PixelWidth,  double  PixelHeight,  FMMHeapStruct *  Heap);  /* Helper for fast marching method: Recomputes a pixel value if it became close or surrounding pixels have changed */
/* Inpaint tools */
int  InpaintPrepareMask(InpaintMaskDataStruct **  MaskDataPtr,  AVS_VideoFrame *  Frame,  InpaintDataStruct *  InpaintData);  /* Prepares mask for inpainting - saves time if logo is static */
void  AddPreBlurValue(float  G[],  int  x,  int  y,  int  Width,  int  Height,  double  Value,  double  BlurXKernel[],  int  BlurXKernelSize,  double  BlurYKernel[],  int  BlurYKernelYize);  /* If a function chi*u gets a new value (chi changes), this function adds the (blurred) value to the corresponding matrix G*(chi*u) */
double  InpaintPixel(double  RadiusX,  double  RadiusY,  BYTE  u[],  BYTE  chi[],  int  Stride,  int  Pitch,  int  x,  int  y,  int  Width,  int  Height,  double  CoherenceStrength,  double  CoherenceDirectionX,  double  CoherenceDirectionY);  /* Calculates the (inpainted) value of a pixel for given coherence data */
double  GetCoherenceData(double *  CoherenceDirectionX,  double *  CoherenceDirectionY,  double  TensorXX,  double  TensorXY,  double  TensorYY,  double  Sharpness);  /* Computes the coherence direction and strength from a structuring tensor */
void  GetStructureTensor(double *  TensorXX,  double *  TensorXY,  double *  TensorYY,
                         float *  GchiLuma,  float *  GchiuLuma,  BYTE *  chiLuma,  double  LumaWeight,
                         float *  GchiChroma,  float **  GchiuChromas,  BYTE *  chiChroma,  double  ChromaWeight,
                         int  ChromaPixelWidth,  int  ChromaPixelHeight,  int  ChromaChannelCount,
                         int  x,  int  y,  int  Width,  int  Height,
                         double *  LumaBlurXKernel,  int  LumaBlurXKernelSize,  int  LumaXDirection,
                         double *  LumaBlurYKernel,  int  LumaBlurYKernelSize,  int  LumaYDirection,
                         double *  ChromaBlurXKernel,  int  ChromaBlurXKernelSize,  int  ChromaXDirection,
                         double *  ChromaBlurYKernel,  int  ChromaBlurYKernelSize,  int  ChromaYDirection);  /* Determines the total structuring tensor */
int  GetChannelTensor(double *  TensorXX,  double *  TensorXY,  double *  TensorYY,  float  Gchi[],  float  Gchiu[],  BYTE  chi[],  int  x,  int  y,  int  Width,  int  Height,  double  BlurKernelX[],  int  BlurKernelXSize,  double  BlurKernelY[],  int  BlurKernelYSize);  /* Computes the structuring tensor of a color channel */
double *  CreateGaussKernel(double  Kernel[],  int  Size,  double  Deviation,  double  Center);  /* Generates a 1d Gauss kernel for convolution*/
/* Deblend tools */
void  DeblendDynamic(BYTE  Data[],  const  BYTE  LogoData[],  const  BYTE  AlphaData[],  int  Pitch,  int  Stride,  int  LogoPitch,  int  LogoStride,  int  AlphaPitch,  int  AlphaStride,  int  Width,  int  Height);  /* Deblends a (not subsamples) color channel */
void  DeblendDynamicSubsampled(BYTE  Data[],  const  BYTE  LogoData[],  const  BYTE  AlphaData[],  int  Pitch,  int  Stride,  int  LogoPitch,  int  LogoStride,  int  AlphaPitch,  int  AlphaStride,  int  Width,  int  Height,  int  PixelWidth,  int  PixelHeight);  /* Deblends a subsampled color channel */
void  DeblendStatic(BYTE  Data[],  const  DoubleByte  LogoData[],  const  BYTE  AlphaData[],  int  Pitch,  int  Stride,  int  Width,  int  Height);  /* Deblends a color channel with pre-computed deblend data */
int  DeblendStaticPrepare(DoubleByte *  LogoDataChannels[],  BYTE *  AlphaDataChannels[],  AVS_VideoFrame *  LogoFrame,  AVS_VideoFrame *  AlphaFrame,  const  AVS_VideoInfo *  LogoVideoInfo,  const  AVS_VideoInfo *  AlphaVideoInfo);  /* Precomputes deblend-data from a logo and logo-mask */
/* Analyze tools */
signed long  Analyze(AVS_VideoFrame *  DestFrame,  AVS_VideoInfo * DestVideoInfo,  AVS_Clip *  SrcClip,  unsigned char  Mask[],  long  MaskSize,  int  ComputeAlpha,  double  DeviationWeight,  double  SubsamplingWeight);  /* Detects a logo's color and alpha channels from a given mask */
int  SortCompareSum1(const void *  Value1,  const void *  Value2);
int  SortCompareSum2(const void *  Value1,  const void *  Value2);



/*****  Function implementation  *****/


AVSC_EXPORT  const char *  AVSC_CC  avisynth_c_plugin_init(AVS_ScriptEnvironment *  Env)
{
  avs_add_function(Env,"InpaintLogo","c[Mask]c[Radius]f[Sharpness]f[PreBlur]f[PostBlur]f[ChromaWeight]f[PreBlurSize]f[PostBlurSize]f[ChromaTensor]b[PixelAspect]f[Steps]i",Inpaint_Create,0);
  avs_add_function(Env,"DeblendLogo","cc[Alpha]c",Deblend_Create,0);
  avs_add_function(Env,"AnalyzeLogo","c[Mask]c[ComputeAlpha]b[DeviationWeight]f[SubsamplingWeight]f",Analyze_Create,0);
  avs_add_function(Env,"DistanceFunction","c[Scale]f[PixelAspect]f",DistanceFunction_Create,0);
  return  "Logo Inpainting Filter";
}



/****************************************************************
                     Inpaint callbacks
 ****************************************************************/
AVS_Value  AVSC_CC  Inpaint_Create(AVS_ScriptEnvironment *  Env,  AVS_Value  Args,  void *  Data)
{
  #define  Max(X,Y)  (((X)>(Y))?(X):(Y))

  AVS_Value  DestClipValue;
  AVS_Clip * MaskClip, * DestClip;
  AVS_FilterInfo * FilterInfo;
  AVS_VideoFrame * Frame;
  const AVS_VideoInfo * VideoInfo, * MaskVideoInfo;
  int  Steps, ChromaTensor, k;
  double  PAR, Radius, Sharpness, PreBlur, PostBlur, ChromaWeight, PreBlurSize, PostBlurSize;
  int  Stride[MaxChannelCount], ChromaPixelWidth[MaxChannelCount], ChromaPixelHeight[MaxChannelCount];
  char * ErrorText;
  InpaintDataStruct * InpaintData;

  /* Create new filter */
  DestClip = avs_new_c_filter(Env,&FilterInfo,avs_array_elt(Args,0),1);
  if (!DestClip)  return  avs_new_value_error("InpaintLogo: Could not create filter");
  VideoInfo = &FilterInfo->vi;
  /* If something fails here, we store a note in ErrorText. This will be checked later, and memory will be cleaned up before leaving */
  ErrorText = 0;
  InpaintData = 0;
  /* Load mask clip */
  MaskClip = avs_is_clip(avs_array_elt(Args,1))?avs_take_clip(avs_array_elt(Args,1),Env):0;
  MaskVideoInfo = MaskClip?avs_get_video_info(MaskClip):0;
  if (MaskClip && !MaskVideoInfo)  ErrorText = "InpaintLogo: Could not load mask clip";
  /* Check all video properties */
  if (!(ErrorText || avs_is_yuy2(VideoInfo) || avs_is_yv12(VideoInfo) || avs_is_rgb32(VideoInfo) || avs_is_rgb24(VideoInfo)))  ErrorText = "InpaintLogo: Color space unknown (RGB24, RGB32, YUY2, YV12 allowed only)";
  if (!(ErrorText || (avs_has_video(VideoInfo) && VideoInfo->num_frames && VideoInfo->width && VideoInfo->height)))  ErrorText = "InpaintLogo: Source clip has no video";
  if (!ErrorText && MaskClip && !(avs_is_yuy2(MaskVideoInfo) || avs_is_yv12(MaskVideoInfo) || avs_is_rgb32(MaskVideoInfo)))  ErrorText = "InpaintLogo: Mask color space must have luma channel (YUY2 or YV12) or alpha channel (RGB32)";
  if (!ErrorText && MaskClip && !(avs_has_video(MaskVideoInfo) && MaskVideoInfo->num_frames && MaskVideoInfo->width && MaskVideoInfo->height))  ErrorText = "InpaintLogo: Mask clip has no video";
  if (!ErrorText && MaskClip && !((VideoInfo->width==MaskVideoInfo->width) && (VideoInfo->height==MaskVideoInfo->height)))  ErrorText = "InpaintLogo: Source and mask clip have different dimensions";
  if (!(ErrorText || MaskClip || avs_is_rgb32(VideoInfo)))  ErrorText = "InpaintLogo: Mask is missing (needed if source clip is not RGB32)";
  if (GetFramePointers(0,0,0,0,0,0,0,VideoInfo->pixel_type)-(MaskClip?0:1)<=0)  ErrorText = "InpaintLogo internal error: Color space unrecognized";

  /* Extract values from AVS_Value container, throw errors if necessary */
  Radius = avs_is_float(avs_array_elt(Args,2))?avs_as_float(avs_array_elt(Args,2)):5.0;
  if (Radius<=0.0)  ErrorText = "InpaintLogo: Radius must be positive";
  Sharpness = avs_is_float(avs_array_elt(Args,3))?avs_as_float(avs_array_elt(Args,3)):30.0;
  if (Sharpness<0.0)  ErrorText = "InpaintLogo: Sharpness parameter may not be negative";
  PreBlur = avs_is_float(avs_array_elt(Args,4))?avs_as_float(avs_array_elt(Args,4)):0.5;
  if (PreBlur<0.0)  ErrorText = "InpaintLogo: Pre-blurring scale may not be negative";
  PostBlur = avs_is_float(avs_array_elt(Args,5))?avs_as_float(avs_array_elt(Args,5)):4.0;
  if (PostBlur<0.0)  ErrorText = "InpaintLogo: Post-blurring scale may not be negative";
  ChromaWeight = avs_is_float(avs_array_elt(Args,6))?avs_as_float(avs_array_elt(Args,6)):0.0;
  if (GetFramePointers(0,0,0,0,0,0,0,VideoInfo->pixel_type)-1-(MaskClip?0:1)==0)  ChromaWeight = 0.0;  else  if (avs_is_rgb(VideoInfo))  ChromaWeight = 1.0-1.0/(GetFramePointers(0,0,0,0,0,0,0,VideoInfo->pixel_type)-1-(MaskClip?0:1));
  if ((ChromaWeight<0.0) || (ChromaWeight>1.0))  ErrorText = "InpaintLogo: Chroma structure tensor weight must lie in the interval [0.0,1.0]";
  #if (!(defined(BlurStandardDeviation) && ((BlurStandardDeviation)!=0)))
  PreBlur = sqrt(2*PreBlur);
  PostBlur = sqrt(2*PostBlur);
  #endif
  PreBlurSize = 2*PreBlur;
  PostBlurSize = 2*PostBlur;
  if (avs_is_float(avs_array_elt(Args,7)))  PreBlurSize = avs_as_float(avs_array_elt(Args,7));
  if (PreBlur==0.0)  PreBlurSize = 0;
  if (PreBlurSize<0.0)  ErrorText = "InpaintLogo: Pre-blurring kernel size may not be negative";
  if (avs_is_float(avs_array_elt(Args,8)))  PostBlurSize = avs_as_float(avs_array_elt(Args,8));
  if (PostBlur==0.0)  PostBlurSize = 0;
  if (PostBlurSize<0.0)  ErrorText = "InpaintLogo: Post-blurring kernel size may not be negative";
  ChromaTensor = avs_is_bool(avs_array_elt(Args,9))?avs_as_bool(avs_array_elt(Args,9)):0;
  PAR = avs_is_float(avs_array_elt(Args,10))?avs_as_float(avs_array_elt(Args,10)):1.0;
  if (PAR<=0.0)  ErrorText = "InpaintLogo: Pixel aspect ratio must be positive";
  Steps = avs_is_int(avs_array_elt(Args,11))?avs_as_int(avs_array_elt(Args,11)):(-1);
  if (Steps<(-1))  ErrorText = "InpaintLogo: Steps count may not be lower than -1" "\n\n" LegalInfoString;
  
  /* Prepare data structure */
  if (!MaskClip)  MaskClip = avs_take_clip(avs_array_elt(Args,0),Env);
  MaskVideoInfo = MaskClip?avs_get_video_info(MaskClip):0;
  if (!MaskVideoInfo)  ErrorText = "InpaintLogo internal error: Opening mask clip failed";
  if (!ErrorText)
  {
    /* Allocate data struct */
    InpaintData = malloc(sizeof(InpaintDataStruct));
    if (!InpaintData)  ErrorText = "InpaintLogo: Could not allocate memory";
  }
  if (InpaintData)
  {
    /** Color space parameters **/
    InpaintData->ChromaChannelCount = GetFramePointers(0,0,0,0,Stride,ChromaPixelWidth,ChromaPixelHeight,VideoInfo->pixel_type)-1-(MaskClip?0:1);
    InpaintData->PixelType = VideoInfo->pixel_type;
    InpaintData->Width = VideoInfo->width;
    InpaintData->Height = VideoInfo->height;
    InpaintData->LumaStride = Stride[0];
    /* If there is no chroma channel (Y8?) all chroma values are set to zero */
    if (InpaintData->ChromaChannelCount)
    {
      InpaintData->ChromaPixelWidth = ChromaPixelWidth[1];
      InpaintData->ChromaPixelHeight = ChromaPixelHeight[1];
      InpaintData->ChromaStride = Stride[1];
    }
    else
    {
      InpaintData->ChromaPixelWidth = 0;
      InpaintData->ChromaPixelHeight = 0;
      InpaintData->ChromaStride = 0;
    }
    /* Parameters of mask clip: Only if mask is dynamic */
    if (!ErrorText && (MaskVideoInfo->num_frames>1))
    {
      InpaintData->MaskClip = MaskClip;
      InpaintData->MaskFrameCount = MaskVideoInfo->num_frames;
    }
    else
    {
      InpaintData->MaskClip = 0;
      InpaintData->MaskFrameCount = 0;
    }
    GetFramePointers(0,0,0,0,Stride,0,0,MaskVideoInfo->pixel_type);
    InpaintData->MaskChannelNo = avs_is_yuv(MaskVideoInfo)?0:3;
    InpaintData->FlipMask = (avs_is_rgb(VideoInfo)!=avs_is_rgb(MaskVideoInfo));
    InpaintData->MaskStride = Stride[InpaintData->MaskChannelNo];
    InpaintData->MaskPixelType = MaskVideoInfo->pixel_type;
    InpaintData->MaxSteps = (Steps>=0)?Steps:-1;
    /** user-specified values **/
    InpaintData->PixelWidth = sqrt(PAR);
    InpaintData->PixelHeight = 1.0/InpaintData->PixelWidth;
    /* If no chroma channels, corresponding values are set to zero */
    if (InpaintData->ChromaChannelCount)
    {
      InpaintData->LumaWeight = 1.0-ChromaWeight;
      InpaintData->ChromaWeight = ChromaWeight/InpaintData->ChromaChannelCount;
      InpaintData->ChromaRadiusX = Max(Radius/InpaintData->PixelWidth/InpaintData->ChromaPixelWidth,InpaintMinRadiusXY);
      InpaintData->ChromaRadiusY = Max(Radius/InpaintData->PixelHeight/InpaintData->ChromaPixelHeight,InpaintMinRadiusXY);
    }
    else
    {
      InpaintData->LumaWeight = 1.0;
      InpaintData->ChromaWeight = 0.0;
      InpaintData->ChromaRadiusX = 0.0;
      InpaintData->ChromaRadiusY = 0.0;
    }
    InpaintData->LumaRadiusX = Max(Radius/InpaintData->PixelWidth,InpaintMinRadiusXY);
    InpaintData->LumaRadiusY = Max(Radius/InpaintData->PixelHeight,InpaintMinRadiusXY);
    InpaintData->Sharpness = Sharpness;
    InpaintData->ChromaTensor = ChromaTensor && InpaintData->ChromaChannelCount && avs_is_yuv(VideoInfo);
    /** Initialize all blur kernel stuff **/
    InpaintData->LumaPreBlurXKernel = InpaintData->LumaPreBlurYKernel = InpaintData->ChromaPreBlurXKernel = InpaintData->ChromaPreBlurYKernel = 0;
    InpaintData->Luma4LumaPostBlurXKernel = InpaintData->Luma4LumaPostBlurYKernel = 0;
    InpaintData->Chroma4ChromaPostBlurXKernel = InpaintData->Chroma4ChromaPostBlurYKernel = 0;
    InpaintData->Luma4ChromaPostBlurXKernel = InpaintData->Luma4ChromaPostBlurYKernel = 0;
    InpaintData->Chroma4LumaPostBlurXKernels = InpaintData->Chroma4LumaPostBlurYKernels = 0;
    InpaintData->LumaPreBlurXKernelVector = InpaintData->LumaPreBlurYKernelVector = InpaintData->ChromaPreBlurXKernelVector = InpaintData->ChromaPreBlurYKernelVector = 0;
    InpaintData->Luma4LumaPostBlurXKernelVector = InpaintData->Luma4LumaPostBlurYKernelVector = 0;
    InpaintData->Chroma4ChromaPostBlurXKernelVector = InpaintData->Chroma4ChromaPostBlurYKernelVector = 0;
    InpaintData->Luma4ChromaPostBlurXKernelVector = InpaintData->Luma4ChromaPostBlurYKernelVector = 0;
    InpaintData->Chroma4LumaPostBlurXKernelVectors = InpaintData->Chroma4LumaPostBlurYKernelVectors = 0;
    InpaintData->LumaPreBlurXKernelSize = InpaintData->LumaPreBlurYKernelSize = InpaintData->ChromaPreBlurXKernelSize = InpaintData->ChromaPreBlurYKernelSize = 0;
    InpaintData->Luma4LumaPostBlurXKernelSize = InpaintData->Luma4LumaPostBlurYKernelSize = 0;
    InpaintData->Chroma4ChromaPostBlurXKernelSize = InpaintData->Chroma4ChromaPostBlurYKernelSize = 0;
    InpaintData->Luma4ChromaPostBlurXKernelSize = InpaintData->Luma4ChromaPostBlurYKernelSize = 0;
    InpaintData->Chroma4LumaPostBlurXKernelSize = InpaintData->Chroma4LumaPostBlurYKernelSize = 0;
    /** Set blurring kernel sizes and allocate a lot of memory **/
    if (InpaintData->LumaWeight!=0.0)
    {
      InpaintData->LumaPreBlurXKernelSize = Max(floor(PreBlurSize/InpaintData->PixelWidth),InpaintMinPreBlurXYKernelSize);
      InpaintData->LumaPreBlurYKernelSize = Max(floor(PreBlurSize/InpaintData->PixelHeight),InpaintMinPreBlurXYKernelSize);
      InpaintData->Luma4LumaPostBlurXKernelSize = Max(floor(PostBlurSize/InpaintData->PixelWidth+0.5),InpaintMinPostBlurXYKernelSize);
      InpaintData->Luma4LumaPostBlurYKernelSize = Max(floor(PostBlurSize/InpaintData->PixelHeight+0.5),InpaintMinPostBlurXYKernelSize);
      InpaintData->LumaPreBlurXKernelVector = (double*)malloc(sizeof(double)*(2*InpaintData->LumaPreBlurXKernelSize+1));
      InpaintData->LumaPreBlurYKernelVector = (double*)malloc(sizeof(double)*(2*InpaintData->LumaPreBlurYKernelSize+1));
      InpaintData->Luma4LumaPostBlurXKernelVector = (double*)malloc(sizeof(double)*(2*InpaintData->Luma4LumaPostBlurXKernelSize+1));
      InpaintData->Luma4LumaPostBlurYKernelVector = (double*)malloc(sizeof(double)*(2*InpaintData->Luma4LumaPostBlurYKernelSize+1));
      if (!(InpaintData->LumaPreBlurXKernelVector && InpaintData->LumaPreBlurYKernelVector && InpaintData->Luma4LumaPostBlurXKernelVector && InpaintData->Luma4LumaPostBlurYKernelVector))  ErrorText = "InpaintLogo: Could not allocate memory";
    }
    if (InpaintData->ChromaChannelCount || (InpaintData->ChromaWeight!=0.0))
    {
      InpaintData->ChromaPreBlurXKernelSize = Max(floor(PreBlurSize/InpaintData->PixelWidth/InpaintData->ChromaPixelWidth),InpaintMinPreBlurXYKernelSize);
      InpaintData->ChromaPreBlurYKernelSize = Max(floor(PreBlurSize/InpaintData->PixelHeight/InpaintData->ChromaPixelHeight),InpaintMinPreBlurXYKernelSize);
      InpaintData->ChromaPreBlurXKernelVector = (double*)malloc(sizeof(double)*(2*InpaintData->ChromaPreBlurXKernelSize+1));
      InpaintData->ChromaPreBlurYKernelVector = (double*)malloc(sizeof(double)*(2*InpaintData->ChromaPreBlurYKernelSize+1));
      if (!(InpaintData->ChromaPreBlurXKernelVector && InpaintData->ChromaPreBlurYKernelVector))  ErrorText = "InpaintLogo: Could not allocate memory";
    }
    if (InpaintData->ChromaTensor || (InpaintData->ChromaWeight!=0.0))
    {
      InpaintData->Chroma4ChromaPostBlurXKernelSize = Max(floor(PostBlurSize/InpaintData->PixelWidth/InpaintData->ChromaPixelWidth+0.5),InpaintMinPostBlurXYKernelSize);
      InpaintData->Chroma4ChromaPostBlurYKernelSize = Max(floor(PostBlurSize/InpaintData->PixelHeight/InpaintData->ChromaPixelHeight+0.5),InpaintMinPostBlurXYKernelSize);
      InpaintData->Chroma4ChromaPostBlurXKernelVector = (double*)malloc(sizeof(double)*(2*InpaintData->Chroma4ChromaPostBlurXKernelSize+1));
      InpaintData->Chroma4ChromaPostBlurYKernelVector = (double*)malloc(sizeof(double)*(2*InpaintData->Chroma4ChromaPostBlurYKernelSize+1));
      if (!(InpaintData->Chroma4ChromaPostBlurXKernelVector && InpaintData->Chroma4ChromaPostBlurYKernelVector))  ErrorText = "InpaintLogo: Could not allocate memory";
    }
    if (InpaintData->ChromaTensor && (InpaintData->LumaWeight!=0.0))
    {
      InpaintData->Luma4ChromaPostBlurXKernelSize = Max(floor(PostBlurSize/InpaintData->PixelWidth+((InpaintData->ChromaPixelWidth%2)?0.0:0.5)),InpaintMinPostBlurXYKernelSize);
      InpaintData->Luma4ChromaPostBlurYKernelSize = Max(floor(PostBlurSize/InpaintData->PixelHeight+((InpaintData->ChromaPixelHeight%2)?0.0:0.5)),InpaintMinPostBlurXYKernelSize);
      InpaintData->Luma4ChromaPostBlurXKernelVector = (double*)malloc(sizeof(double)*(2*InpaintData->Luma4ChromaPostBlurXKernelSize+1));
      InpaintData->Luma4ChromaPostBlurYKernelVector = (double*)malloc(sizeof(double)*(2*InpaintData->Luma4ChromaPostBlurYKernelSize+1));
      if (!(InpaintData->Luma4ChromaPostBlurXKernelVector && InpaintData->Luma4ChromaPostBlurYKernelVector))  ErrorText = "InpaintLogo: Could not allocate memory";
    }
    if (InpaintData->ChromaWeight!=0.0)
    {
      InpaintData->Chroma4LumaPostBlurXKernelSize = Max(floor(PostBlurSize/InpaintData->PixelWidth/InpaintData->ChromaPixelWidth+0.5-(InpaintData->ChromaPixelWidth%2?0.0:0.5/InpaintData->ChromaPixelWidth)),InpaintMinPostBlurXYKernelSize);
      InpaintData->Chroma4LumaPostBlurYKernelSize = Max(floor(PostBlurSize/InpaintData->PixelHeight/InpaintData->ChromaPixelHeight+0.5-(InpaintData->ChromaPixelHeight%2?0.0:0.5/InpaintData->ChromaPixelHeight)),InpaintMinPostBlurXYKernelSize);
      InpaintData->Chroma4LumaPostBlurXKernelVectors = (double**)malloc(sizeof(double*)*(InpaintData->ChromaPixelWidth));
      InpaintData->Chroma4LumaPostBlurYKernelVectors = (double**)malloc(sizeof(double*)*(InpaintData->ChromaPixelHeight));
      InpaintData->Chroma4LumaPostBlurXKernels = (double**)malloc(sizeof(double*)*(InpaintData->ChromaPixelWidth));
      InpaintData->Chroma4LumaPostBlurYKernels = (double**)malloc(sizeof(double*)*(InpaintData->ChromaPixelHeight));
      if (!(InpaintData->Chroma4LumaPostBlurXKernelVectors && InpaintData->Chroma4LumaPostBlurYKernelVectors && InpaintData->Chroma4LumaPostBlurXKernels && InpaintData->Chroma4LumaPostBlurYKernels))
      {
        ErrorText = "InpaintLogo: Could not allocate memory";
      }
      else
      {
        for (k=0 ; k<InpaintData->ChromaPixelWidth ; k++)
        {
          InpaintData->Chroma4LumaPostBlurXKernelVectors[k] = (double*)malloc(sizeof(double)*(2*InpaintData->Chroma4LumaPostBlurXKernelSize+1));
          if (!InpaintData->Chroma4LumaPostBlurXKernelVectors[k])  ErrorText = "InpaintLogo: Could not allocate memory";
          InpaintData->Chroma4LumaPostBlurXKernels[k] = 0;
        }
        for (k=0 ; k<InpaintData->ChromaPixelHeight ; k++)
        {
          InpaintData->Chroma4LumaPostBlurYKernelVectors[k] = (double*)malloc(sizeof(double)*(2*InpaintData->Chroma4LumaPostBlurYKernelSize+1));
          if (!InpaintData->Chroma4LumaPostBlurYKernelVectors[k])  ErrorText = "InpaintLogo: Could not allocate memory";
          InpaintData->Chroma4LumaPostBlurYKernels[k] = 0;
        }
      }
    }
    InpaintData->StaticMaskData = 0;
    if (InpaintData->ChromaPixelWidth*InpaintData->ChromaPixelHeight>255)  ErrorText = "InpaintLogo internal error: Chroma subsampling to strong";
  }
  if (!ErrorText)
  {
    /* Compute and store blur kernels */
    if (InpaintData->LumaPreBlurXKernelVector)  InpaintData->LumaPreBlurXKernel = CreateGaussKernel(InpaintData->LumaPreBlurXKernelVector,InpaintData->LumaPreBlurXKernelSize,Max(PreBlur/InpaintData->PixelWidth,InpaintMinPreBlurXYStandardDeviation),0.0);
    if (InpaintData->LumaPreBlurYKernelVector)  InpaintData->LumaPreBlurYKernel = CreateGaussKernel(InpaintData->LumaPreBlurYKernelVector,InpaintData->LumaPreBlurYKernelSize,Max(PreBlur/InpaintData->PixelHeight,InpaintMinPreBlurXYStandardDeviation),0.0);
    /* Most post blurring kernels are build for forward differences */
    if (InpaintData->Luma4LumaPostBlurXKernelVector)  InpaintData->Luma4LumaPostBlurXKernel = CreateGaussKernel(InpaintData->Luma4LumaPostBlurXKernelVector,InpaintData->Luma4LumaPostBlurXKernelSize,Max(PostBlur/InpaintData->PixelWidth,InpaintMinPostBlurXYStandardDeviation),-0.5);
    if (InpaintData->Luma4LumaPostBlurYKernelVector)  InpaintData->Luma4LumaPostBlurYKernel = CreateGaussKernel(InpaintData->Luma4LumaPostBlurYKernelVector,InpaintData->Luma4LumaPostBlurYKernelSize,Max(PostBlur/InpaintData->PixelHeight,InpaintMinPostBlurXYStandardDeviation),-0.5);
    if (InpaintData->ChromaPreBlurXKernelVector)  InpaintData->ChromaPreBlurXKernel = CreateGaussKernel(InpaintData->ChromaPreBlurXKernelVector,InpaintData->ChromaPreBlurXKernelSize,Max(PreBlur/InpaintData->PixelWidth/InpaintData->ChromaPixelWidth,InpaintMinPreBlurXYStandardDeviation),0.0);
    if (InpaintData->ChromaPreBlurYKernelVector)  InpaintData->ChromaPreBlurYKernel = CreateGaussKernel(InpaintData->ChromaPreBlurYKernelVector,InpaintData->ChromaPreBlurYKernelSize,Max(PreBlur/InpaintData->PixelHeight/InpaintData->ChromaPixelHeight,InpaintMinPreBlurXYStandardDeviation),0.0);
    if (InpaintData->Chroma4ChromaPostBlurXKernelVector)  InpaintData->Chroma4ChromaPostBlurXKernel = CreateGaussKernel(InpaintData->Chroma4ChromaPostBlurXKernelVector,InpaintData->Chroma4ChromaPostBlurXKernelSize,Max(PostBlur/InpaintData->PixelWidth/InpaintData->ChromaPixelWidth,InpaintMinPostBlurXYStandardDeviation),-0.5);
    if (InpaintData->Chroma4ChromaPostBlurYKernelVector)  InpaintData->Chroma4ChromaPostBlurYKernel = CreateGaussKernel(InpaintData->Chroma4ChromaPostBlurYKernelVector,InpaintData->Chroma4ChromaPostBlurYKernelSize,Max(PostBlur/InpaintData->PixelHeight/InpaintData->ChromaPixelHeight,InpaintMinPostBlurXYStandardDeviation),-0.5);
    /* Luma4Chroma kernel is centered in center pixel for odd ChPxSize, or in pixel left of center line for even ChPxSize */
    if (InpaintData->Luma4ChromaPostBlurXKernelVector)  InpaintData->Luma4ChromaPostBlurXKernel = CreateGaussKernel(InpaintData->Luma4ChromaPostBlurXKernelVector,InpaintData->Luma4ChromaPostBlurXKernelSize,Max(PostBlur/InpaintData->PixelWidth,InpaintMinPostBlurXYStandardDeviation),(InpaintData->ChromaPixelWidth%2)?-0.5:0.0);
    if (InpaintData->Luma4ChromaPostBlurYKernelVector)  InpaintData->Luma4ChromaPostBlurYKernel = CreateGaussKernel(InpaintData->Luma4ChromaPostBlurYKernelVector,InpaintData->Luma4ChromaPostBlurYKernelSize,Max(PostBlur/InpaintData->PixelHeight,InpaintMinPostBlurXYStandardDeviation),(InpaintData->ChromaPixelHeight%2)?-0.5:0.0);
    /* Chroma4Luma Kernels: Let k=0..ChPxSize-1 be number of luma pixel in chroma block. Kernel will be centered in luma pixel k, if: backward differences for 2k+1<ChPxSize, forward else */
    if (InpaintData->Chroma4LumaPostBlurXKernelVectors)  for (k=0 ; k<InpaintData->ChromaPixelWidth ; k++)  InpaintData->Chroma4LumaPostBlurXKernels[k] = CreateGaussKernel(InpaintData->Chroma4LumaPostBlurXKernelVectors[k],InpaintData->Chroma4LumaPostBlurXKernelSize,Max(PostBlur/InpaintData->PixelWidth/InpaintData->ChromaPixelWidth,InpaintMinPostBlurXYStandardDeviation),(2.0*k+1.0)/2.0/InpaintData->ChromaPixelWidth-(2*k+1<InpaintData->ChromaPixelWidth?0.0:1.0));
    if (InpaintData->Chroma4LumaPostBlurYKernelVectors)  for (k=0 ; k<InpaintData->ChromaPixelHeight ; k++)  InpaintData->Chroma4LumaPostBlurYKernels[k] = CreateGaussKernel(InpaintData->Chroma4LumaPostBlurYKernelVectors[k],InpaintData->Chroma4LumaPostBlurYKernelSize,Max(PostBlur/InpaintData->PixelHeight/InpaintData->ChromaPixelHeight,InpaintMinPostBlurXYStandardDeviation),(2.0*k+1.0)/2.0/InpaintData->ChromaPixelHeight-(2*k+1<InpaintData->ChromaPixelHeight?0.0:1.0));
  }
  if (!ErrorText && !InpaintData->MaskClip)
  {
    Frame = avs_get_frame(MaskClip,0);
    if (Frame)
    {
      switch (InpaintPrepareMask(&InpaintData->StaticMaskData,Frame,InpaintData))
      {
        case InpaintMask_Result_FullMask :
        /* If error-switch is set, a full mask throws an error, otherwise this filter reduces to the do-nothing-filter */
        #if ((InpaintFullMaskError)>=1)
          ErrorText = "InpaintLogo: Mask is full";
        break;
        #endif
        case InpaintMask_Result_NoMask :
          /* To indicate no inpainting is needed anymore, we clear the StaticMaskData field */
          /* InpaintPrepareMask(&InpaintData->StaticMaskData,0,InpaintData);  ** Not necessary */
        break;
        case InpaintMask_Result_GeneralError :
          ErrorText = "InpaintLogo: Mask preparation failed: Internal error";
        break;
        case InpaintMask_Result_FrameError :
          ErrorText = "InpaintLogo: Mask preparation failed: Could not process mask frame";
        break;
        case InpaintMask_Result_MemoryError :
          ErrorText = "InpaintLogo: Mask preparation failed: Out of memory";
        break;
        default :
          ErrorText = 0;
        break;
      }
      avs_release_video_frame(Frame);
    }
    else
    {
      ErrorText = "InpaintLogo: Could not get mask frame";
    }
  }
  if (MaskClip && (ErrorText || !InpaintData->MaskClip))  avs_release_clip(MaskClip);
  DestClipValue = avs_void;
  FilterInfo->get_frame = Inpaint_GetFrame;
  FilterInfo->free_filter = Inpaint_Free;
  FilterInfo->user_data = InpaintData;
  if (!ErrorText)
  {
    /* If no mask is present (or no inpaint steps are allowed), we return the source clip unchanged */
    if ((InpaintData->MaskClip || InpaintData->StaticMaskData) && Steps)
    {
      DestClipValue = avs_new_value_clip(DestClip);
    }
    else
    {
      avs_copy_value(&DestClipValue,avs_array_elt(Args,0));
      Inpaint_Free(FilterInfo);
    }
    if (!avs_defined(DestClipValue))  ErrorText = "InpaintLogo: Could not create target clip";
  }
  avs_release_clip(DestClip);
  if (!ErrorText)  return  DestClipValue;
  /* Error occured!! We clean up everything */
  Inpaint_Free(FilterInfo);
  return  avs_new_value_error(ErrorText);


  #undef  Max
}



AVS_VideoFrame *  AVSC_CC  Inpaint_GetFrame(AVS_FilterInfo *  FilterInfo,  int  FrameNo)
{
  InpaintDataStruct * InpaintData;
  InpaintMaskDataStruct * MaskData;
  AVS_VideoFrame * Frame;
  BYTE * FrameData[MaxChannelCount], * LumaData, * ChromaDatas[MaxChannelCount-1], * ChromaData;
  int  FramePitch[MaxChannelCount], LumaPitch, * ChromaPitch;
  int  k, x, y, PixelNo, ChannelNo;
  double  CoherenceStrength, CoherenceDirectionX, CoherenceDirectionY, TensorXX, TensorXY, TensorYY, Value;
  char * ErrorText;
  /* Values given by the user */
  double  PixelWidth2, PixelHeight2;
  double  LumaWeight, ChromaWeight;
  double  LumaRadiusX, LumaRadiusY, ChromaRadiusX, ChromaRadiusY, Sharpness;
  int  ChromaTensor;
  /* Color space dependent values */
  int  ChromaPixelWidth, ChromaPixelHeight, LumaStride, ChromaStride, ChromaChannelCount;
  /* Blurring stuff */
  double * LumaPreBlurXKernel, * LumaPreBlurYKernel, * ChromaPreBlurXKernel, * ChromaPreBlurYKernel;  /* (Preblurred data will be buffered later) */
  double * Luma4LumaPostBlurXKernel, * Luma4LumaPostBlurYKernel;  /* Postblurring will also convert between different samplings */
  double * Chroma4ChromaPostBlurXKernel, * Chroma4ChromaPostBlurYKernel;
  double * Luma4ChromaPostBlurXKernel, * Luma4ChromaPostBlurYKernel;
  double ** Chroma4LumaPostBlurXKernels, ** Chroma4LumaPostBlurYKernels;
  int  LumaPreBlurXKernelSize, LumaPreBlurYKernelSize, ChromaPreBlurXKernelSize, ChromaPreBlurYKernelSize;
  int  Luma4LumaPostBlurXKernelSize, Luma4LumaPostBlurYKernelSize;
  int  Chroma4ChromaPostBlurXKernelSize, Chroma4ChromaPostBlurYKernelSize;
  int  Luma4ChromaPostBlurXKernelSize, Luma4ChromaPostBlurYKernelSize;
  int  Chroma4LumaPostBlurXKernelSize, Chroma4LumaPostBlurYKernelSize;
  /* Given by mask */
  int  Width, Height, MaskPixelCount;
  unsigned int * MaskPixelOrder;
  BYTE * chiLuma, * chiChroma;
  float * GchiLuma, * GchiChroma;
  float * GchiuLuma, * GchiuChromas[MaxChannelCount], * GchiuChroma;

  InpaintData = (InpaintDataStruct*)FilterInfo->user_data;
  /* If the logo is dynamic, we prepare the mask stuff */
  MaskData = InpaintData->StaticMaskData;
  if (!MaskData)
  {
    ErrorText = 0;
    Frame = avs_get_frame(InpaintData->MaskClip,(FrameNo<InpaintData->MaskFrameCount)?FrameNo:(InpaintData->MaskFrameCount-1));
    if (!Frame)  ErrorText = "InpaintLogo: Could not get mask frame";
    if (!ErrorText)
    {
      switch (InpaintPrepareMask(&MaskData,Frame,InpaintData))
      {
        case InpaintMask_Result_FullMask :
        #if ((InpaintFullMaskError)>=2)
          ErrorText = "InpaintLogo: Mask is full";
        break;
        #endif
        case InpaintMask_Result_NoMask :
          /* (still) no mask data indicates nothing to do */
          /* InpaintPrepareMask(&MaskData,0,InpaintData);  ** Not necessary */
        break;
        case InpaintMask_Result_GeneralError :
          ErrorText = "InpaintLogo: Mask preparation failed: Internal error";
        break;
        case InpaintMask_Result_FrameError :
          ErrorText = "InpaintLogo: Mask preparation failed: Could not process mask frame";
        break;
        case InpaintMask_Result_MemoryError :
          ErrorText = "InpaintLogo: Mask preparation failed: Out of memory";
        break;
        default :
          ErrorText = 0;
        break;
      }
    }
    if (Frame)  avs_release_video_frame(Frame);
    if (ErrorText)
    {
      FilterInfo->error = ErrorText;
      InpaintPrepareMask(&MaskData,0,InpaintData);
      return  0;
    }
  }
  /* Get video frame */
  Frame = avs_get_frame(FilterInfo->child,FrameNo);
  if (!(MaskData&&Frame))  /* No mask data? Then we just return the source frame */
  {
    if (!InpaintData->StaticMaskData)  InpaintPrepareMask(&MaskData,0,InpaintData);
    if (!Frame)  FilterInfo->error = "InpaintLogo: Could not get video frame";
    return  Frame;
  }
  /* Store all important values locally */
  PixelWidth2 = InpaintData->PixelWidth*InpaintData->PixelWidth;
  PixelHeight2 = InpaintData->PixelHeight*InpaintData->PixelHeight;
  LumaWeight = InpaintData->LumaWeight;
  ChromaWeight = InpaintData->ChromaWeight;
  LumaRadiusX = InpaintData->LumaRadiusX;
  LumaRadiusY = InpaintData->LumaRadiusY;
  ChromaRadiusX = InpaintData->ChromaRadiusX;
  ChromaRadiusY = InpaintData->ChromaRadiusY;
  Sharpness = InpaintData->Sharpness;
  ChromaTensor = InpaintData->ChromaTensor;
  ChromaPixelWidth = InpaintData->ChromaPixelWidth;
  ChromaPixelHeight = InpaintData->ChromaPixelHeight;
  LumaStride = InpaintData->LumaStride;
  ChromaStride = InpaintData->ChromaStride;
  ChromaChannelCount = InpaintData->ChromaChannelCount;
  LumaPreBlurXKernel = InpaintData->LumaPreBlurXKernel;
  LumaPreBlurYKernel = InpaintData->LumaPreBlurYKernel;
  ChromaPreBlurXKernel = InpaintData->ChromaPreBlurXKernel;
  ChromaPreBlurYKernel = InpaintData->ChromaPreBlurYKernel;
  Luma4LumaPostBlurXKernel = InpaintData->Luma4LumaPostBlurXKernel;
  Luma4LumaPostBlurYKernel = InpaintData->Luma4LumaPostBlurYKernel;
  Chroma4ChromaPostBlurXKernel = InpaintData->Chroma4ChromaPostBlurXKernel;
  Chroma4ChromaPostBlurYKernel = InpaintData->Chroma4ChromaPostBlurYKernel;
  Luma4ChromaPostBlurXKernel = InpaintData->Luma4ChromaPostBlurXKernel;
  Luma4ChromaPostBlurYKernel = InpaintData->Luma4ChromaPostBlurYKernel;
  Chroma4LumaPostBlurXKernels = InpaintData->Chroma4LumaPostBlurXKernels;
  Chroma4LumaPostBlurYKernels = InpaintData->Chroma4LumaPostBlurYKernels;
  LumaPreBlurXKernelSize = InpaintData->LumaPreBlurXKernelSize;
  LumaPreBlurYKernelSize = InpaintData->LumaPreBlurYKernelSize;
  ChromaPreBlurXKernelSize = InpaintData->ChromaPreBlurXKernelSize;
  ChromaPreBlurYKernelSize = InpaintData->ChromaPreBlurYKernelSize;
  Luma4LumaPostBlurXKernelSize = InpaintData->Luma4LumaPostBlurXKernelSize;
  Luma4LumaPostBlurYKernelSize = InpaintData->Luma4LumaPostBlurYKernelSize;
  Chroma4ChromaPostBlurXKernelSize = InpaintData->Chroma4ChromaPostBlurXKernelSize;
  Chroma4ChromaPostBlurYKernelSize = InpaintData->Chroma4ChromaPostBlurYKernelSize;
  Luma4ChromaPostBlurXKernelSize = InpaintData->Luma4ChromaPostBlurXKernelSize;
  Luma4ChromaPostBlurYKernelSize = InpaintData->Luma4ChromaPostBlurYKernelSize;
  Chroma4LumaPostBlurXKernelSize = InpaintData->Chroma4LumaPostBlurXKernelSize;
  Chroma4LumaPostBlurYKernelSize = InpaintData->Chroma4LumaPostBlurYKernelSize;
  Width = MaskData->Width;
  Height = MaskData->Height;
  MaskPixelCount = MaskData->MaskPixelCount;
  MaskPixelOrder = MaskData->MaskPixelOrder;
  /* As usual, ErrorText stores a text message, if an error occured */
  ErrorText = 0;
  FrameData[0] = 0;
  if (Frame)  avs_make_writable(FilterInfo->env,&Frame);
  if (Frame)  GetFramePointers(Frame,FrameData,0,FramePitch,0,0,0,InpaintData->PixelType);
  if (!FrameData[0])  ErrorText = "InpaintLogo: Could not get pixel data pointers";
  /* Allocate temporary memory */
  chiLuma = 0;
  GchiLuma = 0;
  GchiuLuma = 0;
  chiChroma = 0;
  GchiChroma = 0;
  for (ChannelNo=0 ; ChannelNo<MaxChannelCount-1 ; ChannelNo++)  GchiuChromas[ChannelNo] = 0;
  chiLuma = (InpaintData->StaticMaskData)?(BYTE*)malloc(sizeof(BYTE)*Width*Height):MaskData->chiLuma;
  if (!chiLuma)  ErrorText = "InpaintLogo: Could not allocate memory";
  if (LumaWeight!=0.0)
  {
    GchiLuma = (InpaintData->StaticMaskData)?(float*)malloc(sizeof(float)*Width*Height):MaskData->GchiLuma;
    GchiuLuma = (float*)malloc(sizeof(float)*Width*Height);
    if (!(GchiLuma && GchiuLuma))  ErrorText = "InpaintLogo: Could not allocate memory";
  }
  chiChroma = (InpaintData->StaticMaskData)?(BYTE*)malloc(sizeof(BYTE)*Width*Height):MaskData->chiChroma;
  if (!chiChroma)  ErrorText = "InpaintLogo: Could not allocate memory";
  if (ChromaWeight!=0.0)
  {
    GchiChroma = (InpaintData->StaticMaskData)?(float*)malloc(sizeof(float)*(Width/ChromaPixelWidth)*(Height/ChromaPixelHeight)):MaskData->GchiChroma;
    if (!GchiChroma)  ErrorText = "InpaintLogo: Could not allocate memory";
    for (ChannelNo=0 ; ChannelNo<ChromaChannelCount ; ChannelNo++)
    {
      GchiuChromas[ChannelNo] = (float*)malloc(sizeof(float)*(Width/ChromaPixelWidth)*(Height/ChromaPixelHeight));
      if (!GchiuChromas[ChannelNo])  ErrorText = "InpaintLogo: Could not allocate memory";
    }
  }
  /* Error? Then run away */
  if (ErrorText)
  {
    for (ChannelNo=0 ; ChannelNo<MaxChannelCount-1 ; ChannelNo++)  if (GchiuChromas[ChannelNo])  free(GchiuChromas[ChannelNo]);
    if (GchiuLuma)  free(GchiuLuma);
    if (InpaintData->StaticMaskData)
    {
      if (GchiChroma)  free(GchiChroma);
      if (chiChroma)  free(chiChroma);
      if (GchiLuma)  free(GchiLuma);
      if (chiLuma)  free(chiLuma);
    }
    else
    {
      InpaintPrepareMask(&MaskData,0,InpaintData);
    }
    if (Frame)  avs_release_video_frame(Frame);  else  ErrorText = "InpaintLogo: Could not get video Frame";
    FilterInfo->error = ErrorText;
    return  0;
  }
  /* Prepare pointers */
  LumaPitch = FramePitch[0];
  ChromaPitch = FramePitch+1;
  LumaData = FrameData[0]+LumaPitch*MaskData->OffsetY+MaskData->OffsetX*InpaintData->LumaStride;
  for (ChannelNo=0 ; ChannelNo<ChromaChannelCount ; ChannelNo++)  ChromaDatas[ChannelNo] = FrameData[ChannelNo+1]+MaskData->OffsetY/ChromaPixelHeight*FramePitch[ChannelNo+1]+MaskData->OffsetX/ChromaPixelWidth*ChromaStride;
  /* Prepare blurred images */
  if (InpaintData->StaticMaskData)
  {
    memcpy(chiLuma,MaskData->chiLuma,sizeof(BYTE)*Width*Height);
    memcpy(chiChroma,MaskData->chiChroma,sizeof(BYTE)*(Width/ChromaPixelWidth)*(Height/ChromaPixelHeight));
    if (LumaWeight!=0.0)  memcpy(GchiLuma,MaskData->GchiLuma,sizeof(float)*Width*Height); 
    if (ChromaWeight!=0.0)  memcpy(GchiChroma,MaskData->GchiChroma,sizeof(float)*(Width/ChromaPixelWidth)*(Height/ChromaPixelHeight));
  }
  if (LumaWeight!=0.0)
  {
    for (k=0 ; k<Width*Height ; k++)  GchiuLuma[k] = 0.0;
    for (k=0 ; k<Width*Height ; k++)  if (!chiLuma[k])  AddPreBlurValue(GchiuLuma,k%Width,k/Width,Width,Height,1.0*LumaData[LumaPitch*(k/Width)+(k%Width)*LumaStride],LumaPreBlurXKernel,LumaPreBlurXKernelSize,LumaPreBlurYKernel,LumaPreBlurYKernelSize);
  }
  if (ChromaWeight!=0.0)
  {
    for (ChannelNo=0 ; ChannelNo<ChromaChannelCount ; ChannelNo++)
    {
      GchiuChroma = GchiuChromas[ChannelNo];
      ChromaData = ChromaDatas[ChannelNo];
      for (k=0 ; k<(Width/ChromaPixelWidth)*(Height/ChromaPixelHeight) ; k++)  GchiuChroma[k] = 0.0;
      for (k=0 ; k<(Width/ChromaPixelWidth)*(Height/ChromaPixelHeight) ; k++)  if (!chiChroma[k])  AddPreBlurValue(GchiuChroma,k%(Width/ChromaPixelWidth),k/(Width/ChromaPixelWidth),Width/ChromaPixelWidth,Height/ChromaPixelHeight,1.0*ChromaData[ChromaPitch[ChannelNo]*(k/(Width/ChromaPixelWidth))+(k%(Width/ChromaPixelWidth))*ChromaStride],ChromaPreBlurXKernel,ChromaPreBlurXKernelSize,ChromaPreBlurYKernel,ChromaPreBlurYKernelSize);
    }
  }
  /** MAIN LOOP **/
  for (PixelNo=0 ; PixelNo<MaskPixelCount ; PixelNo++)
  {
    x = MaskPixelOrder[PixelNo]%Width;
    y = MaskPixelOrder[PixelNo]/Width;
    GetStructureTensor(&TensorXX,&TensorXY,&TensorYY,
                       GchiLuma,GchiuLuma,chiLuma,LumaWeight,
                       GchiChroma,GchiuChromas,chiChroma,ChromaWeight,
                       ChromaPixelWidth,ChromaPixelHeight,ChromaChannelCount,
                       x,y,Width,Height,
                       Luma4LumaPostBlurXKernel,Luma4LumaPostBlurXKernelSize,1,
                       Luma4LumaPostBlurYKernel,Luma4LumaPostBlurYKernelSize,1,
                       (ChromaWeight!=0.0)?Chroma4LumaPostBlurXKernels[x%ChromaPixelWidth]:0,Chroma4LumaPostBlurXKernelSize,2*(x%ChromaPixelWidth)+1<ChromaPixelWidth?(-1):(+1),
                       (ChromaWeight!=0.0)?Chroma4LumaPostBlurYKernels[y%ChromaPixelHeight]:0,Chroma4LumaPostBlurYKernelSize,2*(y%ChromaPixelHeight)+1<ChromaPixelHeight?(-1):(+1));
    CoherenceStrength = GetCoherenceData(&CoherenceDirectionX,&CoherenceDirectionY,TensorXX/PixelWidth2,TensorXY,TensorYY/PixelHeight2,Sharpness);
    Value = InpaintPixel(LumaRadiusX,LumaRadiusY,LumaData,chiLuma,LumaStride,LumaPitch,x,y,Width,Height,CoherenceStrength,CoherenceDirectionX,CoherenceDirectionY);
    /* What to do if inpainting failed ?? */
    if (Value<0.0)
    {
      #if (defined(InpaintFailError) && ((InpaintFailError)!=0))
      ErrorText = (InpaintData->PixelType&AVS_CS_YUV)?"InpaintLogo internal error: inpainting luma pixel failed":"InpaintLogo internal error: inpainting first color channel pixel failed";
      break;
      #else
      Value = 1.0*LumaData[LumaPitch*y+x*LumaStride];
      #endif
    }
    LumaData[LumaPitch*y+x*LumaStride] = Value = round(Value);
    if (GchiuLuma)  AddPreBlurValue(GchiuLuma,x,y,Width,Height,Value,LumaPreBlurXKernel,LumaPreBlurXKernelSize,LumaPreBlurYKernel,LumaPreBlurYKernelSize);
    if (GchiLuma)  AddPreBlurValue(GchiLuma,x,y,Width,Height,1.0,LumaPreBlurXKernel,LumaPreBlurXKernelSize,LumaPreBlurYKernel,LumaPreBlurYKernelSize);
    chiLuma[MaskPixelOrder[PixelNo]] = 0;
    if (!ChromaChannelCount)  continue;
    x /= ChromaPixelWidth;
    y /= ChromaPixelHeight;
    if (chiChroma[Width/ChromaPixelWidth*y+x]==1)
    {
      if (ChromaTensor)
      {
        GetStructureTensor(&TensorXX,&TensorXY,&TensorYY,
                           GchiLuma,GchiuLuma,chiLuma,LumaWeight,
                           GchiChroma,GchiuChromas,chiChroma,ChromaWeight,
                           ChromaPixelWidth,ChromaPixelHeight,ChromaChannelCount,
                           x*ChromaPixelWidth+(ChromaPixelWidth-1)/2,y*ChromaPixelHeight+(ChromaPixelHeight-1)/2,Width,Height,
                           Luma4ChromaPostBlurXKernel,Luma4ChromaPostBlurXKernelSize,1,
                           Luma4ChromaPostBlurYKernel,Luma4ChromaPostBlurYKernelSize,1,
                           Chroma4ChromaPostBlurXKernel,Chroma4ChromaPostBlurXKernelSize,1,
                           Chroma4ChromaPostBlurYKernel,Chroma4ChromaPostBlurYKernelSize,1);
        CoherenceStrength = GetCoherenceData(&CoherenceDirectionX,&CoherenceDirectionY,TensorXX/PixelWidth2,TensorXY,TensorYY/PixelHeight2,Sharpness);
      }
      for (ChannelNo=0 ; ChannelNo<ChromaChannelCount ; ChannelNo++)
      {
        ChromaData = ChromaDatas[ChannelNo];
        Value = InpaintPixel(ChromaRadiusX,ChromaRadiusY,ChromaData,chiChroma,ChromaStride,ChromaPitch[ChannelNo],x,y,Width/ChromaPixelWidth,Height/ChromaPixelHeight,CoherenceStrength,CoherenceDirectionX,CoherenceDirectionY);
        if (Value<0.0)
        {
          /* When inpainting fails on a subsampled channel, then the mask (almost certainly) contains to small holes (not covered by chroma resolution). If otherwise, we have a serious problem with our code (as above in the luma case). */
          #if (defined(InpaintFailError) && ((InpaintFailError)!=0) && defined(InpaintChromaFailError) && ((InpaintChromaFailError)!=0))
          ErrorText = ((InpaintData->PixelType&&AVS_CS_YUV)&&(ChromaPixelWidth-1)&&(ChromaPixelHeight-1))?"InpaintLogo: inpainting chroma pixel failed (maybe mask contains a small hole not resolved by chroma subsampling, or internal error)":"InpaintLogo internal error: inpainting higher color channel pixel failed";
          break;
          #else
          #if (defined(InpaintChromaFailError) && ((InpaintChromaFailError)!=0))
          if ((InpaintData->PixelType&AVS_CS_YUV)&&(ChromaPixelWidth-1)&&(ChromaPixelHeight-1))
          {
            ErrorText = "InpaintLogo: inpainting chroma pixel failed (maybe mask contains a small hole not resolved by chroma subsampling, or internal error)";
            break;
          }
          else
          #elif (defined(InpaintFailError) && ((InpaintFailError)!=0))
          if (!((InpaintData->PixelType&AVS_CS_YUV)&&(ChromaPixelWidth-1)&&(ChromaPixelHeight-1)))
          {
            ErrorText = "InpaintLogo internal error: inpainting higher color channel pixel failed";
            break;
          }
          else
          #endif
          {
            Value = 1.0*ChromaData[ChromaPitch[ChannelNo]*y+x*ChromaStride];
          }
          #endif
        }
        ChromaData[ChromaPitch[ChannelNo]*y+x*ChromaStride] = Value = round(Value);
        if (GchiuChromas[ChannelNo])  AddPreBlurValue(GchiuChromas[ChannelNo],x,y,Width/ChromaPixelWidth,Height/ChromaPixelHeight,Value,ChromaPreBlurXKernel,ChromaPreBlurXKernelSize,ChromaPreBlurYKernel,ChromaPreBlurYKernelSize);
      }
      if (ErrorText)  break;
      if (GchiChroma)  AddPreBlurValue(GchiChroma,x,y,Width/ChromaPixelWidth,Height/ChromaPixelHeight,1.0,ChromaPreBlurXKernel,ChromaPreBlurXKernelSize,ChromaPreBlurYKernel,ChromaPreBlurYKernelSize);
    }
    if (chiChroma[Width/ChromaPixelWidth*y+x])  chiChroma[Width/ChromaPixelWidth*y+x]--;
  }
  for (ChannelNo=0 ; ChannelNo<MaxChannelCount-1 ; ChannelNo++)  if (GchiuChromas[ChannelNo])  free(GchiuChromas[ChannelNo]);
  if (GchiuLuma)  free(GchiuLuma);
  if (InpaintData->StaticMaskData)
  {
    if (GchiChroma)  free(GchiChroma);
    if (chiChroma)  free(chiChroma);
    if (GchiLuma)  free(GchiLuma);
    if (chiLuma)  free(chiLuma);
  }
  else
  {
    InpaintPrepareMask(&MaskData,0,InpaintData);
  }
  if (ErrorText)
  {
    if (Frame)  avs_release_video_frame(Frame);
    Frame = 0;
    FilterInfo->error = ErrorText;
  }
  return  Frame;
}



void  AVSC_CC  Inpaint_Free(AVS_FilterInfo *  FilterInfo)
{
  InpaintDataStruct * InpaintData;
  int  k;


  if (FilterInfo->user_data)
  {
    InpaintData = (InpaintDataStruct*)FilterInfo->user_data;
    if (InpaintData->StaticMaskData)  InpaintPrepareMask(&InpaintData->StaticMaskData,0,InpaintData);
    if (InpaintData->Chroma4LumaPostBlurYKernels)  free(InpaintData->Chroma4LumaPostBlurYKernels);
    if (InpaintData->Chroma4LumaPostBlurXKernels)  free(InpaintData->Chroma4LumaPostBlurXKernels);
    if (InpaintData->Chroma4LumaPostBlurYKernelVectors)
    {
      k = InpaintData->ChromaPixelHeight;
      while (k--)  if (InpaintData->Chroma4LumaPostBlurYKernelVectors[k])  free(InpaintData->Chroma4LumaPostBlurYKernelVectors[k]);
      free(InpaintData->Chroma4LumaPostBlurYKernelVectors);
    }
    if (InpaintData->Chroma4LumaPostBlurXKernelVectors)
    {
      k = InpaintData->ChromaPixelWidth;
      while (k--)  if (InpaintData->Chroma4LumaPostBlurXKernelVectors[k])  free(InpaintData->Chroma4LumaPostBlurXKernelVectors[k]);
      free(InpaintData->Chroma4LumaPostBlurXKernelVectors);
    }
    if (InpaintData->Luma4ChromaPostBlurYKernelVector)  free(InpaintData->Luma4ChromaPostBlurYKernelVector);
    if (InpaintData->Luma4ChromaPostBlurXKernelVector)  free(InpaintData->Luma4ChromaPostBlurXKernelVector);
    if (InpaintData->Chroma4ChromaPostBlurYKernelVector)  free(InpaintData->Chroma4ChromaPostBlurYKernelVector);
    if (InpaintData->Chroma4ChromaPostBlurXKernelVector)  free(InpaintData->Chroma4ChromaPostBlurXKernelVector);
    if (InpaintData->Luma4LumaPostBlurYKernelVector)  free(InpaintData->Luma4LumaPostBlurYKernelVector);
    if (InpaintData->Luma4LumaPostBlurXKernelVector)  free(InpaintData->Luma4LumaPostBlurXKernelVector);
    if (InpaintData->ChromaPreBlurYKernelVector)  free(InpaintData->ChromaPreBlurYKernelVector);
    if (InpaintData->ChromaPreBlurXKernelVector)  free(InpaintData->ChromaPreBlurXKernelVector);
    if (InpaintData->LumaPreBlurYKernelVector)  free(InpaintData->LumaPreBlurYKernelVector);
    if (InpaintData->LumaPreBlurXKernelVector)  free(InpaintData->LumaPreBlurXKernelVector);
    if (InpaintData->MaskClip)  avs_release_clip(InpaintData->MaskClip);
    free(InpaintData);
  }
  FilterInfo->user_data = 0;
}



/****************************************************************
                       Deblend callbacks
 ****************************************************************/
AVS_Value  AVSC_CC  Deblend_Create(AVS_ScriptEnvironment *  Env,  AVS_Value  Args,  void *  Data)
{
  AVS_Value  SrcClipValue, DestClipValue;
  AVS_Clip * SrcClip, * LogoClip, * AlphaClip, * DestClip;
  const  AVS_VideoInfo * SrcVideoInfo, * LogoVideoInfo, * AlphaVideoInfo;
  AVS_VideoFrame * LogoFrame, * AlphaFrame;
  int  LogoPixelWidth[MaxChannelCount], LogoPixelHeight[MaxChannelCount], AlphaStride[MaxChannelCount];
  AVS_FilterInfo * FilterInfo;
  DeblendDataStruct * DeblendData;
  int  ChannelCount, ChannelNo;
  char * ErrorText;


  /* We get all input clips and video info pointers */
  SrcClipValue = avs_array_elt(Args,0);
  SrcClip = avs_take_clip(SrcClipValue,Env);
  LogoClip = avs_take_clip(avs_array_elt(Args,1),Env);
  AlphaClip = avs_is_clip(avs_array_elt(Args,2))?avs_take_clip(avs_array_elt(Args,2),Env):0;
  SrcVideoInfo = avs_get_video_info(SrcClip);
  LogoVideoInfo = avs_get_video_info(LogoClip);
  AlphaVideoInfo = AlphaClip?avs_get_video_info(AlphaClip):0;
  /* Check if all color spaces and dimensions and so on are correct */
  ErrorText = 0;
  if (!(avs_has_video(SrcVideoInfo) && SrcVideoInfo->num_frames && SrcVideoInfo->width && SrcVideoInfo->height))  ErrorText = "Deblend: Source clip has no video";
  if (!(ErrorText || avs_is_rgb24(SrcVideoInfo) || avs_is_rgb32(SrcVideoInfo) || avs_is_yuy2(SrcVideoInfo) || avs_is_yv12(SrcVideoInfo)))  ErrorText = "Deblend: Color space unknown (RGB24, RGB32, YUY2, YV12 allowed only)";
  if (!(ErrorText || avs_is_same_colorspace(SrcVideoInfo,LogoVideoInfo) || (avs_is_rgb(SrcVideoInfo) && avs_is_rgb(LogoVideoInfo))))  ErrorText = "Deblend: Source clip and logo clip must have same color spaces (combinations of RGB / RGBA are allowed)";
  if (!(ErrorText || (avs_has_video(LogoVideoInfo) && LogoVideoInfo->num_frames && LogoVideoInfo->width && LogoVideoInfo->height)))  ErrorText = "Deblend: Logo clip has no video";
  if (!(ErrorText || ((SrcVideoInfo->width==LogoVideoInfo->width) && (SrcVideoInfo->height==LogoVideoInfo->height))))  ErrorText = "Deblend: Source clip and logo clip have different dimensions";
  if (!(ErrorText || AlphaClip || avs_is_rgb32(LogoVideoInfo)))  ErrorText = "Deblend: Alpha clip is missing (needed if logo clip is not RGB32)";
  if (!(ErrorText || !AlphaClip || (avs_has_video(AlphaVideoInfo) && AlphaVideoInfo->num_frames)))  ErrorText = "Deblend: Alpha clip has no video";
  if (!(ErrorText || !AlphaClip || avs_is_yuy2(AlphaVideoInfo) || avs_is_yv12(AlphaVideoInfo) || avs_is_rgb32(AlphaVideoInfo)))  ErrorText = "Deblend: Alpha color space must have luma channel (YUY2 or YV12) or alpha channel (RGBA)";
  if (!(ErrorText || !AlphaClip || ((AlphaVideoInfo->width==SrcVideoInfo->width) && (AlphaVideoInfo->height==SrcVideoInfo->height))))  ErrorText = "Deblend: Source clip and alpha clip have different dimensions";
  /* Everything seems to be fine */
  /* Data structure is allocated */
  DeblendData = 0;
  if (!ErrorText)
  {
    DeblendData = (DeblendDataStruct*)malloc(sizeof(DeblendDataStruct));
    if (DeblendData)
    {
      DeblendData->PixelType = SrcVideoInfo->pixel_type;
      DeblendData->Width = SrcVideoInfo->width;
      DeblendData->Height = SrcVideoInfo->height;
      DeblendData->LogoClip = 0;
      DeblendData->AlphaClip = 0;
      DeblendData->LogoPixelType = LogoVideoInfo->pixel_type;
      DeblendData->AlphaPixelType = AlphaVideoInfo?AlphaVideoInfo->pixel_type:0;
      DeblendData->LogoFrameCount = LogoVideoInfo->num_frames;
      DeblendData->AlphaFrameCount = AlphaVideoInfo?AlphaVideoInfo->num_frames:0;
      DeblendData->AlphaStride = 0;
      DeblendData->FlipAlpha = AlphaVideoInfo?(!avs_is_rgb(LogoVideoInfo) ^ !avs_is_rgb32(AlphaVideoInfo)):0;
      for (ChannelNo=0 ; ChannelNo<MaxChannelCount ; ChannelNo++)
      {
        DeblendData->Stride[ChannelNo] = 0;
        DeblendData->PixelWidth[ChannelNo] = 0;
        DeblendData->PixelHeight[ChannelNo] = 0;
        DeblendData->StaticLogoDataChannels[ChannelNo] = 0;
        DeblendData->StaticAlphaDataChannels[ChannelNo] = 0;
        DeblendData->LogoStride[ChannelNo] = 0;
      }
      GetFramePointers(0,0,0,0,DeblendData->Stride,DeblendData->PixelWidth,DeblendData->PixelHeight,SrcVideoInfo->pixel_type);
    }
    else  ErrorText = "Deblend: Could not allocate memory";
  }
  /* Two cases are separated: static logo and dynamic logo */
  if (!ErrorText && (LogoVideoInfo->num_frames==1) && (!AlphaClip || (AlphaVideoInfo->num_frames==1)))
  {
    /* Prepare static logo */
    LogoFrame = 0;
    AlphaFrame = 0;
    if (!ErrorText)
    {
      /* Logo and alpha clip will be evaluated only once - no cache is needed */
      avs_set_cache_hints(LogoClip,AVS_CACHE_NOTHING,0);
      if (AlphaClip)  avs_set_cache_hints(AlphaClip,AVS_CACHE_NOTHING,0);
      /* Get first frame */
      LogoFrame = avs_get_frame(LogoClip,0);
      AlphaFrame = AlphaClip?avs_get_frame(AlphaClip,0):0;
      if (!LogoFrame || (AlphaClip && !AlphaFrame))  ErrorText = "Deblend: Could not get logo/alpha frame";
    }
    if (!ErrorText)
    {
      /* How many channels need to be processed (should be 3, or 4 if alpha channels is part of the logo) */
      ChannelCount = GetFramePointers(LogoFrame,0,0,0,0,LogoPixelWidth,LogoPixelHeight,LogoVideoInfo->pixel_type)-(!AlphaClip);
      /* Memory for static logo data is allocated */
      for (ChannelNo=0 ; ChannelNo<ChannelCount ; ChannelNo++)
      {
        DeblendData->StaticLogoDataChannels[ChannelNo] = (DoubleByte*)malloc(sizeof(DoubleByte)*LogoVideoInfo->width*LogoVideoInfo->height/LogoPixelWidth[ChannelNo]/LogoPixelHeight[ChannelNo]);
        if (!DeblendData->StaticLogoDataChannels[ChannelNo])  ErrorText = "Deblend: Could not allocate memory";
      }
      DeblendData->StaticAlphaDataChannels[0] = (BYTE*)malloc(sizeof(DoubleByte)*LogoVideoInfo->width*LogoVideoInfo->height);
      if (!DeblendData->StaticAlphaDataChannels[0])  ErrorText = "Deblend: Could not allocate memory";
      if (avs_is_yuy2(SrcVideoInfo) || avs_is_yv12(SrcVideoInfo))
      {
        DeblendData->StaticAlphaDataChannels[1] = (BYTE*)malloc(sizeof(DoubleByte)*LogoVideoInfo->width*LogoVideoInfo->height);
        if (!DeblendData->StaticAlphaDataChannels[1])  ErrorText = "Deblend: Could not allocate memory";
      }
    }
    if (!ErrorText)
    {
      /* Core function for static logos is called */
      if (0>DeblendStaticPrepare(DeblendData->StaticLogoDataChannels,DeblendData->StaticAlphaDataChannels,LogoFrame,AlphaFrame,LogoVideoInfo,AlphaVideoInfo))  ErrorText = "Deblend: Could not gather frame data pointers";
    }

    if (AlphaFrame)  avs_release_video_frame(AlphaFrame); // fixed in v1.1
    if (LogoFrame)  avs_release_video_frame(LogoFrame);
    if (AlphaClip)  avs_release_clip(AlphaClip);
    if (LogoClip)  avs_release_clip(LogoClip);
    LogoClip = 0;
    AlphaClip = 0;
  }
  else
  {
    if (!ErrorText)
    {
      GetFramePointers(0,0,0,0,DeblendData->LogoStride,0,0,LogoVideoInfo->pixel_type);
      if (AlphaClip)
      {
        GetFramePointers(0,0,0,0,AlphaStride,0,0,AlphaVideoInfo->pixel_type);
        DeblendData->AlphaStride = AlphaStride[avs_is_rgb32(AlphaVideoInfo)?3:0];
      }
      else  DeblendData->AlphaStride = DeblendData->LogoStride[3];  /* If this happens, LogoClip *must* be RGBA */
      DeblendData->LogoClip = LogoClip;
      DeblendData->AlphaClip = AlphaClip;
    }
  }
  if (SrcClip)  avs_release_clip(SrcClip);
  DestClip = 0;
  /* Create filter */
  if (!ErrorText)
  {
    DestClip = avs_new_c_filter(Env,&FilterInfo,SrcClipValue,1);
    if (!DestClip)  ErrorText = "Deblend: Could not create filter";
  }
  /* Set target clip information */
  if (!ErrorText)
  {
    FilterInfo->get_frame = (DeblendData->LogoClip)?Deblend_Dynamic_GetFrame:Deblend_Static_GetFrame;
    FilterInfo->free_filter = Deblend_Free;
    FilterInfo->user_data = DeblendData;
    DestClipValue = avs_new_value_clip(DestClip);
    if (!avs_defined(DestClipValue))  ErrorText = "Deblend: Could not create target clip";
  }
  /* Release target clip */
  if (DestClip)  avs_release_clip(DestClip);
  if (!ErrorText)  return  DestClipValue;
  if (DeblendData)
  {
    for (ChannelNo=0 ; ChannelNo<MaxChannelCount ; ChannelNo++)
    {
      if (DeblendData->StaticAlphaDataChannels[ChannelNo])  free(DeblendData->StaticAlphaDataChannels[ChannelNo]);
      if (DeblendData->StaticLogoDataChannels[ChannelNo])  free(DeblendData->StaticLogoDataChannels[ChannelNo]);
    }
    free(DeblendData);
  }
  if (AlphaClip)  avs_release_clip(AlphaClip);
  if (LogoClip)  avs_release_clip(LogoClip);
  return  avs_new_value_error(ErrorText);
}



AVS_VideoFrame *  AVSC_CC  Deblend_Static_GetFrame(AVS_FilterInfo *  FilterInfo,  int  FrameNo)
{
  DeblendDataStruct * DeblendData;
  AVS_VideoFrame * Frame;
  int  ChannelCount, Width, Height, PixelType, FramePitch[MaxChannelCount], * FrameStride, * FramePixelWidth, * FramePixelHeight;
  BYTE * FrameData[MaxChannelCount], * AlphaData;
  int  ChannelNo;


  /* Get some data from the data structure, load frames and get data pointers */
  DeblendData = (DeblendDataStruct*)FilterInfo->user_data;
  PixelType = DeblendData->PixelType;
  Width = DeblendData->Width;
  Height = DeblendData->Height;
  FrameStride = DeblendData->Stride;
  FramePixelWidth = DeblendData->PixelWidth;
  FramePixelHeight = DeblendData->PixelHeight;
  Frame = avs_get_frame(FilterInfo->child,FrameNo);
  avs_make_writable(FilterInfo->env,&Frame);
  ChannelCount = GetFramePointers(Frame,FrameData,0,FramePitch,0,0,0,PixelType);
  if (!ChannelCount)
  {
    FilterInfo->error = Frame?"Deblend: Could not get source data pointers":"Deblend: Could not get source frame";
    if (Frame)  avs_release_video_frame(Frame);
    return  0;
  }
  AlphaData = 0;
  for (ChannelNo=0 ; ChannelNo<MaxChannelCount ; ChannelNo++)
  {
    if (!DeblendData->StaticLogoDataChannels[ChannelNo])  break;
    if (DeblendData->StaticAlphaDataChannels[ChannelNo])  AlphaData = DeblendData->StaticAlphaDataChannels[ChannelNo];
    DeblendStatic(FrameData[ChannelNo],DeblendData->StaticLogoDataChannels[ChannelNo],AlphaData,FramePitch[ChannelNo],FrameStride[ChannelNo],Width/FramePixelWidth[ChannelNo],Height/FramePixelHeight[ChannelNo]);
  }
  return  Frame;
}



AVS_VideoFrame *  AVSC_CC  Deblend_Dynamic_GetFrame(AVS_FilterInfo *  FilterInfo,  int  FrameNo)
{
  DeblendDataStruct * DeblendData;
  AVS_VideoFrame * Frame, * LogoFrame, * AlphaFrame;
  int  ChannelCount, PixelType, Width, Height, FramePitch[MaxChannelCount], * FrameStride, * FramePixelWidth, * FramePixelHeight;
  int  LogoChannelCount, LogoPixelType, LogoFramePitch[MaxChannelCount], * LogoFrameStride;
  int  AlphaChannelCount, AlphaPixelType, AlphaFramePitch[MaxChannelCount], AlphaPitch, AlphaStride;
  BYTE * FrameData[MaxChannelCount];
  const  BYTE * LogoFrameData[MaxChannelCount], * AlphaFrameData[MaxChannelCount], * AlphaData;
  int  ChannelNo;
  char * ErrorText;


  /* Get some data from the data structure, load frames and get data pointers */
  DeblendData = (DeblendDataStruct*)FilterInfo->user_data;
  PixelType = DeblendData->PixelType;
  Width = DeblendData->Width;
  Height = DeblendData->Height;
  FrameStride = DeblendData->Stride;
  FramePixelWidth = DeblendData->PixelWidth;
  FramePixelHeight = DeblendData->PixelHeight;
  LogoFrameStride = DeblendData->LogoStride;
  AlphaStride = DeblendData->AlphaStride;
  Frame = avs_get_frame(FilterInfo->child,FrameNo);
  avs_make_writable(FilterInfo->env,&Frame);
  ChannelCount = GetFramePointers(Frame,FrameData,0,FramePitch,0,0,0,PixelType);
  LogoPixelType = DeblendData->LogoPixelType;
  LogoFrame = avs_get_frame(DeblendData->LogoClip,(FrameNo<DeblendData->LogoFrameCount)?FrameNo:(DeblendData->LogoFrameCount-1));
  LogoChannelCount = GetFramePointers(LogoFrame,0,LogoFrameData,LogoFramePitch,0,0,0,LogoPixelType);
  /* Maybe we need the alpha frame, maybe it's part of the logo (rgbA) */
  if (DeblendData->AlphaClip)
  {
    AlphaPixelType = DeblendData->AlphaPixelType;
    AlphaFrame = avs_get_frame(DeblendData->AlphaClip,(FrameNo<DeblendData->AlphaFrameCount)?FrameNo:(DeblendData->AlphaFrameCount-1));
    AlphaChannelCount = GetFramePointers(AlphaFrame,0,AlphaFrameData,AlphaFramePitch,0,0,0,AlphaPixelType);
  }
  else
  {
    AlphaPixelType = 0;
    AlphaFrame = 0;
    AlphaChannelCount = 0;
  }
  /* If something failed (could not get frame or data pointers), we abort */
  ErrorText = 0;
  if (!(ChannelCount && LogoChannelCount && (!DeblendData->AlphaClip || AlphaChannelCount)))
  {
    if (!AlphaChannelCount)  ErrorText = "Deblend: Could not get alpha channel data pointers";
    if (!AlphaFrame)  ErrorText = "Deblend: Could not get alpha channel frame";
    if (!LogoChannelCount)  ErrorText = "Deblend: Could not get logo data pointers";
    if (!LogoFrame)  ErrorText = "Deblend: Could not get logo frame";
    if (!ChannelCount)  ErrorText = "Deblend: Could not get source data pointers";
    if (!Frame)  ErrorText = "Deblend: Could not get source frame";
  }
  /* Check some assertions */
  if (!ErrorText && (AlphaPixelType==AVS_CS_BGR32) && (AlphaChannelCount!=4))  ErrorText = "Deblend internal error: RGBA clip has no alpha channel";
  if (!ErrorText && (AlphaChannelCount==0) && (LogoPixelType!=AVS_CS_BGR32))  ErrorText = "Deblend internal error: No alpha channel at all";
  if (ErrorText)
  {
    FilterInfo->error = ErrorText;
    if (AlphaFrame)  avs_release_video_frame(AlphaFrame);
    if (LogoFrame)  avs_release_video_frame(LogoFrame);
    if (Frame)  avs_release_video_frame(Frame);
    return  0;
  }
  /* Prepare data pointers */
  if (AlphaFrame)
  {
    AlphaData = AlphaFrameData[(AlphaPixelType==AVS_CS_BGR32)?3:0];  /* If Alpha is not RGBA, the first channel (Y) is the effective alpha channel */
    AlphaPitch = AlphaFramePitch[(AlphaPixelType==AVS_CS_BGR32)?3:0];
    if (DeblendData->FlipAlpha)
    {
      AlphaData += (DeblendData->Height-1)*AlphaPitch;
      AlphaPitch *= -1;
    }
  }
  else
  {
    /* If we are here, LogoClip *must* be RGBA */
    AlphaData = LogoFrameData[3];
    AlphaPitch = LogoFramePitch[3];
    LogoChannelCount = 3;
  }
  if (ChannelCount>LogoChannelCount)  ChannelCount = LogoChannelCount;
  for (ChannelNo=0 ; ChannelNo<ChannelCount ; ChannelNo++)
  {
    if (FramePixelWidth[ChannelNo]*FramePixelHeight[ChannelNo]==1)  DeblendDynamic(FrameData[ChannelNo],LogoFrameData[ChannelNo],AlphaData,FramePitch[ChannelNo],FrameStride[ChannelNo],LogoFramePitch[ChannelNo],LogoFrameStride[ChannelNo],AlphaPitch,AlphaStride,Width,Height);
    else  DeblendDynamicSubsampled(FrameData[ChannelNo],LogoFrameData[ChannelNo],AlphaData,FramePitch[ChannelNo],FrameStride[ChannelNo],LogoFramePitch[ChannelNo],LogoFrameStride[ChannelNo],AlphaPitch,AlphaStride,Width,Height,FramePixelWidth[ChannelNo],FramePixelHeight[ChannelNo]);
  }
  if (AlphaFrame)  avs_release_video_frame(AlphaFrame);
  if (LogoFrame)  avs_release_video_frame(LogoFrame);
  return  Frame;
}
  
  
  
void  AVSC_CC  Deblend_Free(AVS_FilterInfo *  FilterInfo)
{
  DeblendDataStruct * DeblendData;
  int  ChannelNo;


  if (FilterInfo->user_data)
  {
    DeblendData = (DeblendDataStruct*)FilterInfo->user_data;
    for (ChannelNo=0 ; ChannelNo<MaxChannelCount ; ChannelNo++)  if (DeblendData->StaticAlphaDataChannels[ChannelNo])  free(DeblendData->StaticAlphaDataChannels[ChannelNo]);
    for (ChannelNo=0 ; ChannelNo<MaxChannelCount ; ChannelNo++)  if (DeblendData->StaticLogoDataChannels[ChannelNo])  free(DeblendData->StaticLogoDataChannels[ChannelNo]);
    if (DeblendData->AlphaClip)  avs_release_clip(DeblendData->AlphaClip);
    if (DeblendData->LogoClip)  avs_release_clip(DeblendData->LogoClip);
    free(DeblendData);
  }
}



/****************************************************************
                      Analyze callbacks
 ****************************************************************/
AVS_Value  AVSC_CC  Analyze_Create(AVS_ScriptEnvironment *  Env,  AVS_Value  Args,  void *  Data)
{
  AVS_Value  DestClipValue, SrcClipValue;
  AVS_Clip * SrcClip, * MaskClip, * DestClip;
  const  AVS_VideoInfo * SrcVideoInfo, * MaskVideoInfo;
  AVS_VideoInfo * DestVideoInfo;
  AVS_VideoFrame * MaskFrame, * DestFrame;
  AVS_FilterInfo * FilterInfo;
  const  BYTE * MaskData;
  int  MaskStride, MaskPitch;
  int  MaskGiven, ComputeAlpha;
  double  DeviationWeight, SubsamplingWeight;
  signed long  MaskSize, AnalyzeResult;
  BYTE * Mask;
  char * ErrorText;
  /* For mask-processing */
  int  x, y;


  /* First all pointers are initialized */
  ErrorText = 0;
  MaskFrame = 0;
  MaskData = 0;
  DestClip = 0;
  Mask = 0;
  MaskPitch = 0;
  MaskStride = 0;
  MaskData = 0;
  MaskSize = 0;
  DestFrame = 0;
  AnalyzeResult = 0;
  DestVideoInfo = 0;
  /* Get clip(s) and VideoInfo-pointer(s) */
  ComputeAlpha = avs_is_bool(avs_array_elt(Args,2))?avs_as_bool(avs_array_elt(Args,2)):1;
  DeviationWeight = avs_is_float(avs_array_elt(Args,3))?avs_as_float(avs_array_elt(Args,3)):0.5;
  if (DeviationWeight<0.0)  return  avs_new_value_error("Analyze: Deviation weight must not be negative" "\n\n" LegalInfoString);
  SubsamplingWeight = avs_is_float(avs_array_elt(Args,4))?avs_as_float(avs_array_elt(Args,4)):0.5;
  if (SubsamplingWeight<0.0)  return  avs_new_value_error("Analyze: Subsampling weight must not be negative" "\n\n" LegalInfoString);

  SrcClipValue = avs_array_elt(Args,0);
  SrcClip = avs_take_clip(SrcClipValue,Env);
  MaskGiven = avs_is_clip(avs_array_elt(Args,1));
  MaskClip = MaskGiven?avs_take_clip(avs_array_elt(Args,1),Env):0;
  SrcVideoInfo = avs_get_video_info(SrcClip);
  MaskVideoInfo = MaskGiven?avs_get_video_info(MaskClip):0;
  /* Check if everything is consistent. (ErrorText) indicates an error. */
  if (!(avs_has_video(SrcVideoInfo) && SrcVideoInfo->num_frames))  ErrorText = "Analyze: Source clip has no video";
  if (!(ErrorText || avs_is_rgb24(SrcVideoInfo) || avs_is_rgb32(SrcVideoInfo) || avs_is_yuy2(SrcVideoInfo) || avs_is_yv12(SrcVideoInfo)))  ErrorText = "Analyze: Color space unknown (RGB24, RGB32, YUY2, YV12 allowed only)";
  if (!ErrorText && ((SrcVideoInfo->width==0) || (SrcVideoInfo->height==0) || (SrcVideoInfo->width==1 && SrcVideoInfo->height==1)))  ErrorText = "Analyze: Source clip too small";
  if (!ErrorText && ComputeAlpha && (SrcVideoInfo->num_frames<2))  ErrorText = "Analyze: Source clip must have at least two frames for alpha computation";
  if (!(ErrorText || MaskClip || avs_is_rgb32(SrcVideoInfo)))  ErrorText = "Analyze: Mask is missing (needed if source clip is not RGB32)";
  if (!ErrorText && MaskClip && !(avs_has_video(MaskVideoInfo) && MaskVideoInfo->num_frames))  ErrorText = "Analyze: Mask clip has no video";
  if (!ErrorText && MaskClip && !avs_is_yuy2(MaskVideoInfo) && !avs_is_yv12(MaskVideoInfo) && !avs_is_rgb32(MaskVideoInfo))  ErrorText = "Analyze: Mask color space must have luma channel (YUY2 or YV12) or alpha channel (RGB32)";
  if (!ErrorText && MaskClip && !(MaskVideoInfo->width==SrcVideoInfo->width && MaskVideoInfo->height==SrcVideoInfo->height))  ErrorText = "Analyze: Source clip and mask clip have different dimensions";
  /* Done! All arguments seem to be correct */
  /* As this filter is already done when the script is loaded, we try to deactivate all cache systems - Is this smart ? */
  if (!ErrorText)  avs_set_cache_hints(SrcClip,AVS_CACHE_NOTHING,0);
  if (!ErrorText && MaskClip)  avs_set_cache_hints(MaskClip,AVS_CACHE_NOTHING,0);
  /* Load the mask frame... */
  if (!ErrorText)
  {
    MaskFrame = avs_get_frame(MaskClip?MaskClip:SrcClip,0);
    if (!MaskFrame)  ErrorText = "Analyze: Could not get mask frame";
  }
  /* ...and get pointers to pixel data */
  if (!ErrorText)
  {
    if (MaskClip && avs_is_yuv(MaskVideoInfo))
    {
      MaskData = avs_get_read_ptr_p(MaskFrame,AVS_PLANAR_Y);
      MaskPitch = avs_get_pitch_p(MaskFrame,AVS_PLANAR_Y);
      MaskStride = avs_is_yuy2(MaskVideoInfo)?2:1;
    }
    else
    {
      MaskData = avs_get_read_ptr(MaskFrame)+3;
      MaskPitch = avs_get_pitch(MaskFrame);
      MaskStride = 4;
    }
    if (!MaskData)  ErrorText = "Analyze: Could not gather pixel data pointer";
  }
  if (!ErrorText)
  {
    /* If one color space is YUV, the other RGB, we flip the mask so it fits the source */
    if (MaskClip && ((!avs_is_yuv(SrcVideoInfo)) != (!avs_is_yuv(MaskVideoInfo))))
    {
      MaskData += (SrcVideoInfo->height-1)*MaskPitch;
      MaskPitch *= -1;
    }
  }
  /* Release mask clip */
  if (MaskClip)  avs_release_clip(MaskClip);
  /* Allocate mask memory */
  if (!ErrorText)
  {
    Mask = (BYTE*)malloc(sizeof(BYTE)*SrcVideoInfo->width*SrcVideoInfo->height);
    if (!Mask)  ErrorText = "Analyze: Could not allocate memory";
  }
  /* Determine mask from mask frame: All values above some threshold are masked */
  if (!ErrorText)
  {
    /* Transform mask */
    MaskSize = 0;
    if (ComputeAlpha)
    {
      for (y=0 ; y<SrcVideoInfo->height ; y++)
      {
        for (x=0 ; x<SrcVideoInfo->width ; x++)
        {
          MaskSize += Mask[SrcVideoInfo->width*y+x] = ((MaskThresholdSign)*MaskData[MaskPitch*y+x*MaskStride]>(MaskThresholdSign)*(MaskThreshold));
        }
      }
    }
    else
    {
      for (y=0 ; y<SrcVideoInfo->height ; y++)
      {
        for (x=0 ; x<SrcVideoInfo->width ; x++)
        {
          Mask[SrcVideoInfo->width*y+x] = MaskData[MaskPitch*y+x*MaskStride];
          MaskSize += Mask[SrcVideoInfo->width*y+x]?1:0;
        }
      }
    }
    if (!MaskSize)  ErrorText = "Analyze: Mask is empty";
    if (MaskSize==SrcVideoInfo->width*SrcVideoInfo->height)  ErrorText = "Analyze: Mask is full";
  }
  /* Release mask frame */
  if (MaskFrame)  avs_release_video_frame(MaskFrame);
  /* Target frame is allocated */
  if (!ErrorText)
  {
    DestVideoInfo = (AVS_VideoInfo*)malloc(sizeof(AVS_VideoInfo));
    if (!DestVideoInfo)  ErrorText = "Analyze: Could not allocate memory";
  }
  if (!ErrorText)
  {
    DestVideoInfo[0] = SrcVideoInfo[0];
    if (MaskGiven)  DestVideoInfo->height *= 2;
    DestFrame = avs_new_video_frame(Env,DestVideoInfo);
    if (!DestFrame)  ErrorText = "Analyze: Target frame could not be created";
  }
  /* MAIN PART: Calls core function (analyze), which evaluates all source clip frames and determines logo frame */
  if (!ErrorText)
  {
    AnalyzeResult = Analyze(DestFrame,DestVideoInfo,SrcClip,Mask,MaskSize,ComputeAlpha,DeviationWeight,SubsamplingWeight);
    if (AnalyzeResult==Analyze_Result_SrcClipError)  ErrorText = "Analyze: Problem while passing source clip";
    if (AnalyzeResult==Analyze_Result_DestFrameError)  ErrorText = "Analyze: Could not write to target frame";
    if (AnalyzeResult==Analyze_Result_MemoryError)  ErrorText = "Analyze: Could not allocate memory";
    if (AnalyzeResult==Analyze_Result_GeneralError)  ErrorText = "Analyze: A totally strange error occured while passing the source clip";
  }
  /* Free mask memory and source clip */
  if (DestVideoInfo)  free(DestVideoInfo);
  if (Mask)  free(Mask);
  if (SrcClip)  avs_release_clip(SrcClip);
  /* Create filter */
  if (!ErrorText)
  {
    DestClip = avs_new_c_filter(Env,&FilterInfo,SrcClipValue,1);
    if (!DestClip)  ErrorText = "Analyze: Could not create filter";
  }
  /* Set target clip information */
  if (!ErrorText)
  {
    if (MaskGiven)  FilterInfo->vi.height *= 2;
    FilterInfo->vi.num_frames = AnalyzeResult;
    FilterInfo->vi.audio_samples_per_second = 0;
    FilterInfo->vi.num_audio_samples = 0;
    FilterInfo->vi.nchannels = 0;
    FilterInfo->get_frame = Analyze_GetFrame;
    FilterInfo->get_parity = 0;
    FilterInfo->get_audio = 0;
    FilterInfo->set_cache_hints = 0;
    FilterInfo->free_filter = Analyze_Free;
    FilterInfo->user_data = DestFrame;
    DestClipValue = avs_new_value_clip(DestClip);
    if (!avs_defined(DestClipValue))  ErrorText = "Analyze: Could not create target clip";
  }
  /* Release target clip */
  if (DestClip)  avs_release_clip(DestClip);
  if (!ErrorText)  return  DestClipValue;
  avs_release_video_frame(DestFrame);
  return  avs_new_value_error(ErrorText);
}



AVS_VideoFrame *  AVSC_CC  Analyze_GetFrame(AVS_FilterInfo *  FilterInfo,  int  FrameNo)
{
  return  avs_copy_video_frame((AVS_VideoFrame*)FilterInfo->user_data);
}



void  AVSC_CC  Analyze_Free(AVS_FilterInfo *  FilterInfo)
{
  if (FilterInfo->user_data)  avs_release_video_frame((AVS_VideoFrame*)FilterInfo->user_data);
}



/****************************************************************
                  DistanceFunction callbacks
 ****************************************************************/
AVS_Value  AVSC_CC  DistanceFunction_Create(AVS_ScriptEnvironment *  Env,  AVS_Value  Args,  void *  Data)
{
  DistanceFunctionDataStruct *  DistFcnData;
  AVS_Value  DestClipValue;
  AVS_Clip * DestClip;
  AVS_FilterInfo * FilterInfo;
  AVS_VideoInfo * VideoInfo;
  int  Stride[MaxChannelCount];
  double  PAR, Scale;
  char * ErrorText;


  /* Extract values from AVS_Value container, throw errors if necessary */
  Scale = avs_is_float(avs_array_elt(Args,1))?avs_as_float(avs_array_elt(Args,1)):1.0;
  PAR = avs_is_float(avs_array_elt(Args,2))?avs_as_float(avs_array_elt(Args,2)):1.0;
  if (PAR<=0.0)  return  avs_new_value_error("DistanceFunction: Pixel aspect ratio must be positive" "\n\n" LegalInfoString);
  /* Create new filter */
  DestClip = avs_new_c_filter(Env,&FilterInfo,avs_array_elt(Args,0),1);
  if (!DestClip)  return  avs_new_value_error("DistanceFunction: Could not create filter");
  VideoInfo = &FilterInfo->vi;
  /* If something fails here, we store a note in ErrorText. This will be checked later, and memory will be cleaned up before leaving */
  ErrorText = 0;
  DistFcnData = 0;
  /* Check all video properties */
  if (!(ErrorText || avs_is_yuy2(VideoInfo) || avs_is_yv12(VideoInfo) || avs_is_rgb32(VideoInfo)))  ErrorText = "DistanceFunction: Source clip must have luma channel (YUY2 or YV12) or alpha channel (RGB32)";
  if (!(ErrorText || (avs_has_video(VideoInfo) && VideoInfo->num_frames && VideoInfo->width && VideoInfo->height)))  ErrorText = "DistanceFunction: Source clip has no video";
  if (!ErrorText)
  {
    /* Allocate data struct */
    DistFcnData = malloc(sizeof(DistanceFunctionDataStruct));
    if (!DistFcnData)  ErrorText = "DistanceFunction: Could not allocate memory";
  }
  if (DistFcnData)
  {
    DistFcnData->PixelType = VideoInfo->pixel_type;
    DistFcnData->LumaChannelNo = avs_is_rgb32(VideoInfo)?3:0;
    DistFcnData->Width = VideoInfo->width;
    DistFcnData->Height = VideoInfo->height;
    GetFramePointers(0,0,0,0,Stride,0,0,DistFcnData->PixelType);
    DistFcnData->LumaStride = Stride[DistFcnData->LumaChannelNo];
    if (PAR>1.0)
    {
      DistFcnData->PixelWidth = 1.0;
      DistFcnData->PixelHeight = 1.0/PAR;
      DistFcnData->Scale = fabs(Scale)*sqrt(PAR);
    }
    else
    {
      DistFcnData->PixelWidth = PAR;
      DistFcnData->PixelHeight = 1.0;
      DistFcnData->Scale = fabs(Scale)/sqrt(PAR);
    }
    DistFcnData->ScaleSign = (Scale>=0.0)?(+1):(-1);
  }
  DestClipValue = avs_void;
  FilterInfo->get_frame = DistanceFunction_GetFrame;
  FilterInfo->free_filter = DistanceFunction_Free;
  FilterInfo->user_data = DistFcnData;
  DestClipValue = avs_new_value_clip(DestClip);
  if (!avs_defined(DestClipValue))  ErrorText = "DistanceFunction: Could not create target clip";
  avs_release_clip(DestClip);
  if (!ErrorText)  return  DestClipValue;
  /* Error occured!! We clean up everything */
  DistanceFunction_Free(FilterInfo);
  return  avs_new_value_error(ErrorText);
}



AVS_VideoFrame *  AVSC_CC  DistanceFunction_GetFrame(AVS_FilterInfo *  FilterInfo,  int  FrameNo)
{
  DistanceFunctionDataStruct * DistFcnData;
  AVS_VideoFrame * Frame;
  int  Width, Height, k, x, y, FramePitch[MaxChannelCount], Pitch, Stride, ScaleSign;
  float * DistFcn, * u;
  double  Scale;
  BYTE * FrameData[MaxChannelCount], * DataPtr;
  char * ErrorText;


  DistFcnData = (DistanceFunctionDataStruct*)FilterInfo->user_data;
  Width = DistFcnData->Width;
  Height = DistFcnData->Height;
  Stride = DistFcnData->LumaStride;
  Scale = DistFcnData->Scale;
  ScaleSign = DistFcnData->ScaleSign;
  ErrorText = 0;
  Frame = avs_get_frame(FilterInfo->child,FrameNo);
  if (Frame)  avs_make_writable(FilterInfo->env,&Frame);
  FrameData[DistFcnData->LumaChannelNo] = 0;
  if (Frame)  GetFramePointers(Frame,FrameData,0,FramePitch,0,0,0,DistFcnData->PixelType);
  Pitch = FramePitch[DistFcnData->LumaChannelNo];
  if (FrameData[DistFcnData->LumaChannelNo] && (Scale==0.0))
  /* If scale is zero, we simply make the image binary and run away */
  {
    DataPtr = FrameData[DistFcnData->LumaChannelNo];
    for (y=0 ; y<Height ; y++)
    {
      for (x=0 ; x<Width ; x++)
      {
        *DataPtr = ((*DataPtr)>(MaskThreshold))?0xFF:0x00;
        DataPtr += Stride;
      }
      DataPtr += Pitch-Width*Stride;
    }
    return  Frame;
  }
  DistFcn = (float*)malloc(sizeof(float)*Width*Height);
  if (!DistFcn)  ErrorText = "DistanceFunction: Could not allocate memory";
  if (ErrorText || !FrameData[DistFcnData->LumaChannelNo])
  {
    if (DistFcn)  free(DistFcn);
    if (!FrameData[DistFcnData->LumaChannelNo])  ErrorText = "DistanceFunction: Could not get pixel data pointers";
    if (Frame)  avs_release_video_frame(Frame);  else  ErrorText = "DistanceFunction: Could not get video Frame";
    FilterInfo->error = ErrorText;
    return  0;
  }
  u = DistFcn;
  DataPtr = FrameData[DistFcnData->LumaChannelNo];
  for (y=0 ; y<Height ; y++)
  {
    for (x=0 ; x<Width ; x++)
    {
      *(u++) = (ScaleSign*(MaskThresholdSign)*(*DataPtr)>ScaleSign*(MaskThresholdSign)*(MaskThreshold))?0.0:1.0;
      DataPtr += Stride;
    }
    DataPtr += Pitch-Width*Stride;
  }
  if (!FastMarching(DistFcn,0,Width,Height,DistFcnData->PixelWidth,DistFcnData->PixelHeight))
  {
    free(DistFcn);
    avs_release_video_frame(Frame);
    FilterInfo->error = "DistanceFunction: Mask processing failed: Out of memory";
    return  0;
  }
  u = DistFcn;
  for (k=0 ; k<Width*Height ; k++)  *(u++) *= Scale;
  u = DistFcn;
  DataPtr = FrameData[DistFcnData->LumaChannelNo];
  for (y=0 ; y<Height ; y++)
  {
    for (x=0 ; x<Width ; x++)
    {
      *DataPtr = ((*u)>255.0)?0xFF:round(*u);
      DataPtr += Stride;
      u++;
    }
    DataPtr += Pitch-Width*Stride;
  }
  free(DistFcn);
  if (ScaleSign*MaskThresholdSign>0)
  {
    DataPtr = FrameData[DistFcnData->LumaChannelNo];
    for (y=0 ; y<Height ; y++)
    {
      for (x=0 ; x<Width ; x++)
      {
        *DataPtr ^= 0xFF;
        DataPtr += Stride;
      }
      DataPtr += Pitch-Width*Stride;
    }
  }
  return  Frame;
}



void  AVSC_CC  DistanceFunction_Free(AVS_FilterInfo *  FilterInfo)
{
  if (FilterInfo->user_data)  free(FilterInfo->user_data);
}



/****************************************************************
                      Common functions
 ****************************************************************/
int  GetFramePointers(AVS_VideoFrame *  VideoFrame,  BYTE **  WritePtrs,  const BYTE **  ReadPtrs,  int *  Pitch,  int *  Stride,  int *  PixelWidth,  int *  PixelHeight,  int  PixelType)
/* This functions stores some informations about a frame in the
   places given. If read/write pointers are requested and cannot be
   gathered (e.g. write pointers for a read-only frame), 0 is
   returned, otherwise the number of color channels.                */
{
  int  ChannelNo, ChannelCount;


  ChannelCount = MaxChannelCount;
  if (WritePtrs)  for (ChannelNo=0 ; ChannelNo<ChannelCount ; ChannelNo++)  WritePtrs[ChannelNo] = 0;
  if (ReadPtrs)  for (ChannelNo=0 ; ChannelNo<ChannelCount ; ChannelNo++)  ReadPtrs[ChannelNo] = 0;
  if (Pitch)  for (ChannelNo=0 ; ChannelNo<ChannelCount ; ChannelNo++)  Pitch[ChannelNo] = 0;
  if (Stride)  for (ChannelNo=0 ; ChannelNo<ChannelCount ; ChannelNo++)  Stride[ChannelNo] = 0;
  switch (PixelType)
  {
    case AVS_CS_BGR24:
    case AVS_CS_YUY2:
    case AVS_CS_YV12:
    case AVS_CS_I420:
      if (ChannelCount>3)  ChannelCount = 3;
    break;
    case AVS_CS_BGR32:
      if (ChannelCount>4)  ChannelCount = 4;
    break;
    default:
      return  0;
    break;
  }
  if (Stride)
  {
    switch (PixelType)
    {
      case AVS_CS_BGR24:
        for (ChannelNo=0 ; ChannelNo<ChannelCount ; ChannelNo++)  Stride[ChannelNo] = 3;
      break;
      case AVS_CS_BGR32:
        for (ChannelNo=0 ; ChannelNo<ChannelCount ; ChannelNo++)  Stride[ChannelNo] = 4;
      break;
      case AVS_CS_YUY2:
        Stride[0] = 2;
        for (ChannelNo=1 ; ChannelNo<ChannelCount ; ChannelNo++)  Stride[ChannelNo] = 4;
      break;
      case AVS_CS_YV12:
      case AVS_CS_I420:
        for (ChannelNo=0 ; ChannelNo<ChannelCount ; ChannelNo++)  Stride[ChannelNo] = 1;
      break;
    }
  }
  if (Pitch)
  {
    switch (PixelType)
    {
      case AVS_CS_BGR24:
      case AVS_CS_BGR32:
      case AVS_CS_YUY2:
        Pitch[0] = avs_get_pitch(VideoFrame);
        for (ChannelNo=1 ; ChannelNo<ChannelCount ; ChannelNo++)  Pitch[ChannelNo] = Pitch[0];
      break;
      case AVS_CS_YV12:
      case AVS_CS_I420:
        Pitch[0] = avs_get_pitch_p(VideoFrame,AVS_PLANAR_Y);
        if (ChannelCount>1)  Pitch[1] = avs_get_pitch_p(VideoFrame,AVS_PLANAR_U);
        if (ChannelCount>2)  Pitch[2] = avs_get_pitch_p(VideoFrame,AVS_PLANAR_V);
      break;
    }
  }
  if (PixelWidth)
  {
    switch (PixelType)
    {
      case AVS_CS_BGR24:
      case AVS_CS_BGR32:
        for (ChannelNo=0 ; ChannelNo<ChannelCount ; ChannelNo++)  PixelWidth[ChannelNo] = 1;
      break;
      case AVS_CS_YUY2:
      case AVS_CS_YV12:
      case AVS_CS_I420:
        PixelWidth[0] = 1;
        for (ChannelNo=1 ; ChannelNo<ChannelCount ; ChannelNo++)  PixelWidth[ChannelNo] = 2;
      break;
    }
  }
  if (PixelHeight)
  {
    switch (PixelType)
    {
      case AVS_CS_BGR24:
      case AVS_CS_BGR32:
      case AVS_CS_YUY2:
        for (ChannelNo=0 ; ChannelNo<ChannelCount ; ChannelNo++)  PixelHeight[ChannelNo] = 1;
      break;
      case AVS_CS_YV12:
      case AVS_CS_I420:
        PixelHeight[0] = 1;
        for (ChannelNo=1 ; ChannelNo<ChannelCount ; ChannelNo++)  PixelHeight[ChannelNo] = 2;
      break;
    }
  }
  if (ReadPtrs)
  {
    if (!VideoFrame)  return  0;
    switch (PixelType)
    {
      case AVS_CS_BGR24:
      case AVS_CS_BGR32:
      case AVS_CS_YUY2:
        ReadPtrs[0] = avs_get_read_ptr(VideoFrame);
        if (!ReadPtrs[0])  return  0;
        for (ChannelNo=1 ; ChannelNo<ChannelCount ; ChannelNo++)  ReadPtrs[ChannelNo] = ReadPtrs[ChannelNo-1]+1;
        if ((PixelType==AVS_CS_YUY2) && (ChannelCount>2))  ReadPtrs[2]++;
      break;
      case AVS_CS_YV12:
      case AVS_CS_I420:
        ReadPtrs[0] = avs_get_read_ptr_p(VideoFrame,AVS_PLANAR_Y);
        if (ChannelCount>1)  ReadPtrs[1] = avs_get_read_ptr_p(VideoFrame,AVS_PLANAR_U);
        if (ChannelCount>2)  ReadPtrs[2] = avs_get_read_ptr_p(VideoFrame,AVS_PLANAR_V);
        for (ChannelNo=0 ; ChannelNo<ChannelCount ; ChannelNo++)  if (!ReadPtrs[ChannelNo])  return 0;
      break;
    }
  }
  if (WritePtrs)
  {
    if (!VideoFrame)  return  0;
    switch (PixelType)
    {
      case AVS_CS_BGR24:
      case AVS_CS_BGR32:
      case AVS_CS_YUY2:
        WritePtrs[0] = avs_get_write_ptr(VideoFrame);
        if (!WritePtrs[0])  return  0;
        for (ChannelNo=1 ; ChannelNo<ChannelCount ; ChannelNo++)  WritePtrs[ChannelNo] = WritePtrs[ChannelNo-1]+1;
        if ((PixelType==AVS_CS_YUY2) && (ChannelCount>2))  WritePtrs[2]++;
      break;
      case AVS_CS_YV12:
      case AVS_CS_I420:
        WritePtrs[0] = avs_get_write_ptr_p(VideoFrame,AVS_PLANAR_Y);
        if (ChannelCount>1)  WritePtrs[1] = avs_get_write_ptr_p(VideoFrame,AVS_PLANAR_U);
        if (ChannelCount>2)  WritePtrs[2] = avs_get_write_ptr_p(VideoFrame,AVS_PLANAR_V);
        for (ChannelNo=0 ; ChannelNo<ChannelCount ; ChannelNo++)  if (!WritePtrs[ChannelNo])  return 0;
      break;
    }
  }
  return  ChannelCount;
}



int  FastMarching(float  u[],  unsigned int *  Order,  int  Width,  int  Height,  double  PixelWidth,  double  PixelHeight)
{
  int * Close;  /* Position in Heap array, or 0, if not "close" */
  int  k, x, y, h, Error;
  FMMHeapStruct  Heap;


  /* Initialize close matrix */
  Close = (int*)calloc(Width*Height,sizeof(int));
  if (!Close)  return  0;
  for (k=0 ; k<Width*Height ; k++)  if (u[k]>(float)0.0)  u[k] = FMMFarMarker;  /* All non-positive values are fixed (boundary values for the eikonal equation) */
  /* Initialize heap struct */
  Heap.Delta = FMMHeapDelta;
  Heap.MaxSize = FMMHeapInitialSize;  /* Enough memory for slightly unregular domains */
  Heap.MemPointer = (int*)malloc(sizeof(int)*Heap.MaxSize);
  if (!Heap.MemPointer)
  {
    free(Close);
    return  0;
  }
  Heap.Size = 0;
  Heap.Heap = Heap.MemPointer-1;
  /* Initialize close region (around initial values) */
  Error = 0;
  for (k=0 ; k<Width*Height ; k++)
  {
    if (u[k]!=FMMFarMarker && !Close[k])
    {
      y = k/Width;
      x = k%Width;
      if (x>0)  Error |= !FMMUpdateCloseValue(u,Close,x-1,y,Width,Height,PixelWidth,PixelHeight,&Heap);
      if (y>0)  Error |= !FMMUpdateCloseValue(u,Close,x,y-1,Width,Height,PixelWidth,PixelHeight,&Heap);
      if (x<Width-1)  Error |= !FMMUpdateCloseValue(u,Close,x+1,y,Width,Height,PixelWidth,PixelHeight,&Heap);
      if (y<Height-1)  Error |= !FMMUpdateCloseValue(u,Close,x,y+1,Width,Height,PixelWidth,PixelHeight,&Heap);
    }
  }
  /* Main loop */
  while (!Error && Heap.Size)
  {
    /** Make lowest value alive **/
    k = Heap.Heap[1];
    if (Order)  *(Order++) = k;
    Close[k] = 0;  /* Lowest close value becomes alive */
    /* We transport the void value in Heap[1] down to the bottom of the heap */
    h = 1;
    while (2*h<=Heap.Size)
    {
      h *= 2;
      h += (h<Heap.Size)&&(u[Heap.Heap[h+1]]<u[Heap.Heap[h]]);
      Heap.Heap[h/2] = Heap.Heap[h];
      Close[Heap.Heap[h]] = h/2;
    }
    /* The void value is now at position h in the heap - depending on the new value (Heap[HeapSize]), we transport it up again */
    while ((h>1) && (u[Heap.Heap[Heap.Size]]<u[Heap.Heap[h/2]]))
    {
      Heap.Heap[h] = Heap.Heap[h/2];
      Close[Heap.Heap[h]] = h;
      h /= 2;
    }
    /* Now h denotes the new position of Heap[HeapSize] */
    if (h!=Heap.Size)
    {
      Heap.Heap[h] = Heap.Heap[Heap.Size];
      Close[Heap.Heap[h]] = h;
    }
    Heap.Size--;
    /** Make points around the new alive point close **/
    y = k/Width;
    x = k%Width;
    if (x>0)  Error |= !FMMUpdateCloseValue(u,Close,x-1,y,Width,Height,PixelWidth,PixelHeight,&Heap);
    if (y>0)  Error |= !FMMUpdateCloseValue(u,Close,x,y-1,Width,Height,PixelWidth,PixelHeight,&Heap);
    if (x<Width-1)  Error |= !FMMUpdateCloseValue(u,Close,x+1,y,Width,Height,PixelWidth,PixelHeight,&Heap);
    if (y<Height-1)  Error |= !FMMUpdateCloseValue(u,Close,x,y+1,Width,Height,PixelWidth,PixelHeight,&Heap);
  }
  free(Heap.MemPointer);
  free(Close);
  return  !Error;
}



int  FMMUpdateCloseValue(float  u[],  int  Close[],  int  x,  int  y,  int  Width,  int  Height,  double  PixelWidth,  double  PixelHeight,  FMMHeapStruct *  Heap)
{
  int  k, h;  /* k = Position in matrix,   h = position in heap */
  int  xm, xp, ym, yp;
  // for large (even FullHD) frame sizes 'float' is not enough
  // process sometimes does not converge
  double  p, uxm, uxp, uym, uyp, ux, uy;


  k = Width*y+x;  /* Position in arrays */
  if ((!Close[k]) && (u[k]!=FMMFarMarker))  return  1;  /* If not close and not far, it is alive, hence cannot be updated anymore */
  xm = k-1;  /* Pointers to neighbors */
  xp = k+1;
  ym = k-Width;
  yp = k+Width;
  uxm = ((x>0)        && !Close[xm])?u[xm]:FMMFarMarker;  /* Values of neighbors, or FarMarker */
  uxp = ((x<Width-1)  && !Close[xp])?u[xp]:FMMFarMarker;  /* (= very big value, which          */
  uym = ((y>0)        && !Close[ym])?u[ym]:FMMFarMarker;  /* ensures it isn't considered),     */
  uyp = ((y<Height-1) && !Close[yp])?u[yp]:FMMFarMarker;  /* if neighbors are close            */
  /* Formular (14) in Sethian-Paper */
  ux = (uxm<uxp)?uxm:uxp;
  uy = (uym<uyp)?uym:uyp;
  /* Solve   max((u[k]-ux)/PW,0)^2 + max((u[k]-uy)/PH,0)^2 = 1  */
  if (ux+PixelWidth<uy)
  {
    u[k] = ux+PixelWidth;
  }
  else  if (uy+PixelHeight<ux)
  {
    u[k] = uy+PixelHeight;
  }
  else
  {
    PixelWidth *= PixelWidth;
    PixelHeight *= PixelHeight;
    p = PixelWidth*uy+PixelHeight*ux;
    p /= PixelWidth+PixelHeight;
    u[k] = p+sqrt(p*p-(PixelWidth*uy*uy+PixelHeight*ux*ux-PixelWidth*PixelHeight)/(PixelWidth+PixelHeight));
  }
  h = Close[k];
  /* If it wasn't close, its new heap position is at the very end */
  if (!h)
  {
    Heap->Size++;
    if (Heap->Size>Heap->MaxSize)
    {
      /* Increase heap memory */
      Heap->MaxSize += Heap->Delta;
      Heap->Heap = realloc(Heap->MemPointer,sizeof(int)*Heap->MaxSize);
      if (!Heap->Heap)  return  0;
      Heap->MemPointer = Heap->Heap;
      Heap->Heap--;
    }
    h = Heap->Size;
  }
  /* Push upwards in the heap until heap structure is restored */
  while ((h>1) && (u[k]<u[Heap->Heap[h/2]]))
  {
    Heap->Heap[h] = Heap->Heap[h/2];
    Close[Heap->Heap[h]] = h;
    h /= 2;
  }
  /* Update new heap- and close-pointer */
  Heap->Heap[h] = k;
  Close[k] = h;
  return  1;
}



/****************************************************************
                         Inpaint tools
 ****************************************************************/
int  InpaintPrepareMask(InpaintMaskDataStruct **  MaskDataPtr,  AVS_VideoFrame *  Frame,  InpaintDataStruct *  InpaintData)
/* If mask is full or empty, MaskData is cleared */
{
  #define  Max(X,Y)  (((X)>(Y))?(X):(Y))


  InpaintMaskDataStruct * MaskData;
  double  PixelWidth, PixelHeight;
  float * DistFcn, * u, * Gchi;
  int  x, y, xmin, xmax, ymin, ymax, Width, Height, k;
  int  Stride, Pitch, FramePitch[MaxChannelCount];
  signed int  MaskSize;
  const  BYTE * FrameData[MaxChannelCount], * DataPtr;
  BYTE * chi;
  int  Result;
  register  int  MaskIsSet;


  /* Clear old mask data */
  MaskData = *MaskDataPtr;
  if (MaskData)
  {
    if (MaskData->GchiChroma)  free(MaskData->GchiChroma);
    if (MaskData->GchiLuma)  free(MaskData->GchiLuma);
    if (MaskData->MaskPixelOrder)  free(MaskData->MaskPixelOrder);
    if (MaskData->chiChroma)  free(MaskData->chiChroma);
    if (MaskData->chiLuma)  free(MaskData->chiLuma);
    *MaskDataPtr = 0;
  }
  if (!(Frame && GetFramePointers(Frame,0,FrameData,FramePitch,0,0,0,InpaintData->MaskPixelType) && FrameData[0]))
  {
    if (MaskData)  free(MaskData);
    return  Frame?InpaintMask_Result_FrameError:InpaintMask_Result_NoMask;
  }
  if (!MaskData)  MaskData = (InpaintMaskDataStruct*)malloc(sizeof(InpaintMaskDataStruct));
  if (!MaskData)  return  InpaintMask_Result_MemoryError;
  MaskData->chiLuma = 0;
  MaskData->GchiLuma = 0;
  MaskData->chiChroma = 0;
  MaskData->GchiChroma = 0;
  MaskData->MaskPixelOrder = 0;
  MaskData->MaskPixelCount = 0;
  MaskData->OffsetX = 0;
  MaskData->OffsetY = 0;
  MaskData->Width = Width = InpaintData->Width;
  MaskData->Height = Height = InpaintData->Height;
  DistFcn = (float*)malloc(sizeof(float)*Width*Height);
  if (!DistFcn)
  {
    free(MaskData);
    return  InpaintMask_Result_MemoryError;
  }
  Pitch = FramePitch[InpaintData->MaskChannelNo];
  Stride = InpaintData->MaskStride;
  MaskSize = 0;
  DataPtr = FrameData[InpaintData->MaskChannelNo];
  u = DistFcn;
  if (InpaintData->FlipMask)
  {
    Pitch *= -1;
    DataPtr -= (InpaintData->Height-1)*Pitch;
  }
  for (y=0 ; y<Height ; y++)
  {
    for (x=0 ; x<Width ; x++)
    {
      MaskSize += MaskIsSet = ((MaskThresholdSign)*(*DataPtr)>(MaskThresholdSign)*(MaskThreshold));
      *(u++) = MaskIsSet?0.0:1.0;
      DataPtr += Stride;
    }
    DataPtr += Pitch-Width*Stride;
  }
  if (!MaskSize || (MaskSize==Width*Height))
  {
    free(MaskData);
    free(DistFcn);
    return  (MaskSize)?InpaintMask_Result_FullMask:InpaintMask_Result_NoMask;
  }
  /* We create a halo around the inpainting mask. Outside of this halo nothing will happen! */
  /* These variables are not the size of a pixel, but used to rescale the aspect ratio of the halo */
  PixelWidth = InpaintData->ChromaPreBlurXKernelSize+Max(InpaintData->Chroma4LumaPostBlurXKernelSize,InpaintData->Chroma4ChromaPostBlurXKernelSize);
  PixelWidth += 1.5;
  PixelWidth *= InpaintData->ChromaPixelWidth;
  PixelWidth = Max(PixelWidth,1.5+InpaintData->LumaPreBlurXKernelSize+Max(InpaintData->Luma4LumaPostBlurXKernelSize,InpaintData->Luma4ChromaPostBlurXKernelSize));
  PixelWidth += InpaintData->ChromaPixelWidth;
  PixelWidth = Max(PixelWidth,2.0);
  PixelHeight = InpaintData->ChromaPreBlurYKernelSize+Max(InpaintData->Chroma4LumaPostBlurYKernelSize,InpaintData->Chroma4ChromaPostBlurYKernelSize);
  PixelHeight += 1.5;
  PixelHeight *= InpaintData->ChromaPixelHeight;
  PixelHeight = Max(PixelHeight,1.5+InpaintData->LumaPreBlurYKernelSize+Max(InpaintData->Luma4LumaPostBlurYKernelSize,InpaintData->Luma4ChromaPostBlurYKernelSize));
  PixelHeight += InpaintData->ChromaPixelHeight;
  PixelHeight = Max(PixelHeight,2.0);
  if (!FastMarching(DistFcn,0,Width,Height,1.0/PixelWidth,1.0/PixelHeight))
  {
    free(MaskData);
    free(DistFcn);
    return  InpaintMask_Result_MemoryError;
  }
  u = DistFcn;
  for (k=0 ; k<Width*Height ; k++)  u[k] = (u[k]<(float)1.1)?(float)1.0:(float)0.0;  /* Determines the halo size */
  xmin = Width-1;
  xmax = 0;
  ymin = Height-1;
  ymax = 0;
  u = DistFcn;
  for (y=0 ; y<Height ; y++)
  {
    for (x=0 ; x<Width ; x++)
    {
      if ((*(u++))!=0.0)
      {
        if (x<xmin)  xmin = x;
        if (x>xmax)  xmax = x;
        if (y<ymin)  ymin = y;
        if (y>ymax)  ymax = y;
      }
    }
  }
  /* Current state is stored in Result */
  Result = InpaintMask_Result_OK;
  if ((xmin>xmax) || (ymin>ymax))  Result = InpaintMask_Result_GeneralError;
  if (Result==InpaintMask_Result_OK)
  {
    /* xymin is first pixel inside the processing area
       xymax is last pixel inside the processing area */
    xmin /= InpaintData->ChromaPixelWidth;
    xmin *= InpaintData->ChromaPixelWidth;
    xmax /= InpaintData->ChromaPixelWidth;
    xmax++;
    xmax *= InpaintData->ChromaPixelWidth;
    ymin /= InpaintData->ChromaPixelHeight;
    ymin *= InpaintData->ChromaPixelHeight;
    ymax /= InpaintData->ChromaPixelHeight;
    ymax++;
    ymax *= InpaintData->ChromaPixelHeight;
    /* xymin is first pixel inside the processing area
       xymax is first pixel outside the processing area */
    MaskData->OffsetX = xmin;
    MaskData->OffsetY = ymin;
    MaskData->Width = Width = xmax-xmin;
    MaskData->Height = Height = ymax-ymin;
    MaskData->chiLuma = (BYTE*)malloc(sizeof(BYTE)*Width*Height);
    if (!MaskData->chiLuma)  Result = InpaintMask_Result_MemoryError;
    if (InpaintData->ChromaChannelCount)
    {
      MaskData->chiChroma = (BYTE*)calloc((Width/InpaintData->ChromaPixelWidth)*(Height/InpaintData->ChromaPixelHeight),sizeof(BYTE));
      if (!MaskData->chiChroma)  Result = InpaintMask_Result_MemoryError;
    }
    if (MaskSize==Width*Height)  Result = InpaintMask_Result_GeneralError;
  }
  if (Result==InpaintMask_Result_OK)
  {
    /* The halo ist still stored in DistFcn */
    u = DistFcn+MaskData->OffsetY*InpaintData->Width+MaskData->OffsetX;
    chi = MaskData->chiLuma;
    /* Transfer the halo to chi: chi=0 for inpainting domain or halo, chi=1 in far-away-region
       chi should also be 1 in the inpainting domain - we'll set that later */
    for (y=0 ; y<Height ; y++)
    {
      for (x=0 ; x<Width ; x++)  *(chi++) = ((*(u++))==0.0);
      u += InpaintData->Width-Width;
    }
    /* Save MaskSize, then MaskSize will be decreased for each pixel set. If it doesn't vanish after processing the mask again, an internal error occured */
    MaskData->MaskPixelCount = MaskSize;
    DataPtr = FrameData[InpaintData->MaskChannelNo]+MaskData->OffsetY*Pitch+MaskData->OffsetX*Stride;
    u = DistFcn;
    chi = MaskData->chiLuma;
    if (InpaintData->FlipMask)  DataPtr -= (InpaintData->Height-1)*Pitch;  /* Pitch is negative already ! */
    /* We set chi=1 in the inpainting domain */
    for (y=0 ; y<Height ; y++)
    {
      for (x=0 ; x<Width ; x++)
      {
        MaskSize -= MaskIsSet = ((MaskThresholdSign)*(*DataPtr)>(MaskThresholdSign)*(MaskThreshold));
        chi[0] = chi[0] || MaskIsSet;
        chi++;
        *(u++) = MaskIsSet?1.0:0.0;
        DataPtr += Stride;
      }
      DataPtr += Pitch-Width*Stride;
    }
    if (MaskSize)  Result = InpaintMask_Result_GeneralError;
  }
  if (Result==InpaintMask_Result_OK)
  {
    if (MaskData->chiChroma)
    {
      chi = MaskData->chiLuma;
      for (y=0 ; y<Height ; y++)
      {
        for (x=0 ; x<Width ; x++)
        {
          MaskData->chiChroma[(y/InpaintData->ChromaPixelHeight)*(Width/InpaintData->ChromaPixelWidth)+x/InpaintData->ChromaPixelWidth] += *(chi++);
        }
      }
    }
    MaskData->MaskPixelOrder = (unsigned int*)malloc(sizeof(unsigned int)*MaskData->MaskPixelCount);
    /* We call FastMarching to determine the inpainting pixel order */
    if (!(MaskData->MaskPixelOrder && FastMarching(DistFcn,MaskData->MaskPixelOrder,Width,Height,InpaintData->PixelWidth,InpaintData->PixelHeight)))  Result = InpaintMask_Result_MemoryError;
  }
  if (DistFcn)  free(DistFcn);
  if (Result==InpaintMask_Result_OK)
  {
    if ((InpaintData->MaxSteps>=0)&&(InpaintData->MaxSteps<MaskData->MaskPixelCount))
    {
      MaskData->MaskPixelCount = InpaintData->MaxSteps;
      MaskData->MaskPixelOrder = (unsigned int*)realloc(MaskData->MaskPixelOrder,sizeof(unsigned int)*MaskData->MaskPixelCount);
    }
    if (InpaintData->LumaWeight!=0.0)
    {
      MaskData->GchiLuma = (float*)malloc(sizeof(float)*Width*Height);
      if (!MaskData->GchiLuma)  Result = InpaintMask_Result_MemoryError;
    }
    if (InpaintData->ChromaWeight!=0.0)
    {
      MaskData->GchiChroma = (float*)malloc(sizeof(float)*(Width/InpaintData->ChromaPixelWidth)*(Height/InpaintData->ChromaPixelHeight));
      if (!MaskData->GchiChroma)  Result = InpaintMask_Result_MemoryError;
    }
  }
  if (Result==InpaintMask_Result_OK)
  {
    if (MaskData->GchiLuma)
    {
      Gchi = MaskData->GchiLuma;
      for (k=0 ; k<Width*Height ; k++)  *(Gchi++) = 0.0;
      chi = MaskData->chiLuma;
      Gchi = MaskData->GchiLuma;
      for (k=0 ; k<Width*Height ; k++)  if (!chi[k])  AddPreBlurValue(Gchi,k%Width,k/Width,Width,Height,1.0,InpaintData->LumaPreBlurXKernel,InpaintData->LumaPreBlurXKernelSize,InpaintData->LumaPreBlurYKernel,InpaintData->LumaPreBlurYKernelSize);
    }
    if (MaskData->GchiChroma)
    {
      Gchi = MaskData->GchiChroma;
      for (k=0 ; k<(Width/InpaintData->ChromaPixelWidth)*(Height/InpaintData->ChromaPixelHeight) ; k++)  *(Gchi++) = 0.0;
      chi = MaskData->chiChroma;
      Gchi = MaskData->GchiChroma;
      for (k=0 ; k<(Width/InpaintData->ChromaPixelWidth)*(Height/InpaintData->ChromaPixelHeight) ; k++)  if (!chi[k])  AddPreBlurValue(Gchi,k%(Width/InpaintData->ChromaPixelWidth),k/(Width/InpaintData->ChromaPixelWidth),Width/InpaintData->ChromaPixelWidth,Height/InpaintData->ChromaPixelHeight,1.0,InpaintData->ChromaPreBlurXKernel,InpaintData->ChromaPreBlurXKernelSize,InpaintData->ChromaPreBlurYKernel,InpaintData->ChromaPreBlurYKernelSize);
    }
  }
  if (Result!=InpaintMask_Result_OK)  InpaintPrepareMask(&MaskData,0,0);
  *MaskDataPtr = MaskData;
  return  Result;


  #undef  Max
}



void  AddPreBlurValue(float  G[],  int  x,  int  y,  int  Width,  int  Height,  double  Value,  double  BlurXKernel[],  int  BlurXKernelSize,  double  BlurYKernel[],  int  BlurYKernelSize)
{
  int  xmin, xmax, ymin, ymax, x0, y0;


  xmin = -((BlurXKernelSize<x)?BlurXKernelSize:x);
  xmax = (BlurXKernelSize<(Width-1)-x)?(BlurXKernelSize):((Width-1)-x);
  ymin = -((BlurYKernelSize<y)?BlurYKernelSize:y);
  ymax = (BlurYKernelSize<(Height-1)-y)?(BlurYKernelSize):((Height-1)-y);
  /* Time-critical loop !! */
  for (y0=ymin ; y0<=ymax ; y0++)
   for (x0=xmin ; x0<=xmax ; x0++)
   G[Width*(y+y0)+x+x0] += Value*(double)BlurXKernel[x0]*(double)BlurYKernel[y0];
}



double  InpaintPixel(double  RadiusX,  double  RadiusY,  BYTE  u[],  BYTE  chi[],  int  Stride,  int  Pitch,  int  x,  int  y,  int  Width,  int  Height,  double  CoherenceStrength,  double  CoherenceDirectionX,  double  CoherenceDirectionY)
{
  int  xmin, xmax, ymin, ymax, x0, y0, dx, dy, dx2, dy2;
  double  u0, w, w0, Radius;


  u0 = 0;
  w0 = 0;
  xmin = (x>RadiusX)?ceil(x-RadiusX):0;
  ymin = (y>RadiusY)?ceil(y-RadiusY):0;
  xmax = floor(x+RadiusX);  if (xmax>=Width)  xmax = Width-1;
  ymax = floor(y+RadiusY);  if (ymax>=Height)  ymax = Height-1;
  if (CoherenceStrength>0.0)
  {
    CoherenceDirectionX *= CoherenceStrength/sqrt(2.0*RadiusX*RadiusY);
    CoherenceDirectionY *= CoherenceStrength/sqrt(2.0*RadiusX*RadiusY);
    CoherenceStrength *= sqrt(2.0*atan(1.0));
    RadiusX *= RadiusX;
    RadiusY *= RadiusY;
    Radius = RadiusX*RadiusY;
    /* Time-critical loop !! */
    for (y0=ymin ; y0<=ymax ; y0++)
    {
      dy = y0-y;
      dy2 = dy*dy;
      for (x0=xmin ; x0<=xmax ; x0++)
      {
        if (chi[Width*y0+x0])  continue;
        dx = x0-x;
        dx2 = dx*dx;
        if (dx2*RadiusY+dy2*RadiusX>Radius)  continue;
        /* Formular (11) of Bornemann/Maerz goes here */
        w = CoherenceDirectionY*dx-CoherenceDirectionX*dy;
        w = CoherenceStrength/sqrt(dx2+dy2)*exp(-w*w);
        u0 += w*u[Pitch*y0+x0*Stride];
        w0 += w;
      }
    }
  }
  if (w0==0.0)
  {
    RadiusX *= RadiusX;
    RadiusY *= RadiusY;
    Radius = RadiusX*RadiusY;
    for (y0=ymin ; y0<=ymax ; y0++)
    {
      dy = y0-y;
      dy2 = dy*dy;
      for (x0=xmin ; x0<=xmax ; x0++)
      {
        if (chi[Width*y0+x0])  continue;
        dx = x0-x;
        dx2 = dx*dx;
        if (dx2*RadiusY+dy2*RadiusX>Radius)  continue;
        /* No coherence ==> (11) collapses to 1/|x-y| */
        w = 1.0/sqrt(dx2+dy2);
        u0 += w*u[Pitch*y0+x0*Stride];
        w0 += w;
      }
    }
  }
  if (w0==0.0)  return  -1.0;
  u0 /= w0;
  if (u0<0.0)  u0 = 0.0;  else  if (u0>255.0)  u0 = 255.0;  /* Just to be sure - maybe this is not necessary */
  return  u0;
}



double  GetCoherenceData(double *  CoherenceDirectionX,  double *  CoherenceDirectionY,  double  TensorXX,  double  TensorXY,  double  TensorYY,  double  Sharpness)
/* Solves the eigenvalue problem of formular (22) and computes formular (20) in Bornemann/Maerz */
{
  register  double  a;  /* Some temporary variables */
  register  double  b;
  register  double  c;


  a = TensorXX+TensorYY;  /* trace */
  if (a==0.0)  return  0.0;
  c = TensorXY*TensorXY;
  b = TensorXX*TensorYY-c;  /* determinant */
  a += sqrt(a*a-4.0*b);
  a /= 2.0;  /* bigger eigenvalue */
  b /= a;  /* smaller eigenvalue */
  a -= b;  /* difference between eigenvalues */
  if (a==0.0)  return  0.0;
  a = Sharpness*exp(-1.0/(a*a));
  if (a==0.0)  return  0.0;
  if ((!(TensorXX+TensorYY<2*b)) ^ (!(TensorXX<TensorYY)))
  {
    b -= TensorYY;
    c += b*b;  /* Squared norm of eigenvector */
    if (c==0.0)  return  0.0;
    c = sqrt(c);  /* Norm of eigenvector */
    *CoherenceDirectionX = b/c;  /* Normalize eigenvector for output */
    *CoherenceDirectionY = TensorXY/c;
  }
  else
  {
    b -= TensorXX;
    c += b*b;  /* Squared norm of eigenvector */
    if (c==0.0)  return  0.0;
    c = sqrt(c);  /* Norm of eigenvector */
    *CoherenceDirectionX = TensorXY/c;  /* Normalize eigenvector for output */
    *CoherenceDirectionY = b/c;
  }
  return  a+1.0;
}


  
void  GetStructureTensor(double *  TensorXX,  double *  TensorXY,  double *  TensorYY,
                         float *  GchiLuma,  float *  GchiuLuma,  BYTE *  chiLuma,  double  LumaWeight,
                         float *  GchiChroma,  float **  GchiuChromas,  BYTE *  chiChroma,  double  ChromaWeight,
                         int  ChromaPixelWidth,  int  ChromaPixelHeight,  int  ChromaChannelCount,
                         int  x,  int  y,  int  Width,  int  Height,
                         double *  LumaBlurXKernel,  int  LumaBlurXKernelSize,  int  LumaXDirection,
                         double *  LumaBlurYKernel,  int  LumaBlurYKernelSize,  int  LumaYDirection,
                         double *  ChromaBlurXKernel,  int  ChromaBlurXKernelSize,  int  ChromaXDirection,
                         double *  ChromaBlurYKernel,  int  ChromaBlurYKernelSize,  int  ChromaYDirection)
/*
 If a weight is !=0.0, then all corresponding pointers must be valid!
 Direction>0 : Forward difference    Direction<0 : Backward difference
*/
{
  double  xx, xy, yy, Weight;
  int  ChannelNo;


  *TensorXX = *TensorXY = *TensorYY = Weight = 0.0;
  if (ChromaWeight!=0.0)
  {
    for (ChannelNo=0 ; ChannelNo<ChromaChannelCount ; ChannelNo++)
    {
      if (GetChannelTensor(&xx,&xy,&yy,GchiChroma,GchiuChromas[ChannelNo],chiChroma,x/ChromaPixelWidth-(ChromaXDirection<0),y/ChromaPixelHeight-(ChromaYDirection<0),Width/ChromaPixelWidth,Height/ChromaPixelHeight,ChromaBlurXKernel,ChromaBlurXKernelSize,ChromaBlurYKernel,ChromaBlurYKernelSize))
      {
        *TensorXX += xx;
        *TensorXY += xy;
        *TensorYY += yy;
        Weight += 1.0;
      }
    }
    *TensorXX *= ChromaWeight/ChromaPixelWidth/ChromaPixelWidth;
    *TensorXY *= ChromaWeight/ChromaPixelWidth/ChromaPixelHeight;
    *TensorYY *= ChromaWeight/ChromaPixelHeight/ChromaPixelHeight;
    Weight *= ChromaWeight;
  }
  if (LumaWeight!=0.0)
  {
    if (GetChannelTensor(&xx,&xy,&yy,GchiLuma,GchiuLuma,chiLuma,x-(LumaXDirection<0),y-(LumaYDirection<0),Width,Height,LumaBlurXKernel,LumaBlurXKernelSize,LumaBlurYKernel,LumaBlurYKernelSize))
    {
      *TensorXX += xx*LumaWeight;
      *TensorXY += xy*LumaWeight;
      *TensorYY += yy*LumaWeight;
      Weight += LumaWeight;
    }
  }
  if (Weight>0.0)
  {
    *TensorXX /= Weight;
    *TensorXY /= Weight;
    *TensorYY /= Weight;
  }
}



int  GetChannelTensor(double *  TensorXX,  double *  TensorXY,  double *  TensorYY,  float  Gchi[],  float  Gchiu[],  BYTE  chi[],  int  x,  int  y,  int  Width,  int  Height,  double  BlurKernelX[],  int  BlurKernelXSize,  double  BlurKernelY[],  int  BlurKernelYSize)
/* This (fairly) realized formula (21) of Bornemann/März-Paper */
/* This function uses forward differences. Anything else must be done by providing an appropriate kernel and by modifying x,y */
{
  double  xx, xy, yy, chi0;
  register  double  dx, dy, Blur;
  int  xmin, ymin, xmax, ymax, x0, y0;
  register  float  chiC, chiS, chiE;


  chi0 = xx = xy = yy = 0;
  xmin = x;           if (BlurKernelXSize<xmin)  xmin = BlurKernelXSize;  xmin *= -1;
  xmax = Width-x-2;   if (BlurKernelXSize<xmax)  xmax = BlurKernelXSize;
  ymin = y;           if (BlurKernelYSize<ymin)  ymin = BlurKernelYSize;  ymin *= -1;
  ymax = Height-y-2;  if (BlurKernelYSize<ymax)  ymax = BlurKernelYSize;
  /* Time-critical loop !! */
  for (y0=ymin ; y0<=ymax ; y0++)
  {
    for (x0=xmin ; x0<=xmax ; x0++)
    {
      if (chi[Width*(y+y0)+x+x0])  continue;  /* This line is due to the 1I_O(x) factor in the numerator of J_s,r(x) in formula (21) */
      chiC = Gchi[Width*(y+y0  )+x+x0  ];  /*   0----------     These lines fetch K_s */
      chiE = Gchi[Width*(y+y0  )+x+x0+1];  /*   |   C E                               */
      chiS = Gchi[Width*(y+y0+1)+x+x0  ];  /*   |   S                                 */
      if ((chiC==0.0) || (chiE==0.0) || (chiS==0.0))  continue;
      dx = dy = (double)Gchiu[Width*(y+y0)+x+x0]/(double)chiC;  /* This is v_s(x) */
      dx -= (double)Gchiu[Width*(y+y0)+x+x0+1]/(double)chiE;  /* This subtracts v_x(x+h*e1) */
      dy -= (double)Gchiu[Width*(y+y0+1)+x+x0]/(double)chiS;  /* This subtracts v_x(x+h*e2) */
      chi0 += Blur = (double)BlurKernelX[x0]*(double)BlurKernelY[y0];  /* This is K_r */
      xx += dx*dx*Blur;
      xy += dx*dy*Blur;
      yy += dy*dy*Blur;
    }
  }
  if (chi0==0.0)  return  0;
  *TensorXX = xx/chi0;
  *TensorXY = xy/chi0;
  *TensorYY = yy/chi0;
  return  1;
}



double *  CreateGaussKernel(double  Kernel[],  int  Size,  double  Deviation,  double  Center)
{
  signed int  k;


  Kernel += Size;
  if (Deviation>sqrt(4*sqrt(256.0*DBL_EPSILON/2.0*4.0*atan(1.0)*exp(1.0))))  /* 4 for safety */
  {
    Deviation = -0.5/(Deviation*Deviation);
    for (k=-Size ; k<=Size ; k++)  Kernel[k] = exp(Deviation*(Center-k)*(Center-k));
  }
  else
  {
    // FIXME: check if integer abs is appropriate here, probably yes
    for (k=-Size ; k<=Size ; k++)  Kernel[k] = (abs(Center-k)<1.0)?(1.0-abs(Center-k)):0.0;
  }
  if (!Size)  Kernel[0] = 1.0;
  return  Kernel;
}



/****************************************************************
                        Deblend tools
 ****************************************************************/
void  DeblendDynamic(BYTE  Data[],  const  BYTE  LogoData[],  const  BYTE  AlphaData[],  int  Pitch,  int  Stride,  int  LogoPitch,  int  LogoStride,  int  AlphaPitch,  int  AlphaStride,  int  Width,  int  Height)
/* Performs a simple deblend with given logo and alpha data         */
{
  int  x;
  double  Value;


  AlphaPitch -= AlphaStride*Width;
  LogoPitch -= LogoStride*Width;
  Pitch -= Stride*Width;
  while (Height--)
  {
    x = Width;
    while (x--)
    {
      if (0xFF-AlphaData[0])
      {
        Value = 0xFF*((int)(Data[0]))-((int)(AlphaData[0]))*((int)(LogoData[0]));
        Value /= 0xFF-AlphaData[0];
        if (Value<0.0)  Value = 0.0;  else  if (Value>255.0)  Value = 255.0;
        Data[0] = round(Value);
      }
      AlphaData += AlphaStride;
      LogoData += LogoStride;
      Data += Stride;
    }
    AlphaData += AlphaPitch;
    LogoData += LogoPitch;
    Data += Pitch;
  }
}



void  DeblendDynamicSubsampled(BYTE  Data[],  const  BYTE  LogoData[],  const  BYTE  AlphaData[],  int  Pitch,  int  Stride,  int  LogoPitch,  int  LogoStride,  int  AlphaPitch,  int  AlphaStride,  int  Width,  int  Height,  int  PixelWidth,  int  PixelHeight)
/* Performs a simple deblend with given logo and alpha data, where
   the source and logo data are subsampled with respect to the
   alpha data.                                                      */
{
  int  x, Alpha, TotalAlpha, xAlpha, yAlpha, AlphaBigPitch;
  double  Value;


  AlphaBigPitch = AlphaPitch*PixelHeight-AlphaStride*Width;
  TotalAlpha = PixelWidth*PixelHeight*0xFF;
  Width /= PixelWidth;
  Height /= PixelHeight;
  LogoPitch -= LogoStride*Width;
  Pitch -= Stride*Width;
  while (Height--)
  {
    x = Width;
    while (x--)
    {
      Alpha = 0;
      xAlpha = PixelWidth;
      while (xAlpha--)
      {
        for (yAlpha=0 ; yAlpha<PixelHeight ; yAlpha++)  Alpha += AlphaData[AlphaPitch*yAlpha];
        AlphaData += AlphaStride;
      }
      if (TotalAlpha-Alpha)
      {
        Value = TotalAlpha*((int)(Data[0]))-((int)Alpha)*((int)(LogoData[0]));
        Value /= TotalAlpha-Alpha;
        if (Value<0.0)  Value = 0.0;  else  if (Value>255.0)  Value = 255.0;
        Data[0] = round(Value);
      }
      LogoData += LogoStride;
      Data += Stride;
    }
    AlphaData += AlphaBigPitch;
    LogoData += LogoPitch;
    Data += Pitch;
  }
}



void  DeblendStatic(BYTE  Data[],  const  DoubleByte  LogoData[],  const  BYTE  AlphaData[],  int  Pitch,  int  Stride,  int  Width,  int  Height)
/* This deblends a logo with preprocessed data (by
   DeblendStaticPrepare)                                            */
{
  double  Value;
  int  W;


  Pitch -= Stride*Width;
  while (Height--)
  {
    W = Width;
    while (W--)
    {
      if (AlphaData[0])
      {
        Value = (255*((int)(Data[0]))-256*((int)(LogoData[0].H))+((int)(LogoData[0].L)));
        Value /= AlphaData[0];
        if (Value<0.0)  Value = 0.0;  else  if (Value>255.0)  Value = 255.0;
        Data[0] = round(Value);
      }
      AlphaData++;
      LogoData++;
      Data += Stride;
    }
    Data += Pitch;
  }
}



int  DeblendStaticPrepare(DoubleByte *  LogoDataChannels[],  BYTE *  AlphaDataChannels[],  AVS_VideoFrame *  LogoFrame,  AVS_VideoFrame *  AlphaFrame,  const  AVS_VideoInfo *  LogoVideoInfo,  const  AVS_VideoInfo *  AlphaVideoInfo)
/* If the logo to be deblended is static (not moving), some values
   can be prepared prior to deblending. Those values will be
   stored in the LogoData/AlphaData vectors, which are later
   handed over to the static-deblend-function                       */
{
  const  BYTE * LogoData[MaxChannelCount], * AlphaData[MaxChannelCount], * SrcData, * AlphaData0;
  BYTE * AlphaDestData;
  DoubleByte * DestData;
  int  LogoPitch[MaxChannelCount], LogoStride[MaxChannelCount], LogoPixelWidth[MaxChannelCount], LogoPixelHeight[MaxChannelCount], AlphaPitch[MaxChannelCount], AlphaStride[MaxChannelCount], AlphaPitch0, AlphaStride0;
  int  ChannelNo, ChannelCount, Width, Height, x, y, x2, y2, Alpha, Logo;


  Width = LogoVideoInfo->width;
  Height = LogoVideoInfo->height;
  ChannelCount = GetFramePointers(LogoFrame,0,LogoData,LogoPitch,LogoStride,LogoPixelWidth,LogoPixelHeight,LogoVideoInfo->pixel_type);
  if ((!ChannelCount) || ((ChannelCount<4) && (!AlphaFrame)))  return  -1;  /* If LogoFrame is not RGBA, AlphaFrame must be supplied! */
  if (AlphaFrame)
  {
    if (!GetFramePointers(AlphaFrame,0,AlphaData,AlphaPitch,AlphaStride,0,0,AlphaVideoInfo->pixel_type))  return  -1;
    AlphaData0 = AlphaData[0];
    AlphaPitch0 = AlphaPitch[0];
    AlphaStride0 = AlphaStride[0];
    if (avs_is_rgb32(AlphaVideoInfo))  AlphaData0 += 3;
    if (!avs_is_rgb(LogoVideoInfo) ^ !avs_is_rgb32(AlphaVideoInfo))
    {
      AlphaData0 += (Height-1)*AlphaPitch0;
      AlphaPitch0 *= -1;
    }
  }
  else
  {
    ChannelCount--;
    AlphaData0 = LogoData[ChannelCount];
    AlphaPitch0 = LogoPitch[ChannelCount];
    AlphaStride0 = LogoStride[ChannelCount];
  }
  for (ChannelNo=0 ; ChannelNo<ChannelCount ; ChannelNo++)
  {
    DestData = LogoDataChannels[ChannelNo];
    if (!DestData)  continue;
    SrcData = LogoData[ChannelNo];
    AlphaDestData = AlphaDataChannels[ChannelNo];
    for (y=0 ; y<Height/LogoPixelHeight[ChannelNo] ; y++)
    {
      for (x=0 ; x<Width/LogoPixelWidth[ChannelNo] ; x++)
      {
        Alpha = 0;
        for (y2=0 ; y2<LogoPixelHeight[ChannelNo] ; y2++)
        {
          for (x2=0 ; x2<LogoPixelWidth[ChannelNo] ; x2++)
          {
            Alpha += AlphaData0[AlphaPitch0*(LogoPixelHeight[ChannelNo]*y+y2)+(x2+x*LogoPixelWidth[ChannelNo])*AlphaStride0];
          }
        }
        Alpha = round(1.0*Alpha/LogoPixelHeight[ChannelNo]/LogoPixelWidth[ChannelNo]);
        Logo = Alpha*SrcData[LogoPitch[ChannelNo]*y+x*LogoStride[ChannelNo]];
        if (AlphaDestData)  AlphaDestData[Width/LogoPixelWidth[ChannelNo]*y+x] = 0xFF-Alpha;
        DestData[Width/LogoPixelWidth[ChannelNo]*y+x].L = Logo%(0xFF);
        DestData[Width/LogoPixelWidth[ChannelNo]*y+x].H = Logo/(0xFF);
      }
    }
  }
  return  0;
}



/****************************************************************
                      Analyze tools
 ****************************************************************/
signed long  Analyze(AVS_VideoFrame *  DestFrame,  AVS_VideoInfo * DestVideoInfo,  AVS_Clip *  SrcClip,  BYTE  Mask[],  long  MaskSize,  int  ComputeAlpha,  double  DeviationWeight,  double  SubsamplingWeight)
/* This function evaluate SrcClip and estimates the logo */
{
  #define  Sum1MaxDelta  ((unsigned int)(0x80))
  #define  Sum2MaxDelta  (Sum1MaxDelta*Sum1MaxDelta)
  #define  KahanAdditionMakro(S,A,C,T,Y)  {Y=(A);Y-=C;T=S+Y;C=T-S;C-=Y;S=T;}  /* Kahan's summation algorithm reduces numerical problems - hopefully compiler optimization won't destroy this */

  /* Variables storing general stuff about source clip and destination frame */
  const  AVS_VideoInfo * SrcVideoInfo;
  int  Width, Height;
  int  PixelWidth[MaxChannelCount], PixelHeight[MaxChannelCount], FrameStride[MaxChannelCount];
  Analyze_Sum1Type * Sum1[MaxChannelCount];
  Analyze_Sum2Type * Sum2[MaxChannelCount];
  register  Analyze_Sum1Type * Sum1Ptr;
  register  Analyze_Sum2Type * Sum2Ptr;
  int  x, y, ChannelNo, ChannelCount;
  unsigned int  FrameCount;
  long  k;
  double  ChannelNMVariance[MaxChannelCount], ChannelNMAverage[MaxChannelCount];
  int  Result;
  int  FramePitch[MaxChannelCount];
  /* Variables for Phase 1 */
  AVS_VideoFrame * SrcFrame;
  unsigned int  FrameNo, FramesLeft;
  Analyze_Sum1Type  Sum1Max;
  Analyze_Sum2Type  Sum2Max;
  register  signed int  Value12;
  const  BYTE * SrcData[MaxChannelCount];
  register  const  BYTE * SrcPtr;
  int  SrcPtrDelta, DividedWidth, SrcStride;
  /* Variables for Phase 2 */
  BYTE * ChannelMask, * ChannelMaskVector;
  Analyze_Sum1Type * NMSum1;
  Analyze_Sum2Type * NMSum2;
  double  ChannelNMSum1[MaxChannelCount], ChannelNMSum2[MaxChannelCount], KahanSum, KahanC, KahanY, KahanT;
  int  ChannelMaskSize;
  /* Variables for Phase 3 */
  float * AlphaChannel, * ChannelAlpha, * ChannelAlphaVector;
  double  PixelValue, AlphaSum, AlphaWeightSum, AlphaWeight[MaxChannelCount];
  BYTE * DestData[MaxChannelCount];
  BYTE * AlphaData, * DestPtr;
  int  AlphaPitch, AlphaStride;


  /*** PHASE 0 :  Allocating memory ***/
  /* First check some assertions and store basic video parameters */
  if (!(SrcClip && DestFrame))  return  Analyze_Result_GeneralError;
  SrcVideoInfo = avs_get_video_info(SrcClip);
  if (!SrcVideoInfo)  return  Analyze_Result_SrcClipError;
  if (SrcVideoInfo->pixel_type!=DestVideoInfo->pixel_type)  return  Analyze_Result_GeneralError;
  Width = SrcVideoInfo->width;
  Height = SrcVideoInfo->height;
  FrameCount = SrcVideoInfo->num_frames;
  for (ChannelNo=0 ; ChannelNo<MaxChannelCount ; ChannelNo++)  Sum1[ChannelNo] = 0;
  for (ChannelNo=0 ; ChannelNo<MaxChannelCount ; ChannelNo++)  Sum2[ChannelNo] = 0;
  ChannelCount = GetFramePointers(0,0,0,0,FrameStride,PixelWidth,PixelHeight,SrcVideoInfo->pixel_type)-(DestVideoInfo->height>SrcVideoInfo->height?0:1);
  /* Allocate memory for sum and square-sum vectors (depending on color space) */
  if (ChannelCount<=0)  return  Analyze_Result_GeneralError;  /* Unknown color space (a bug!) */
  for (ChannelNo=0 ; ChannelNo<ChannelCount ; ChannelNo++)  Sum1[ChannelNo] = (Analyze_Sum1Type*)malloc(sizeof(Analyze_Sum1Type)*Width/PixelWidth[ChannelNo]*Height/PixelHeight[ChannelNo]);
  if (ComputeAlpha)  for (ChannelNo=0 ; ChannelNo<ChannelCount ; ChannelNo++)  Sum2[ChannelNo] = (Analyze_Sum2Type*)malloc(sizeof(Analyze_Sum2Type)*Width/PixelWidth[ChannelNo]*Height/PixelHeight[ChannelNo]);
  ChannelNo = 0;
  while (ChannelNo<ChannelCount)
  {
    if (!Sum1[ChannelNo])  break;
    if (ComputeAlpha && !Sum2[ChannelNo])  break;
    ChannelNo++;
  }
  if (ChannelNo<ChannelCount)
  {
    for (ChannelNo=0 ; ChannelNo<MaxChannelCount ; ChannelNo++)  if (Sum1[ChannelNo])  free(Sum1[ChannelNo]);
    for (ChannelNo=0 ; ChannelNo<MaxChannelCount ; ChannelNo++)  if (Sum2[ChannelNo])  free(Sum2[ChannelNo]);
    return  Analyze_Result_MemoryError;
  }
  /*** PHASE 1: Summing up (over time) all pixel values and their squares (Dear user: this will take some time!!) ***/
  /* Initialize main loop */
  for (ChannelNo=0 ; ChannelNo<ChannelCount ; ChannelNo++)
  {
    Sum1Ptr = Sum1[ChannelNo];
    if (Sum1Ptr)  for (k=0 ; k<Width/PixelWidth[ChannelNo]*Height/PixelHeight[ChannelNo] ; k++)  Sum1Ptr[k] = 0;
    Sum2Ptr = Sum2[ChannelNo];
    if (Sum2Ptr)  for (k=0 ; k<Width/PixelWidth[ChannelNo]*Height/PixelHeight[ChannelNo] ; k++)  Sum2Ptr[k] = 0;
  }
  SrcFrame = 0;
  FramesLeft = FrameCount;
  if (ComputeAlpha)  if (FramesLeft>Analyze_Sum2Max/Sum2MaxDelta)  FramesLeft = Analyze_Sum2Max/Sum2MaxDelta;
  if (FramesLeft>Analyze_Sum1Max/Sum1MaxDelta)  FramesLeft = Analyze_Sum1Max/Sum1MaxDelta;
  FrameNo = 0;
  Result = Analyze_Result_OK;
  /** MAIN LOOP **/
  /* This loops over all frames of the source clip */
  while (FramesLeft)
  {
    /* Get source frame... */
    SrcFrame = avs_get_frame(SrcClip,FrameNo);
    /* ...and data pointers... */
    if (!GetFramePointers(SrcFrame,0,SrcData,FramePitch,0,0,0,SrcVideoInfo->pixel_type))
    {
      /* ...and quit loop, if problem occured */
      Result = Analyze_Result_SrcClipError;
      break;
    }
    /* Loop over all color channels */
    for (ChannelNo=0 ; ChannelNo<ChannelCount ; ChannelNo++)
    {
      Sum1Ptr = Sum1[ChannelNo];
      Sum2Ptr = Sum2[ChannelNo];
      SrcPtr = SrcData[ChannelNo];
      SrcStride = FrameStride[ChannelNo];
      DividedWidth = Width/PixelWidth[ChannelNo];
      SrcPtrDelta = FramePitch[ChannelNo]-DividedWidth*SrcStride;
      /* This is the cirtical loop: Here all color values of all source frames are added up. Fast code will be rewarded with fast computation. */
      y = Height/PixelHeight[ChannelNo];
      if (ComputeAlpha)
      {
        while (y--)
        {
          x = DividedWidth;
          while (x--)
          {
            *(Sum1Ptr++) += Value12 = ((signed int)SrcPtr[0])-((signed int)0x80);  /* Shift all values in the range [-128..127] to reduce squared values */
            *(Sum2Ptr++) += Value12*Value12;
            SrcPtr += SrcStride;
          }
          SrcPtr += SrcPtrDelta;
        }
      }
      else
      {
        while (y--)
        {
          x = DividedWidth;
          while (x--)
          {
            *(Sum1Ptr++) += ((signed int)SrcPtr[0])-((signed int)0x80);  /* Shift all values in the range [-128..127] for compatibility with the idea described above */
            SrcPtr += SrcStride;
          }
          SrcPtr += SrcPtrDelta;
        }
      }
    }
    /* Release source frame */
    avs_release_video_frame(SrcFrame);
    SrcFrame = 0;
    FramesLeft--;
    FrameNo++;
    if (!FramesLeft && (FrameNo!=FrameCount))
    {
      Sum1Max = 0;
      Sum2Max = 0;
      for (ChannelNo=0 ; ChannelNo<ChannelCount ; ChannelNo++)
      {
        Sum1Ptr = Sum1[ChannelNo];
        Sum2Ptr = Sum2[ChannelNo];
        for (k=0 ; k<Width/PixelWidth[ChannelNo]*Height/PixelHeight[ChannelNo] ; k++)
        {
          if (Sum1Max<((Sum1Ptr[k]<0)?(-Sum1Ptr[k]):(Sum1Ptr[k])))  Sum1Max = (Sum1Ptr[k]<0)?(-Sum1Ptr[k]):(Sum1Ptr[k]);
          if (Sum2Ptr)  if (Sum2Max<Sum2Ptr[k])  Sum2Max = Sum2Ptr[k];
        }
      }
      FramesLeft = FrameCount-FrameNo;
      if (ComputeAlpha)  if (FramesLeft>(Analyze_Sum2Max-Sum2Max)/Sum2MaxDelta)  FramesLeft = (Analyze_Sum2Max-Sum2Max)/Sum2MaxDelta;
      if (FramesLeft>(Analyze_Sum1Max-Sum1Max)/Sum1MaxDelta)  FramesLeft = (Analyze_Sum1Max-Sum1Max)/Sum1MaxDelta;
    }
  }
  /* If (whyever) there is still a SrcFrame, we release it */
  if (SrcFrame)  avs_release_video_frame(SrcFrame);
  /* If we have less than two frames, we return an error (maybe Sum1/2Max is too low..??) */
  if (ComputeAlpha)  if (FrameNo<2)  Result = Analyze_Result_GeneralError;
  if (FrameNo<1)  Result = Analyze_Result_GeneralError;
  /* If a (serious) error occured, we return */
  if (Result!=Analyze_Result_OK)
  {
    for (ChannelNo=0 ; ChannelNo<ChannelCount ; ChannelNo++)  if (Sum1[ChannelNo])  free(Sum1[ChannelNo]);
    for (ChannelNo=0 ; ChannelNo<ChannelCount ; ChannelNo++)  if (Sum2[ChannelNo])  free(Sum2[ChannelNo]);
    return  Result;
  }
  FrameCount = FrameNo;  /* From now on this is the number of frames processed (may be less than the real FrameCount) */
  /* Current state:
     Sum1[ChNo][Width/PxWidth[ChNo]*y+x] contains the sum of all values (minus 0x80) of the pixel in position (x,y) in color channel (ChNo) over frames 0..FrameCount-1
     Sum2[ChNo][Width/PxWidth[ChNo]*y+x] contains the sum of the squares of all values (minus 0x80) of the pixel in position (x,y) in color channel (ChNo) over frames 0..FrameCount-1
     FrameCount is the numer of frames processed
  */
  /*** PHASE 2: Determine Color averages and standard deviations of non-masked pixels ***/
  /* We are going to sum up all (non-masked) Sum1 and Sum2 values.
     This will take place in double variables (with limited
     accuracy of about 10^-16). We use Kahan's summation formula
     to reduce numerical errors. Hopefully we don't end up with
     negative variances ...                                         */
  /* ChannelMask will store the sub-sampled logo mask, NMSum2 is an array for sorting the squared pixel sums */
  ChannelMaskVector = 0;
  ChannelNo = 0;
  while (ChannelNo<ChannelCount)
  {
    if (PixelWidth[ChannelNo]*PixelHeight[ChannelNo]>1)
    {
      ChannelMaskVector = (BYTE*)malloc(sizeof(BYTE)*Width*Height/2);
      break;
    }
    ChannelNo++;
  }
  NMSum1 = (Analyze_Sum1Type*)malloc(sizeof(Analyze_Sum1Type)*(Width*Height-MaskSize));
  if (ComputeAlpha)  NMSum2 = (Analyze_Sum2Type*)malloc(sizeof(Analyze_Sum2Type)*(Width*Height-MaskSize));  else  NMSum2 = 0;
  if (((ChannelNo<ChannelCount) && !ChannelMaskVector) || !NMSum1 || (ComputeAlpha && !NMSum2))
  {
    if (NMSum2)  free(NMSum2);
    if (NMSum1)  free(NMSum1);
    if (ChannelMaskVector)  free(ChannelMaskVector);
    for (ChannelNo=0 ; ChannelNo<ChannelCount ; ChannelNo++)  if (Sum2[ChannelNo])  free(Sum2[ChannelNo]);
    for (ChannelNo=0 ; ChannelNo<ChannelCount ; ChannelNo++)  if (Sum1[ChannelNo])  free(Sum1[ChannelNo]);
    return  Analyze_Result_MemoryError;
  }
  /** Main loop in Phase 2: Each channel is processed completely **/
  ChannelMask = Mask;
  ChannelMaskSize = MaskSize;
  for (ChannelNo=0 ; ChannelNo<ChannelCount ; ChannelNo++)
  {
    if (!ChannelNo || PixelWidth[ChannelNo]!=PixelWidth[ChannelNo-1] || PixelHeight[ChannelNo]!=PixelHeight[ChannelNo-1])
    {
      if (PixelWidth[ChannelNo]*PixelHeight[ChannelNo]==1)
      {
        ChannelMask = Mask;
        ChannelMaskSize = MaskSize;
      }
      else
      {
        ChannelMask = ChannelMaskVector;
        /* Clear and fill ChannelMask */
        for (k=0 ; k<(Width/PixelWidth[ChannelNo])*(Height/PixelHeight[ChannelNo]) ; k++)  ChannelMask[k] = 0;
        for (y=0 ; y<Height ; y++)
        {
          for (x=0 ; x<Width ; x++)
          {
            /* Each makro-pixel *is* masked whenever one of its smaller (real-sized) pixels is masked */
            ChannelMask[(Width/PixelWidth[ChannelNo])*(y/PixelHeight[ChannelNo])+(x/PixelWidth[ChannelNo])] += Mask[Width*y+x]?1:0;
          }
        }
        /* Determine size of sub-sampled mask */
        ChannelMaskSize = 0;
        for (k=0 ; k<(Width/PixelWidth[ChannelNo])*(Height/PixelHeight[ChannelNo]) ; k++)  ChannelMaskSize += ChannelMask[k]?1:0;
      }
    }
    Sum1Ptr = Sum1[ChannelNo];
    Sum2Ptr = Sum2[ChannelNo];
    k = 0;
    if (ComputeAlpha)
    {
      for (y=0 ; y<Height/PixelHeight[ChannelNo] ; y++)
      {
        for (x=0 ; x<Width/PixelWidth[ChannelNo] ; x++)
        {
          if (!ChannelMask[(Width/PixelWidth[ChannelNo])*y+x])
          {
            NMSum1[k] = Sum1Ptr[(Width/PixelWidth[ChannelNo])*y+x];
            NMSum2[k] = Sum2Ptr[(Width/PixelWidth[ChannelNo])*y+x];
            k++;
          }
        }
      }
    }
    else
    {
      for (y=0 ; y<Height/PixelHeight[ChannelNo] ; y++)
      {
        for (x=0 ; x<Width/PixelWidth[ChannelNo] ; x++)
        {
          if (!ChannelMask[(Width/PixelWidth[ChannelNo])*y+x])
          {
            NMSum1[k] = Sum1Ptr[(Width/PixelWidth[ChannelNo])*y+x];
            k++;
          }
        }
      }
    }
    /* Assertion: If mask size has changed, something must be wrong !! */
    if ((Width/PixelWidth[ChannelNo])*(Height/PixelHeight[ChannelNo])!=ChannelMaskSize+k)
    {
      if (NMSum2)  free(NMSum2);
      if (NMSum1)  free(NMSum1);
      if (ChannelMaskVector)  free(ChannelMaskVector);
      for (ChannelNo=0 ; ChannelNo<ChannelCount ; ChannelNo++)  if (Sum2[ChannelNo])  free(Sum2[ChannelNo]);
      for (ChannelNo=0 ; ChannelNo<ChannelCount ; ChannelNo++)  if (Sum1[ChannelNo])  free(Sum1[ChannelNo]);
      return  Analyze_Result_GeneralError;
    }
    qsort(NMSum1,(Width/PixelWidth[ChannelNo])*(Height/PixelHeight[ChannelNo])-ChannelMaskSize,sizeof(Analyze_Sum1Type),SortCompareSum1);
    KahanSum = 0.0;
    KahanC = 0.0;
    for (k=0 ; k<((Width/PixelWidth[ChannelNo])*(Height/PixelHeight[ChannelNo])-ChannelMaskSize) ; k++)  KahanAdditionMakro(KahanSum,NMSum1[k],KahanC,KahanT,KahanY);
    ChannelNMSum1[ChannelNo] = KahanSum;
    if (ComputeAlpha)
    {
      qsort(NMSum2,(Width/PixelWidth[ChannelNo])*(Height/PixelHeight[ChannelNo])-ChannelMaskSize,sizeof(Analyze_Sum2Type),SortCompareSum2);
      KahanSum = 0.0;
      KahanC = 0.0;
      for (k=0 ; k<((Width/PixelWidth[ChannelNo])*(Height/PixelHeight[ChannelNo])-ChannelMaskSize) ; k++)  KahanAdditionMakro(KahanSum,NMSum2[k],KahanC,KahanT,KahanY);
      ChannelNMSum2[ChannelNo] = KahanSum;
    }
    /* Compute averages and variances of non-masked pixels */
    k = (Width/PixelWidth[ChannelNo])*(Height/PixelHeight[ChannelNo])-ChannelMaskSize;
    ChannelNMAverage[ChannelNo] = ChannelNMSum1[ChannelNo]/k/FrameCount;
    if (ComputeAlpha)
    {
      ChannelNMVariance[ChannelNo] = ChannelNMSum2[ChannelNo]-ChannelNMSum1[ChannelNo]*ChannelNMSum1[ChannelNo]/k/FrameCount;
      ChannelNMVariance[ChannelNo] /= (double)k*(double)FrameCount-1.0;
      if (!(ChannelNMVariance[ChannelNo]>0.0))  ChannelNMVariance[ChannelNo] = 0.0;
    }
  }
  if (NMSum2)  free(NMSum2);
  if (NMSum1)  free(NMSum1);
  if (ChannelMaskVector)  free(ChannelMaskVector);
  /*** PHASE 3: Compute transparency values and color values for each pixel of the logo (each mask pixel) and store in destination frame ***/
  /** We start with the alpha channel - it is needed to compute the color channels **/
  AlphaChannel = (float*)malloc(sizeof(float)*Width*Height);
  if (!AlphaChannel)
  {
    for (ChannelNo=0 ; ChannelNo<ChannelCount ; ChannelNo++)  if (Sum1[ChannelNo])  free(Sum1[ChannelNo]);
    for (ChannelNo=0 ; ChannelNo<ChannelCount ; ChannelNo++)  if (Sum2[ChannelNo])  free(Sum2[ChannelNo]);
    return  Analyze_Result_MemoryError;
  }
  if (ComputeAlpha)
  {
    /* Here is THE CORE ALGORITHM for computing the alpha channel out of all the variances gathered before */
    /* The alpha channel is computed as the quotiont of the standard deviation and the average (non-masked) standard deviation */
    AlphaWeightSum = 0.0;
    for (ChannelNo=0 ; ChannelNo<ChannelCount ; ChannelNo++)
    {
      AlphaWeight[ChannelNo] = 0.0;
      if (ChannelNMVariance[ChannelNo]<0.0)  continue;
      AlphaWeight[ChannelNo] = 1.0;
      if (DeviationWeight>2*FLT_EPSILON)  AlphaWeight[ChannelNo] *= pow(ChannelNMVariance[ChannelNo],0.5*DeviationWeight);
      if (SubsamplingWeight>FLT_EPSILON)  AlphaWeight[ChannelNo] /= pow(PixelWidth[ChannelNo]*PixelHeight[ChannelNo],SubsamplingWeight);
      if (!(AlphaWeight[ChannelNo]>0.0))  AlphaWeight[ChannelNo] = 0.0;
      AlphaWeightSum += AlphaWeight[ChannelNo];
    }
    if (AlphaWeightSum>0.0)
    {
      for (ChannelNo=0 ; ChannelNo<ChannelCount ; ChannelNo++)  AlphaWeight[ChannelNo] /= AlphaWeightSum;
      for (y=0 ; y<Height ; y++)
      {
        for(x=0 ; x<Width ; x++)
        {
          AlphaSum = 0.0;
          for (ChannelNo=0 ; ChannelNo<ChannelCount ; ChannelNo++)
          {
            Sum1Ptr = Sum1[ChannelNo];
            Sum2Ptr = Sum2[ChannelNo];
            k = (Width/PixelWidth[ChannelNo])*(y/PixelHeight[ChannelNo])+(x/PixelWidth[ChannelNo]);
            PixelValue = (double)Sum2Ptr[k]-(double)Sum1Ptr[k]*(double)Sum1Ptr[k]/FrameCount;
            if (!(PixelValue>0.0))  PixelValue = 0.0;
            PixelValue = sqrt(PixelValue/(FrameCount-1)/ChannelNMVariance[ChannelNo]);
            if (!(PixelValue>0.0))  PixelValue = 0.0;
            if (PixelValue>1.0)  PixelValue = 1.0;
            AlphaSum += AlphaWeight[ChannelNo]*PixelValue;
          }
          if (AlphaSum>1.0)  AlphaSum = 1.0;
          if (!(AlphaSum<2.0))  AlphaSum = 1.0;  /* Emergency stop if division by zero or other shit occured */
          AlphaChannel[Width*y+x] = 1.0-AlphaSum;  /* = Opacity */
        }
      }
    }
    else
    {
      for (k=0 ; k<Width*Height ; k++)  AlphaChannel[k] = 0.0;
    }
  }
  else
  {
    for (k=0 ; k<Width*Height ; k++)  AlphaChannel[k] = (float)Mask[k]/(float)(0xFF);
  }
  /* Now Sum2 is not needed anymore */
  for (ChannelNo=0 ; ChannelNo<ChannelCount ; ChannelNo++)  if (Sum2[ChannelNo])  free(Sum2[ChannelNo]);
  /** The destination frame pointers are determined **/
  /* Now we're approaching color-space dependent code !! */
  if (!GetFramePointers(DestFrame,DestData,0,FramePitch,0,0,0,SrcVideoInfo->pixel_type))
  {
    if (AlphaChannel)  free(AlphaChannel);
    for (ChannelNo=0 ; ChannelNo<ChannelCount ; ChannelNo++)  if (Sum1[ChannelNo])  free(Sum1[ChannelNo]);
    return  Analyze_Result_DestFrameError;
  }
  if (avs_is_yuv(SrcVideoInfo))
  {
    AlphaData = DestData[0]+Height*FramePitch[0];
    AlphaPitch = FramePitch[0];
    AlphaStride = FrameStride[0];
  }
  else
  {
    AlphaData = DestData[(DestVideoInfo->height>SrcVideoInfo->height)?0:3];  /* For RGB24 we have to copy the alpha values to the green and blue channel later ! */
    if (DestVideoInfo->height>SrcVideoInfo->height)  for (ChannelNo=0 ; ChannelNo<ChannelCount ; ChannelNo++)  DestData[ChannelNo] += Height*FramePitch[ChannelNo];
    AlphaPitch = FramePitch[0];
    AlphaStride = FrameStride[0];
  }
  /** The alpha channel is stored in the destination frame **/
  for (y=0 ; y<Height ; y++)
  {
    for(x=0 ; x<Width ; x++)
    {
      AlphaData[y*AlphaPitch+x*AlphaStride] = round(AlphaChannel[Width*y+x]*0xFF);  /* 0=transparent 255=opaque */
    }
  }
  /** Now we compute the color channels **/
  ChannelAlphaVector = 0;
  ChannelNo = 0;
  while (ChannelNo<ChannelCount)
  {
    /* Only needed if PixelWidth[CC]*PixelHeight[CC]>1 for at least one color channel */
    if (PixelWidth[ChannelNo]*PixelHeight[ChannelNo]>1)
    {
      ChannelAlphaVector = (float*)malloc(sizeof(float)*Width*Height/2);
      break;
    }
    ChannelNo++;
  }
  if ((ChannelNo<ChannelCount) && !ChannelAlphaVector)
  {
    if (AlphaChannel)  free(AlphaChannel);
    for (ChannelNo=0 ; ChannelNo<ChannelCount ; ChannelNo++)  if (Sum1[ChannelNo])  free(Sum1[ChannelNo]);
    return  Analyze_Result_MemoryError;
  }
  ChannelAlpha = AlphaChannel;
  for (ChannelNo=0 ; ChannelNo<ChannelCount ; ChannelNo++)
  {
    if (!(ChannelNo && (PixelWidth[ChannelNo]==PixelWidth[ChannelNo-1]) && (PixelHeight[ChannelNo]==PixelHeight[ChannelNo-1])))
    {
      if (PixelWidth[ChannelNo]*PixelHeight[ChannelNo]==1)
      {
        ChannelAlpha = AlphaChannel;
      }
      else
      {
        ChannelAlpha = ChannelAlphaVector;
        for (k=0 ; k<(Width/PixelWidth[ChannelNo])*(Height/PixelHeight[ChannelNo]) ; k++)  ChannelAlpha[k] = 0.0;
        for (y=0 ; y<Height ; y++)
        {
          for(x=0 ; x<Width ; x++)
          {
            ChannelAlpha[(Width/PixelWidth[ChannelNo])*(y/PixelHeight[ChannelNo])+(x/PixelWidth[ChannelNo])] += AlphaChannel[Width*y+x]/(PixelWidth[ChannelNo]*PixelHeight[ChannelNo]);
          }
        }
      }
    }
    Sum1Ptr = Sum1[ChannelNo];
    DestPtr = DestData[ChannelNo];
    for (y=0 ; y<Height/PixelHeight[ChannelNo] ; y++)
    {
      for (x=0 ; x<Width/PixelWidth[ChannelNo] ; x++)
      {
        PixelValue = ChannelAlpha[(Width/PixelWidth[ChannelNo])*y+x];
        PixelValue = (PixelValue>0.0)?((double)Sum1Ptr[(Width/PixelWidth[ChannelNo])*y+x]/(double)FrameCount-(1.0-PixelValue)*ChannelNMAverage[ChannelNo])/PixelValue:(double)Sum1Ptr[(Width/PixelWidth[ChannelNo])*y+x]/(double)FrameCount;
        PixelValue += 0x80;
        if (PixelValue<0x00)  PixelValue = 0x00;
        if (PixelValue>0xFF)  PixelValue = 0xFF;
        if (!(PixelValue<0x100))  PixelValue = 0x80;  /* Emergency stop if division by zero or something similar occured */
        DestPtr[FramePitch[ChannelNo]*y+x*FrameStride[ChannelNo]] = round(PixelValue);
      }
    }
  }
  if (ChannelAlphaVector)  free(ChannelAlphaVector);
  if (AlphaChannel)  free(AlphaChannel);
  for (ChannelNo=0 ; ChannelNo<ChannelCount ; ChannelNo++)  if (Sum1[ChannelNo])  free(Sum1[ChannelNo]);
  if (DestVideoInfo->height>SrcVideoInfo->height)
  {
    if (avs_is_yuv(SrcVideoInfo))
    {
      /* Clear chroma of mask part */
      for (ChannelNo=1 ; ChannelNo<ChannelCount ; ChannelNo++)
      {
        DestPtr = DestData[ChannelNo]+(Height/PixelHeight[ChannelNo])*FramePitch[ChannelNo];
        for (y=0 ; y<Height/PixelHeight[ChannelNo] ; y++)
        {
          for (x=0 ; x<Width/PixelWidth[ChannelNo] ; x++)
          {
            DestPtr[FramePitch[ChannelNo]*y+x*FrameStride[ChannelNo]] = 0x80;  /* No color I hope */
          }
        }
      }
    }
    if (avs_is_rgb(SrcVideoInfo))
    {
      for (y=0 ; y<Height ; y++)
      {
        for (x=0 ; x<Width ; x++)
        {
          AlphaData[y*AlphaPitch+x*AlphaStride+2] = AlphaData[y*AlphaPitch+x*AlphaStride+1] = AlphaData[y*AlphaPitch+x*AlphaStride];
        }
      }
      if (avs_is_rgb32(SrcVideoInfo))
      {
        for (y=0 ; y<Height ; y++)
        {
          for (x=0 ; x<Width ; x++)
          {
            AlphaData[y*AlphaPitch+x*AlphaStride+3] = 0;
          }
        }
      }
    }
  }
  return  FrameCount;

  #undef  KahanAdditionMakro
  #undef  Sum1MaxDelta
  #undef  Sum2MaxDelta
}



int  SortCompareSum1(const void *  Value1,  const void *  Value2)
{
  #define  Value2(X)  (*((Analyze_Sum1Type*)(X)))
  #define  Value(X)  (Value2(X)*(Value2(X)>0?+1:-1))
  if (Value(Value1)<Value(Value2))  return  -1;
  if (Value(Value1)>Value(Value2))  return  +1;
  return  0;
  #undef  Value
  #undef  Value2
}



int  SortCompareSum2(const void *  Value1,  const void *  Value2)
{
  #define  Value(X)  (*((Analyze_Sum2Type*)(X)))
  if (Value(Value1)<Value(Value2))  return  -1;
  if (Value(Value1)>Value(Value2))  return  +1;
  return  0;
  #undef  Value
}
