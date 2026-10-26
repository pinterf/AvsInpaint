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
//#define  VersionString  "2019.06.27 1.2"
#define  VersionString  "2021.10.25 1.3"
//#define  LegalInfoString  "* Logo Inpainting for AviSynth by Wolfgang Boiger *\nVersion " VersionString " ;  built  " __DATE__ "  " __TIME__ " .\nCopyright (C)  2007, 2008  Wolfgang Boiger, Berlin.\nLicensed under GNU GPL version 2 with possible geographic restrictions.\nThis software comes without any warranty, even without warranty\nof merchantability or fitness for a particular purpose.\nSee files 'AVSInpaint.htm' and 'GNUGPLv2.txt' for details."


constexpr int  FMMHeapDelta  = 8;  /* If heap size needs to be increased (= reallocation of memory), how many elemnts to add */
/* FMMHeapInitialSize is inlined at usage as: 4*(Width+Height+FMMHeapDelta) */


#define  FMMFarMarker  ((float)(32.0*INT_MAX))  /* Should be bigger than the maximal diameter of a frame */



#include  <cstdlib>
#include  <cmath>
#include  <climits>
#include  <cstdint>
#include  <cfloat>
#include  <cstring>
#include  <string>
#include  <algorithm>
#include  <array>
#include  <functional>
#include  <type_traits>
#include  <vector>

#include  "InpaintCore.h"
#ifdef ENABLE_AVISYNTH
#include  "AvsLoader.h"
#endif

typedef  int32_t   Analyze_Sum1Type;  /* Data types used for summation in Analyze function */
typedef  uint32_t  Analyze_Sum2Type;
#define  Analyze_Sum1Max  INT32_MAX   /* Maximal values for summation data types (summation will stop here) */
#define  Analyze_Sum2Max  UINT32_MAX


/* InpaintMaskDataStruct and InpaintDataStruct are defined in InpaintCore.h */


/* Heap used by fast marching method */
struct FMMHeapStruct
{
  int  Size, MaxSize, Delta;
  int * MemPointer, * Heap;
};


#ifdef ENABLE_AVISYNTH
/* Structure for deblending - with pre-computed data for static logo */
struct DeblendDataStruct
{
  int  PixelType, Width, Height;
  FrameLayout  layout;
  /* Static logo */
  std::array<std::vector<uint16_t>, MaxChannelCount>  StaticLogoDataChannels;
  std::array<std::vector<uint8_t>, MaxChannelCount>         StaticAlphaDataChannels;
  /* Dynamic logo */
  AVS_Clip * LogoClip, * AlphaClip;
  int  LogoPixelType, AlphaPixelType, LogoFrameCount, AlphaFrameCount, AlphaSampleStep, AlphaChannelNo;
  FrameLayout  logoLayout;
  bool  FlipLogo, FlipAlpha;  /* storage direction differs from the source (packed RGB is bottom-up) */
  int  ChromaLoc;  /* siting of subsampled chroma (ChromaLocation) */
  /* Is static? Check (LogoClip)     Separate alpha clip? Check (AlphaClip) */
  DeblendDataStruct() :
    PixelType(0), Width(0), Height(0), layout(),
    LogoClip(nullptr), AlphaClip(nullptr),
    LogoPixelType(0), AlphaPixelType(0), LogoFrameCount(0), AlphaFrameCount(0), AlphaSampleStep(0), AlphaChannelNo(0), logoLayout(),
    FlipLogo(false), FlipAlpha(false), ChromaLoc(ChromaLoc_Default)
  {}
};
#endif  // ENABLE_AVISYNTH


/* Some data for the distance function filter */
struct DistanceFunctionDataStruct
{
  int  PixelType, LumaChannelNo, Width, Height;
  FrameLayout  layout;
  double  Scale, PixelWidth, PixelHeight;
  signed int  ScaleSign;
};


/*****  Function declaration  *****/

#ifdef ENABLE_AVISYNTH
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

/* AviSynth-specific helpers */
FrameLayout  FrameLayoutFromPixelType(int  PixelType);
bool  AvsGetFramePlanes(AVS_VideoFrame *  Frame,  const FrameLayout &  L,  uint8_t **  Planes,  int *  Pitch);
bool  AvsGetFramePlanesRO(AVS_VideoFrame *  Frame,  const FrameLayout &  L,  const uint8_t **  Planes,  int *  Pitch);
static InpaintMaskResult  AvsInpaintPrepareMask(InpaintMaskDataStruct **  MaskDataPtr,  AVS_VideoFrame *  Frame,  const InpaintDataStruct *  InpaintData);
int  AvsDeblendStaticPrepare(uint16_t *  LogoDataChannels[],  uint8_t *  AlphaDataChannels[],  AVS_VideoFrame *  LogoFrame,  AVS_VideoFrame *  AlphaFrame,  const AVS_VideoInfo *  LogoVideoInfo,  const AVS_VideoInfo *  AlphaVideoInfo,  int  AlphaChannel,  bool  FlipLogo,  bool  FlipAlpha,  int  ChromaLoc);
static int  AvsGetChromaLocation(AVS_ScriptEnvironment *  Env,  AVS_Clip *  Clip,  const AVS_Value &  Arg,  const char *  FilterName,  const char **  ErrorText);
#endif  // ENABLE_AVISYNTH

/** Own functions **/
/* Common functions */
bool  FastMarching(float  u[],  unsigned int *  Order,  int  Width,  int  Height,  double  PixelWidth,  double  PixelHeight);
bool  FMMUpdateCloseValue(float  u[],  int  Close[],  int  x,  int  y,  int  Width,  int  Height,  double  PixelWidth,  double  PixelHeight,  FMMHeapStruct *  Heap,  std::vector<int> &  HeapVec);
/* Inpaint tools */
InpaintMaskResult  InpaintPrepareMask(InpaintMaskDataStruct **  MaskDataPtr,  const FrameLayout &  maskLayout,  const uint8_t *  planes[],  int  pitch[],  const InpaintDataStruct *  InpaintData);
void  AddPreBlurValue(float  G[],  int  x,  int  y,  int  Width,  int  Height,  int  Value,  double  BlurXKernel[],  int  BlurXKernelSize,  double  BlurYKernel[],  int  BlurYKernelYize);
template<int SS> static int  InpaintPixelImpl(double  RadiusX,  double  RadiusY,  uint8_t  u[],  uint8_t  chi[],  int  SampleStep,  int  Pitch,  int  x,  int  y,  int  Width,  int  Height,  double  CoherenceStrength,  double  CoherenceDirectionX,  double  CoherenceDirectionY);
double  GetCoherenceData(double *  CoherenceDirectionX,  double *  CoherenceDirectionY,  double  TensorXX,  double  TensorXY,  double  TensorYY,  double  Sharpness);
void  GetStructureTensor(double *  TensorXX,  double *  TensorXY,  double *  TensorYY,
                         float *  GchiLuma,  float *  GchiuLuma,  uint8_t *  chiLuma,  double  LumaWeight,
                         float *  GchiChroma,  float **  GchiuChromas,  uint8_t *  chiChroma,  double  ChromaWeight,
                         int  ChromaPixelWidth,  int  ChromaPixelHeight,  int  ChromaChannelCount,
                         int  x,  int  y,  int  Width,  int  Height,
                         double *  LumaBlurXKernel,  int  LumaBlurXKernelSize,  int  LumaXDirection,
                         double *  LumaBlurYKernel,  int  LumaBlurYKernelSize,  int  LumaYDirection,
                         double *  ChromaBlurXKernel,  int  ChromaBlurXKernelSize,  int  ChromaXDirection,
                         double *  ChromaBlurYKernel,  int  ChromaBlurYKernelSize,  int  ChromaYDirection);
bool  GetChannelTensor(double *  TensorXX,  double *  TensorXY,  double *  TensorYY,  float  Gchi[],  float  Gchiu[],  uint8_t  chi[],  int  x,  int  y,  int  Width,  int  Height,  double  BlurKernelX[],  int  BlurKernelXSize,  double  BlurKernelY[],  int  BlurKernelYSize);
double *  CreateGaussKernel(double  Kernel[],  int  Size,  double  Deviation,  double  Center);
/* Deblend tools */
void  DeblendDynamic(uint8_t  Data[],  const  uint8_t  LogoData[],  const  uint8_t  AlphaData[],  int  Pitch,  int  SampleStep,  int  LogoPitch,  int  LogoSampleStep,  int  AlphaPitch,  int  AlphaSampleStep,  int  Width,  int  Height);
void  DeblendDynamicSubsampled(uint8_t  Data[],  const  uint8_t  LogoData[],  const  uint8_t  AlphaData[],  int  Pitch,  int  SampleStep,  int  LogoPitch,  int  LogoSampleStep,  int  AlphaPitch,  int  AlphaSampleStep,  int  Width,  int  Height,  int  PixelWidth,  int  PixelHeight,  int  ChromaLoc,  int  ChannelNo,  SubsampledAlphaCache &  Cache);
/* Analyze tools */
int32_t  Analyze(const FrameLayout &  srcFmt,  int  Width,  int  Height,  unsigned int  FrameCount,
                 std::function<bool(unsigned int, const uint8_t*[], int[])>  fetchSrcFrame,
                 std::function<void(unsigned int)>                           releaseSrcFrame,
                 uint8_t *  destPlanes[],  int  destPitch[],  bool  destHasAlphaRow,
                 unsigned char  Mask[],  int32_t  MaskSize,  int  ComputeAlpha,
                 double  DeviationWeight,  double  SubsamplingWeight,  int  ChromaLoc);



/*****  Function implementation  *****/


#ifdef ENABLE_AVISYNTH
/* FrameLayout channel index of the mask/alpha in a Mask/Alpha clip: the luma of YUY2/YV12/16/24/Y8,
   the alpha of RGB32/RGBAP (RGB channels are stored B,G,R,A). Other formats are rejected by the callers. */
static int  AvsMaskChannel(int  PixelType)
{
  const FrameLayout  L = FrameLayoutFromPixelType(PixelType);
  return  ((L.colorFamily == ColorFamily::RGB) && (L.channelCount == 4)) ? 3 : 0;
}

/* Packed RGB (RGB24/RGB32) is stored bottom-up, every other format top-down */
static bool  AvsIsBottomUp(const AVS_VideoInfo *  vi)
{
  return  avs_is_rgb(vi) && !avs_is_planar(vi);
}


AVSC_EXPORT  const char *  AVSC_CC  avisynth_c_plugin_init(AVS_ScriptEnvironment *  Env)
{
  if (!avs_loader_init())  return  "AvsInPaint: failed to load AviSynth library";
  avs_add_function(Env,"InpaintLogo","c[Mask]c[Radius]f[Sharpness]f[PreBlur]f[PostBlur]f[ChromaWeight]f[PreBlurSize]f[PostBlurSize]f[ChromaTensor]b[PixelAspect]f[Steps]i",Inpaint_Create,nullptr);
  avs_add_function(Env,"DeblendLogo","cc[Alpha]c[ChromaPlacement]s",Deblend_Create,nullptr);
  avs_add_function(Env,"AnalyzeLogo","c[Mask]c[ComputeAlpha]b[DeviationWeight]f[SubsamplingWeight]f[ChromaPlacement]s",Analyze_Create,nullptr);
  avs_add_function(Env,"DistanceFunction","c[Scale]f[PixelAspect]f",DistanceFunction_Create,nullptr);
  return  "Logo Inpainting Filter";
}


AVSC_EXPORT  const char *  AVSC_CC  avisynth_c_plugin_init2(AVS_ScriptEnvironment *  Env)
{
  g_is_v11_64bit_data_capable_host = true;
  return  avisynth_c_plugin_init(Env);
}
#endif  // ENABLE_AVISYNTH



#ifdef ENABLE_AVISYNTH
/****************************************************************
                     Inpaint callbacks
 ****************************************************************/
AVS_Value  AVSC_CC  Inpaint_Create(AVS_ScriptEnvironment *  Env,  AVS_Value  Args,  void *  Data)
{
  AVS_Value  DestClipValue;
  AVS_Clip * MaskClip, * DestClip;
  AVS_FilterInfo * FilterInfo;
  AVS_VideoFrame * Frame;
  const AVS_VideoInfo * VideoInfo, * MaskVideoInfo;
  int  Steps, k;
  bool  ChromaTensor;
  double  PAR, Radius, Sharpness, PreBlur, PostBlur, ChromaWeight, PreBlurSize, PostBlurSize;
  const char * ErrorText;
  InpaintDataStruct * InpaintData;

  /* Create new filter */
  DestClip = avs_new_c_filter(Env,&FilterInfo,avs_array_elt(Args,0),1);
  if (!DestClip)  return  avs_new_value_error("InpaintLogo: Could not create filter");
  VideoInfo = &FilterInfo->vi;
  /* If something fails here, we store a note in ErrorText. This will be checked later, and memory will be cleaned up before leaving */
  ErrorText = nullptr;
  InpaintData = nullptr;
  /* Load mask clip */
  MaskClip = avs_is_clip(avs_array_elt(Args,1))?avs_take_clip(avs_array_elt(Args,1),Env):nullptr;
  MaskVideoInfo = MaskClip?avs_get_video_info(MaskClip):nullptr;
  if (MaskClip && !MaskVideoInfo)  ErrorText = "InpaintLogo: Could not load mask clip";
  /* Check all video properties */
  else if (!(avs_is_yuy2(VideoInfo) || avs_is_yv12(VideoInfo) || avs_is_yv16(VideoInfo) || avs_is_yv24(VideoInfo) || avs_is_y8(VideoInfo) || avs_is_rgb32(VideoInfo) || avs_is_rgb24(VideoInfo) || avs_is_planar_rgb(VideoInfo) || avs_is_planar_rgba(VideoInfo)))  ErrorText = "InpaintLogo: Color space unknown (RGB24, RGB32, RGBP, RGBAP, YUY2, YV12/16/24/8 allowed only)";
  else if (!(avs_has_video(VideoInfo) && VideoInfo->num_frames && VideoInfo->width && VideoInfo->height))  ErrorText = "InpaintLogo: Source clip has no video";
  else if (MaskClip && !(avs_is_yuy2(MaskVideoInfo) || avs_is_yv12(MaskVideoInfo) || avs_is_yv16(MaskVideoInfo) || avs_is_yv24(MaskVideoInfo) || avs_is_y8(MaskVideoInfo) || avs_is_rgb32(MaskVideoInfo) || avs_is_planar_rgba(MaskVideoInfo)))  ErrorText = "InpaintLogo: Mask color space must have luma channel (YUY2, YV12/16/24, Y8) or alpha channel (RGB32, RGBAP)";
  else if (MaskClip && !(avs_has_video(MaskVideoInfo) && MaskVideoInfo->num_frames && MaskVideoInfo->width && MaskVideoInfo->height))  ErrorText = "InpaintLogo: Mask clip has no video";
  else if (MaskClip && !((VideoInfo->width==MaskVideoInfo->width) && (VideoInfo->height==MaskVideoInfo->height)))  ErrorText = "InpaintLogo: Source and mask clip have different dimensions";
  else if (!(MaskClip || avs_is_rgb32(VideoInfo) || avs_is_planar_rgba(VideoInfo)))  ErrorText = "InpaintLogo: Mask is missing (needed if source clip is not RGB32 or RGBAP)";
  else if (FrameLayoutFromPixelType(VideoInfo->pixel_type).channelCount-(MaskClip?0:1)<=0)  ErrorText = "InpaintLogo internal error: Color space unrecognized";

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
  if (FrameLayoutFromPixelType(VideoInfo->pixel_type).channelCount-1-(MaskClip?0:1)==0)  ChromaWeight = 0.0;  else  if (avs_is_rgb(VideoInfo))  ChromaWeight = 1.0-1.0/(FrameLayoutFromPixelType(VideoInfo->pixel_type).channelCount-1-(MaskClip?0:1));
  if ((ChromaWeight<0.0) || (ChromaWeight>1.0))  ErrorText = "InpaintLogo: Chroma structure tensor weight must lie in the interval [0.0,1.0]";
  #if (!(defined(BlurStandardDeviation) && ((BlurStandardDeviation)!=0)))
  PreBlur = std::sqrt(2*PreBlur);
  PostBlur = std::sqrt(2*PostBlur);
  #endif
  PreBlurSize = 2*PreBlur;
  PostBlurSize = 2*PostBlur;
  if (avs_is_float(avs_array_elt(Args,7)))  PreBlurSize = avs_as_float(avs_array_elt(Args,7));
  if (PreBlur==0.0)  PreBlurSize = 0;
  if (PreBlurSize<0.0)  ErrorText = "InpaintLogo: Pre-blurring kernel size may not be negative";
  if (avs_is_float(avs_array_elt(Args,8)))  PostBlurSize = avs_as_float(avs_array_elt(Args,8));
  if (PostBlur==0.0)  PostBlurSize = 0;
  if (PostBlurSize<0.0)  ErrorText = "InpaintLogo: Post-blurring kernel size may not be negative";
  ChromaTensor = avs_is_bool(avs_array_elt(Args,9)) ? (avs_as_bool(avs_array_elt(Args,9)) != 0) : false;
  PAR = avs_is_float(avs_array_elt(Args,10))?avs_as_float(avs_array_elt(Args,10)):1.0;
  if (PAR<=0.0)  ErrorText = "InpaintLogo: Pixel aspect ratio must be positive";
  Steps = avs_is_int(avs_array_elt(Args,11))?avs_as_int(avs_array_elt(Args,11)):(-1);
  if (Steps<-1)  ErrorText = "InpaintLogo: Steps count may not be lower than -1";

  /* Prepare data structure */
  if (!MaskClip)  MaskClip = avs_take_clip(avs_array_elt(Args,0),Env);
  MaskVideoInfo = MaskClip?avs_get_video_info(MaskClip):nullptr;
  if (!MaskVideoInfo)  ErrorText = "InpaintLogo internal error: Opening mask clip failed";
  else
  {
    /* Allocate data struct */
    InpaintData = new (std::nothrow) InpaintDataStruct();
    if (!InpaintData)  ErrorText = "InpaintLogo: Could not allocate memory";
  }
  if (!ErrorText)
  {
    /** Color space parameters **/
    InpaintData->srcLayout = FrameLayoutFromPixelType(VideoInfo->pixel_type);
    InpaintData->ChromaChannelCount = InpaintData->srcLayout.channelCount-1-(MaskClip?0:1);
    InpaintData->PixelType = VideoInfo->pixel_type;
    InpaintData->Width = VideoInfo->width;
    InpaintData->Height = VideoInfo->height;
    /* If there is no chroma channel (Y8) all chroma values are set to zero */
    if (InpaintData->ChromaChannelCount)
    {
      InpaintData->ChromaPixelWidth  = InpaintData->srcLayout.pixelWidth[1];
      InpaintData->ChromaPixelHeight = InpaintData->srcLayout.pixelHeight[1];
    }
    else
    {
      InpaintData->ChromaPixelWidth = 0;
      InpaintData->ChromaPixelHeight = 0;
    }
    InpaintData->isYUV             = (InpaintData->PixelType & AVS_CS_YUV) != 0;
    InpaintData->isChromaSubsampled = (InpaintData->ChromaPixelWidth > 1) || (InpaintData->ChromaPixelHeight > 1);
    /* Parameters of mask clip: Only if mask is dynamic */
    if (!ErrorText && (MaskVideoInfo->num_frames>1))
    {
      InpaintData->MaskClip = MaskClip;
      InpaintData->MaskFrameCount = MaskVideoInfo->num_frames;
    }
    else
    {
      InpaintData->MaskClip = nullptr;
      InpaintData->MaskFrameCount = 0;
    }
    InpaintData->maskLayout = FrameLayoutFromPixelType(MaskVideoInfo->pixel_type);
    InpaintData->MaskChannelNo = AvsMaskChannel(MaskVideoInfo->pixel_type);  /* no Mask given: the source alpha */
    // only packed RGB is upside-down, planar is not
    InpaintData->FlipMask = ((avs_is_rgb(VideoInfo) && !avs_is_planar(VideoInfo)) != (avs_is_rgb(MaskVideoInfo) && !avs_is_planar(MaskVideoInfo)));
    InpaintData->MaskPixelType = MaskVideoInfo->pixel_type;
    InpaintData->MaxSteps = (Steps>=0)?Steps:-1;
    /** user-specified values **/
    InpaintData->PixelWidth = std::sqrt(PAR);
    InpaintData->PixelHeight = 1.0/InpaintData->PixelWidth;
    /* If no chroma channels, corresponding values are set to zero */
    if (InpaintData->ChromaChannelCount)
    {
      InpaintData->LumaWeight = 1.0-ChromaWeight;
      InpaintData->ChromaWeight = ChromaWeight/InpaintData->ChromaChannelCount;
      InpaintData->ChromaRadiusX = std::max<double>(Radius/InpaintData->PixelWidth/InpaintData->ChromaPixelWidth,InpaintMinRadiusXY);
      InpaintData->ChromaRadiusY = std::max<double>(Radius/InpaintData->PixelHeight/InpaintData->ChromaPixelHeight,InpaintMinRadiusXY);
    }
    else
    {
      InpaintData->LumaWeight = 1.0;
      InpaintData->ChromaWeight = 0.0;
      InpaintData->ChromaRadiusX = 0.0;
      InpaintData->ChromaRadiusY = 0.0;
    }
    InpaintData->LumaRadiusX = std::max<double>(Radius/InpaintData->PixelWidth,InpaintMinRadiusXY);
    InpaintData->LumaRadiusY = std::max<double>(Radius/InpaintData->PixelHeight,InpaintMinRadiusXY);
    InpaintData->Sharpness = Sharpness;
    InpaintData->ChromaTensor = ChromaTensor && InpaintData->ChromaChannelCount && avs_is_yuv(VideoInfo);
    /** Set blurring kernel sizes and allocate a lot of memory **/
    if (InpaintData->LumaWeight!=0.0)
    {
      InpaintData->LumaPreBlurXKernelSize = static_cast<int>(std::max<double>(std::floor(PreBlurSize/InpaintData->PixelWidth),InpaintMinPreBlurXYKernelSize));
      InpaintData->LumaPreBlurYKernelSize = static_cast<int>(std::max<double>(std::floor(PreBlurSize/InpaintData->PixelHeight),InpaintMinPreBlurXYKernelSize));
      InpaintData->Luma4LumaPostBlurXKernelSize = static_cast<int>(std::max<double>(std::floor(PostBlurSize/InpaintData->PixelWidth+0.5),InpaintMinPostBlurXYKernelSize));
      InpaintData->Luma4LumaPostBlurYKernelSize = static_cast<int>(std::max<double>(std::floor(PostBlurSize/InpaintData->PixelHeight+0.5),InpaintMinPostBlurXYKernelSize));
      try
      {
        InpaintData->LumaPreBlurXKernelVector.resize(2*InpaintData->LumaPreBlurXKernelSize+1);
        InpaintData->LumaPreBlurYKernelVector.resize(2*InpaintData->LumaPreBlurYKernelSize+1);
        InpaintData->Luma4LumaPostBlurXKernelVector.resize(2*InpaintData->Luma4LumaPostBlurXKernelSize+1);
        InpaintData->Luma4LumaPostBlurYKernelVector.resize(2*InpaintData->Luma4LumaPostBlurYKernelSize+1);
      }
      catch (std::bad_alloc&)  { ErrorText = "InpaintLogo: Could not allocate memory"; }
    }
    if (InpaintData->ChromaChannelCount || (InpaintData->ChromaWeight!=0.0))
    {
      InpaintData->ChromaPreBlurXKernelSize = static_cast<int>(std::max<double>(std::floor(PreBlurSize/InpaintData->PixelWidth/InpaintData->ChromaPixelWidth),InpaintMinPreBlurXYKernelSize));
      InpaintData->ChromaPreBlurYKernelSize = static_cast<int>(std::max<double>(std::floor(PreBlurSize/InpaintData->PixelHeight/InpaintData->ChromaPixelHeight),InpaintMinPreBlurXYKernelSize));
      try
      {
        InpaintData->ChromaPreBlurXKernelVector.resize(2*InpaintData->ChromaPreBlurXKernelSize+1);
        InpaintData->ChromaPreBlurYKernelVector.resize(2*InpaintData->ChromaPreBlurYKernelSize+1);
      }
      catch (std::bad_alloc&)  { ErrorText = "InpaintLogo: Could not allocate memory"; }
    }
    if (InpaintData->ChromaTensor || (InpaintData->ChromaWeight!=0.0))
    {
      InpaintData->Chroma4ChromaPostBlurXKernelSize = static_cast<int>(std::max<double>(std::floor(PostBlurSize/InpaintData->PixelWidth/InpaintData->ChromaPixelWidth+0.5),InpaintMinPostBlurXYKernelSize));
      InpaintData->Chroma4ChromaPostBlurYKernelSize = static_cast<int>(std::max<double>(std::floor(PostBlurSize/InpaintData->PixelHeight/InpaintData->ChromaPixelHeight+0.5),InpaintMinPostBlurXYKernelSize));
      try
      {
        InpaintData->Chroma4ChromaPostBlurXKernelVector.resize(2*InpaintData->Chroma4ChromaPostBlurXKernelSize+1);
        InpaintData->Chroma4ChromaPostBlurYKernelVector.resize(2*InpaintData->Chroma4ChromaPostBlurYKernelSize+1);
      }
      catch (std::bad_alloc&)  { ErrorText = "InpaintLogo: Could not allocate memory"; }
    }
    if (InpaintData->ChromaTensor && (InpaintData->LumaWeight!=0.0))
    {
      InpaintData->Luma4ChromaPostBlurXKernelSize = static_cast<int>(std::max<double>(std::floor(PostBlurSize/InpaintData->PixelWidth+((InpaintData->ChromaPixelWidth%2)?0.0:0.5)),InpaintMinPostBlurXYKernelSize));
      InpaintData->Luma4ChromaPostBlurYKernelSize = static_cast<int>(std::max<double>(std::floor(PostBlurSize/InpaintData->PixelHeight+((InpaintData->ChromaPixelHeight%2)?0.0:0.5)),InpaintMinPostBlurXYKernelSize));
      try
      {
        InpaintData->Luma4ChromaPostBlurXKernelVector.resize(2*InpaintData->Luma4ChromaPostBlurXKernelSize+1);
        InpaintData->Luma4ChromaPostBlurYKernelVector.resize(2*InpaintData->Luma4ChromaPostBlurYKernelSize+1);
      }
      catch (std::bad_alloc&)  { ErrorText = "InpaintLogo: Could not allocate memory"; }
    }
    if (InpaintData->ChromaWeight!=0.0)
    {
      InpaintData->Chroma4LumaPostBlurXKernelSize = static_cast<int>(std::max<double>(std::floor(PostBlurSize/InpaintData->PixelWidth/InpaintData->ChromaPixelWidth+0.5-(InpaintData->ChromaPixelWidth%2?0.0:0.5/InpaintData->ChromaPixelWidth)),InpaintMinPostBlurXYKernelSize));
      InpaintData->Chroma4LumaPostBlurYKernelSize = static_cast<int>(std::max<double>(std::floor(PostBlurSize/InpaintData->PixelHeight/InpaintData->ChromaPixelHeight+0.5-(InpaintData->ChromaPixelHeight%2?0.0:0.5/InpaintData->ChromaPixelHeight)),InpaintMinPostBlurXYKernelSize));
      try
      {
        InpaintData->Chroma4LumaPostBlurXKernelVectors.resize(InpaintData->ChromaPixelWidth);
        InpaintData->Chroma4LumaPostBlurYKernelVectors.resize(InpaintData->ChromaPixelHeight);
        InpaintData->Chroma4LumaPostBlurXKernels.resize(InpaintData->ChromaPixelWidth, nullptr);
        InpaintData->Chroma4LumaPostBlurYKernels.resize(InpaintData->ChromaPixelHeight, nullptr);
        for (k=0 ; k<InpaintData->ChromaPixelWidth ; k++)
          InpaintData->Chroma4LumaPostBlurXKernelVectors[k].resize(2*InpaintData->Chroma4LumaPostBlurXKernelSize+1);
        for (k=0 ; k<InpaintData->ChromaPixelHeight ; k++)
          InpaintData->Chroma4LumaPostBlurYKernelVectors[k].resize(2*InpaintData->Chroma4LumaPostBlurYKernelSize+1);
      }
      catch (std::bad_alloc&)  { ErrorText = "InpaintLogo: Could not allocate memory"; }
    }
    InpaintData->StaticMaskData = nullptr;
    if (InpaintData->ChromaPixelWidth*InpaintData->ChromaPixelHeight>255)  ErrorText = "InpaintLogo internal error: Chroma subsampling to strong";
  }
  if (!ErrorText)
  {
    /* Compute and store blur kernels */
    if (!InpaintData->LumaPreBlurXKernelVector.empty())  InpaintData->LumaPreBlurXKernel = CreateGaussKernel(InpaintData->LumaPreBlurXKernelVector.data(),InpaintData->LumaPreBlurXKernelSize,std::max<double>(PreBlur/InpaintData->PixelWidth,InpaintMinPreBlurXYStandardDeviation),0.0);
    if (!InpaintData->LumaPreBlurYKernelVector.empty())  InpaintData->LumaPreBlurYKernel = CreateGaussKernel(InpaintData->LumaPreBlurYKernelVector.data(),InpaintData->LumaPreBlurYKernelSize,std::max<double>(PreBlur/InpaintData->PixelHeight,InpaintMinPreBlurXYStandardDeviation),0.0);
    /* Most post blurring kernels are build for forward differences */
    if (!InpaintData->Luma4LumaPostBlurXKernelVector.empty())  InpaintData->Luma4LumaPostBlurXKernel = CreateGaussKernel(InpaintData->Luma4LumaPostBlurXKernelVector.data(),InpaintData->Luma4LumaPostBlurXKernelSize,std::max<double>(PostBlur/InpaintData->PixelWidth,InpaintMinPostBlurXYStandardDeviation),-0.5);
    if (!InpaintData->Luma4LumaPostBlurYKernelVector.empty())  InpaintData->Luma4LumaPostBlurYKernel = CreateGaussKernel(InpaintData->Luma4LumaPostBlurYKernelVector.data(),InpaintData->Luma4LumaPostBlurYKernelSize,std::max<double>(PostBlur/InpaintData->PixelHeight,InpaintMinPostBlurXYStandardDeviation),-0.5);
    if (!InpaintData->ChromaPreBlurXKernelVector.empty())  InpaintData->ChromaPreBlurXKernel = CreateGaussKernel(InpaintData->ChromaPreBlurXKernelVector.data(),InpaintData->ChromaPreBlurXKernelSize,std::max<double>(PreBlur/InpaintData->PixelWidth/InpaintData->ChromaPixelWidth,InpaintMinPreBlurXYStandardDeviation),0.0);
    if (!InpaintData->ChromaPreBlurYKernelVector.empty())  InpaintData->ChromaPreBlurYKernel = CreateGaussKernel(InpaintData->ChromaPreBlurYKernelVector.data(),InpaintData->ChromaPreBlurYKernelSize,std::max<double>(PreBlur/InpaintData->PixelHeight/InpaintData->ChromaPixelHeight,InpaintMinPreBlurXYStandardDeviation),0.0);
    if (!InpaintData->Chroma4ChromaPostBlurXKernelVector.empty())  InpaintData->Chroma4ChromaPostBlurXKernel = CreateGaussKernel(InpaintData->Chroma4ChromaPostBlurXKernelVector.data(),InpaintData->Chroma4ChromaPostBlurXKernelSize,std::max<double>(PostBlur/InpaintData->PixelWidth/InpaintData->ChromaPixelWidth,InpaintMinPostBlurXYStandardDeviation),-0.5);
    if (!InpaintData->Chroma4ChromaPostBlurYKernelVector.empty())  InpaintData->Chroma4ChromaPostBlurYKernel = CreateGaussKernel(InpaintData->Chroma4ChromaPostBlurYKernelVector.data(),InpaintData->Chroma4ChromaPostBlurYKernelSize,std::max<double>(PostBlur/InpaintData->PixelHeight/InpaintData->ChromaPixelHeight,InpaintMinPostBlurXYStandardDeviation),-0.5);
    /* Luma4Chroma kernel is centered in center pixel for odd ChPxSize, or in pixel left of center line for even ChPxSize */
    if (!InpaintData->Luma4ChromaPostBlurXKernelVector.empty())  InpaintData->Luma4ChromaPostBlurXKernel = CreateGaussKernel(InpaintData->Luma4ChromaPostBlurXKernelVector.data(),InpaintData->Luma4ChromaPostBlurXKernelSize,std::max<double>(PostBlur/InpaintData->PixelWidth,InpaintMinPostBlurXYStandardDeviation),(InpaintData->ChromaPixelWidth%2)?-0.5:0.0);
    if (!InpaintData->Luma4ChromaPostBlurYKernelVector.empty())  InpaintData->Luma4ChromaPostBlurYKernel = CreateGaussKernel(InpaintData->Luma4ChromaPostBlurYKernelVector.data(),InpaintData->Luma4ChromaPostBlurYKernelSize,std::max<double>(PostBlur/InpaintData->PixelHeight,InpaintMinPostBlurXYStandardDeviation),(InpaintData->ChromaPixelHeight%2)?-0.5:0.0);
    /* Chroma4Luma Kernels: Let k=0..ChPxSize-1 be number of luma pixel in chroma block. Kernel will be centered in luma pixel k, if: backward differences for 2k+1<ChPxSize, forward else */
    if (!InpaintData->Chroma4LumaPostBlurXKernelVectors.empty())  for (k=0 ; k<InpaintData->ChromaPixelWidth ; k++)  InpaintData->Chroma4LumaPostBlurXKernels[k] = CreateGaussKernel(InpaintData->Chroma4LumaPostBlurXKernelVectors[k].data(),InpaintData->Chroma4LumaPostBlurXKernelSize,std::max<double>(PostBlur/InpaintData->PixelWidth/InpaintData->ChromaPixelWidth,InpaintMinPostBlurXYStandardDeviation),(2.0*k+1.0)/2.0/InpaintData->ChromaPixelWidth-(2*k+1<InpaintData->ChromaPixelWidth?0.0:1.0));
    if (!InpaintData->Chroma4LumaPostBlurYKernelVectors.empty())  for (k=0 ; k<InpaintData->ChromaPixelHeight ; k++)  InpaintData->Chroma4LumaPostBlurYKernels[k] = CreateGaussKernel(InpaintData->Chroma4LumaPostBlurYKernelVectors[k].data(),InpaintData->Chroma4LumaPostBlurYKernelSize,std::max<double>(PostBlur/InpaintData->PixelHeight/InpaintData->ChromaPixelHeight,InpaintMinPostBlurXYStandardDeviation),(2.0*k+1.0)/2.0/InpaintData->ChromaPixelHeight-(2*k+1<InpaintData->ChromaPixelHeight?0.0:1.0));
  }
  if (!ErrorText && !InpaintData->MaskClip)
  {
    Frame = avs_get_frame(MaskClip,0);
    if (Frame)
    {
      switch (AvsInpaintPrepareMask(&InpaintData->StaticMaskData,Frame,InpaintData))
      {
        case InpaintMaskResult::FullMask :
        /* If error-switch is set, a full mask throws an error, otherwise this filter reduces to the do-nothing-filter */
        #if ((InpaintFullMaskError)>=1)
          ErrorText = "InpaintLogo: Mask is full";
        break;
        #endif
        case InpaintMaskResult::NoMask :
          /* To indicate no inpainting is needed anymore, we clear the StaticMaskData field */
        break;
        case InpaintMaskResult::GeneralError :
          ErrorText = "InpaintLogo: Mask preparation failed: Internal error";
        break;
        case InpaintMaskResult::FrameError :
          ErrorText = "InpaintLogo: Mask preparation failed: Could not process mask frame";
        break;
        case InpaintMaskResult::MemoryError :
          ErrorText = "InpaintLogo: Mask preparation failed: Out of memory";
        break;
        default :
          ErrorText = nullptr;
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
}
#endif  // ENABLE_AVISYNTH


// With Luma and Chroma SampleStep template
template<int LSS, int CSS>
static const char*  InpaintCoreImpl(const InpaintDataStruct *  InpaintData,  InpaintMaskDataStruct *  MaskData,  uint8_t *  FrameData[],  const int  FramePitch[])
{
  uint8_t * LumaData, * ChromaDatas[MaxChannelCount-1], * ChromaData;
  int  LumaPitch;
  const int * ChromaPitch;
  int  x, y, PixelNo, ChannelNo;
  double  CoherenceStrength, CoherenceDirectionX, CoherenceDirectionY, TensorXX, TensorXY, TensorYY;
  const char * ErrorText = nullptr;
  /* Values given by the user */
  double  PixelWidth2, PixelHeight2;
  double  LumaWeight, ChromaWeight;
  double  LumaRadiusX, LumaRadiusY, ChromaRadiusX, ChromaRadiusY, Sharpness;
  bool  ChromaTensor;
  /* Color space dependent values */
  int  ChromaPixelWidth, ChromaPixelHeight, ChromaChannelCount;
  /* Blurring stuff */
  double * LumaPreBlurXKernel, * LumaPreBlurYKernel, * ChromaPreBlurXKernel, * ChromaPreBlurYKernel;
  double * Luma4LumaPostBlurXKernel, * Luma4LumaPostBlurYKernel;
  double * Chroma4ChromaPostBlurXKernel, * Chroma4ChromaPostBlurYKernel;
  double * Luma4ChromaPostBlurXKernel, * Luma4ChromaPostBlurYKernel;
  double * const * Chroma4LumaPostBlurXKernels, * const * Chroma4LumaPostBlurYKernels;
  int  LumaPreBlurXKernelSize, LumaPreBlurYKernelSize, ChromaPreBlurXKernelSize, ChromaPreBlurYKernelSize;
  int  Luma4LumaPostBlurXKernelSize, Luma4LumaPostBlurYKernelSize;
  int  Chroma4ChromaPostBlurXKernelSize, Chroma4ChromaPostBlurYKernelSize;
  int  Luma4ChromaPostBlurXKernelSize, Luma4ChromaPostBlurYKernelSize;
  int  Chroma4LumaPostBlurXKernelSize, Chroma4LumaPostBlurYKernelSize;
  /* Given by mask */
  int  Width, Height, MaskPixelCount;
  unsigned int * MaskPixelOrder;
  std::vector<uint8_t> chiLumaVec, chiChromaVec;
  uint8_t * chiLuma = nullptr, * chiChroma = nullptr;
  std::vector<float> GchiLumaVec, GchiChromaVec, GchiuLumaVec;
  float * GchiLuma = nullptr, * GchiChroma = nullptr, * GchiuLuma = nullptr;
  std::vector<float> GchiuChromasVec[MaxChannelCount-1];
  float * GchiuChromas[MaxChannelCount];

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
  const int LumaSampleStep   = (LSS > 0) ? LSS : InpaintData->srcLayout.sampleStep[0];
  const int ChromaSampleStep = (CSS > 0) ? CSS : InpaintData->srcLayout.sampleStep[1];
  ChromaChannelCount = InpaintData->ChromaChannelCount;
  const bool isYUV             = InpaintData->isYUV;
  const bool isChromaSubsampled = InpaintData->isChromaSubsampled;
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
  Chroma4LumaPostBlurXKernels = InpaintData->Chroma4LumaPostBlurXKernels.data();
  Chroma4LumaPostBlurYKernels = InpaintData->Chroma4LumaPostBlurYKernels.data();
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
  MaskPixelOrder = MaskData->MaskPixelOrder.data();
  const int LumaArea   = Width * Height;
  const int CWidth     = ChromaPixelWidth  > 0 ? Width  / ChromaPixelWidth  : 0;
  const int CHeight    = ChromaPixelHeight > 0 ? Height / ChromaPixelHeight : 0;
  const int ChromaArea = CWidth * CHeight;
  /* Allocate temporary memory */
  for (ChannelNo=0 ; ChannelNo<MaxChannelCount-1 ; ChannelNo++)  GchiuChromas[ChannelNo] = nullptr;
  try
  {
    if (InpaintData->StaticMaskData)  { chiLumaVec.resize(LumaArea);  chiLuma = chiLumaVec.data(); }
    else  chiLuma = MaskData->chiLuma.data();
    if (LumaWeight!=0.0)
    {
      if (InpaintData->StaticMaskData)  { GchiLumaVec.resize(LumaArea);  GchiLuma = GchiLumaVec.data(); }
      else  GchiLuma = MaskData->GchiLuma.data();
      GchiuLumaVec.resize(LumaArea);  GchiuLuma = GchiuLumaVec.data();
    }
    if (InpaintData->StaticMaskData)  { chiChromaVec.resize(ChromaArea);  chiChroma = chiChromaVec.data(); }
    else  chiChroma = MaskData->chiChroma.data();
    if (ChromaWeight!=0.0)
    {
      if (InpaintData->StaticMaskData)  { GchiChromaVec.resize(ChromaArea);  GchiChroma = GchiChromaVec.data(); }
      else  GchiChroma = MaskData->GchiChroma.data();
      for (ChannelNo=0 ; ChannelNo<ChromaChannelCount ; ChannelNo++)
      {
        GchiuChromasVec[ChannelNo].resize(ChromaArea);
        GchiuChromas[ChannelNo] = GchiuChromasVec[ChannelNo].data();
      }
    }
  }
  catch (std::bad_alloc&)  { ErrorText = "InpaintLogo: Could not allocate memory"; }
  if (ErrorText)  return  ErrorText;
  /* Prepare pointers */
  LumaPitch = FramePitch[0];
  ChromaPitch = FramePitch+1;
  LumaData = FrameData[0]+LumaPitch*MaskData->OffsetY+MaskData->OffsetX*LumaSampleStep;
  for (ChannelNo=0 ; ChannelNo<ChromaChannelCount ; ChannelNo++)  ChromaDatas[ChannelNo] = FrameData[ChannelNo+1]+MaskData->OffsetY/ChromaPixelHeight*FramePitch[ChannelNo+1]+MaskData->OffsetX/ChromaPixelWidth*ChromaSampleStep;
  /* Prepare blurred images */
  if (InpaintData->StaticMaskData)
  {
    std::memcpy(chiLuma,MaskData->chiLuma.data(),sizeof(uint8_t)*LumaArea);
    if(ChromaChannelCount)
      std::memcpy(chiChroma,MaskData->chiChroma.data(),sizeof(uint8_t)*ChromaArea);
    if (LumaWeight!=0.0)  std::memcpy(GchiLuma,MaskData->GchiLuma.data(),sizeof(float)*LumaArea);
    if (ChromaWeight!=0.0)  std::memcpy(GchiChroma,MaskData->GchiChroma.data(),sizeof(float)*ChromaArea);
  }
  if (LumaWeight!=0.0)
  {
    std::fill(GchiuLuma, GchiuLuma + LumaArea, 0.0f);

    for (int y = 0; y < Height; ++y)
    {
      // Hoist pointers to the start of the row
      const uint8_t* maskRow = &chiLuma[y * Width];
      const uint8_t* dataRow = &LumaData[y * LumaPitch];

      for (int x = 0; x < Width; ++x)
      {
        if (!maskRow[x])
        {
          AddPreBlurValue(GchiuLuma, x, y, Width, Height,
            dataRow[x * LumaSampleStep],
            LumaPreBlurXKernel, LumaPreBlurXKernelSize,
            LumaPreBlurYKernel, LumaPreBlurYKernelSize);
        }
      }
    }
  }
  if (ChromaWeight!=0.0)
  {
    for (int ChannelNo = 0; ChannelNo < ChromaChannelCount; ChannelNo++)
    {
      float* GchiuC = GchiuChromas[ChannelNo];
      const uint8_t* cData = ChromaDatas[ChannelNo];
      const uint8_t* cMask = chiChroma; // Assuming chiChroma is shared or indexed
      const int cPitch = ChromaPitch[ChannelNo];

      std::fill(GchiuC, GchiuC + ChromaArea, 0.0f);

      for (int y = 0; y < CHeight; y++)
      {
        // Hoist pointers to the start of the row
        const uint8_t* dataRow = &cData[cPitch * y];
        const uint8_t* maskRow = &cMask[y * CWidth];
        for (int x = 0; x < CWidth; x++)
        {
          if (!maskRow[x])
          {
            AddPreBlurValue(GchiuC, x, y, CWidth, CHeight,
              dataRow[x * ChromaSampleStep],
              ChromaPreBlurXKernel, ChromaPreBlurXKernelSize,
              ChromaPreBlurYKernel, ChromaPreBlurYKernelSize);
          }
        }
      }
    }
  }
  /** MAIN LOOP **/
  for (PixelNo=0 ; PixelNo<MaskPixelCount ; PixelNo++)
  {
    x = MaskPixelOrder[PixelNo]%Width;
    y = MaskPixelOrder[PixelNo]/Width;
    if(ChromaChannelCount)
      GetStructureTensor(&TensorXX,&TensorXY,&TensorYY,
                         GchiLuma,GchiuLuma,chiLuma,LumaWeight,
                         GchiChroma,GchiuChromas,chiChroma,ChromaWeight,
                         ChromaPixelWidth,ChromaPixelHeight,ChromaChannelCount,
                         x,y,Width,Height,
                         Luma4LumaPostBlurXKernel,Luma4LumaPostBlurXKernelSize,1,
                         Luma4LumaPostBlurYKernel,Luma4LumaPostBlurYKernelSize,1,
                         (ChromaWeight!=0.0)?Chroma4LumaPostBlurXKernels[x%ChromaPixelWidth]:nullptr,Chroma4LumaPostBlurXKernelSize,2*(x%ChromaPixelWidth)+1<ChromaPixelWidth?(-1):(+1),
                         (ChromaWeight!=0.0)?Chroma4LumaPostBlurYKernels[y%ChromaPixelHeight]:nullptr,Chroma4LumaPostBlurYKernelSize,2*(y%ChromaPixelHeight)+1<ChromaPixelHeight?(-1):(+1));
    else
      GetStructureTensor(&TensorXX, &TensorXY, &TensorYY,
        GchiLuma, GchiuLuma, chiLuma, LumaWeight,
        GchiChroma, GchiuChromas, chiChroma, ChromaWeight,
        ChromaPixelWidth, ChromaPixelHeight, ChromaChannelCount,
        x, y, Width, Height,
        Luma4LumaPostBlurXKernel, Luma4LumaPostBlurXKernelSize, 1,
        Luma4LumaPostBlurYKernel, Luma4LumaPostBlurYKernelSize, 1,
        nullptr, 0, 1 /*n/a*/,
        nullptr, 0, 1 /*n/a*/);
    CoherenceStrength = GetCoherenceData(&CoherenceDirectionX,&CoherenceDirectionY,TensorXX/PixelWidth2,TensorXY,TensorYY/PixelHeight2,Sharpness);
    int iValue = InpaintPixelImpl<LSS>(LumaRadiusX,LumaRadiusY,LumaData,chiLuma,LumaSampleStep,LumaPitch,x,y,Width,Height,CoherenceStrength,CoherenceDirectionX,CoherenceDirectionY);
    /* What to do if inpainting failed ?? */
    if (iValue<0)
    {
      #if (defined(InpaintFailError) && ((InpaintFailError)!=0))
      ErrorText = isYUV?"InpaintLogo internal error: inpainting luma pixel failed":"InpaintLogo internal error: inpainting first color channel pixel failed";
      break;
      #else
      iValue = LumaData[LumaPitch*y+x*LumaSampleStep];
      #endif
    }
    LumaData[LumaPitch*y+x*LumaSampleStep] = static_cast<uint8_t>(iValue);
    if (GchiuLuma)  AddPreBlurValue(GchiuLuma,x,y,Width,Height,iValue,LumaPreBlurXKernel,LumaPreBlurXKernelSize,LumaPreBlurYKernel,LumaPreBlurYKernelSize);
    if (GchiLuma)  AddPreBlurValue(GchiLuma,x,y,Width,Height,1,LumaPreBlurXKernel,LumaPreBlurXKernelSize,LumaPreBlurYKernel,LumaPreBlurYKernelSize);
    chiLuma[MaskPixelOrder[PixelNo]] = 0;
    if (!ChromaChannelCount)  continue;
    x /= ChromaPixelWidth;
    y /= ChromaPixelHeight;
    if (chiChroma[CWidth*y+x]==1)
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
        int iValue = InpaintPixelImpl<CSS>(ChromaRadiusX,ChromaRadiusY,ChromaData,chiChroma,ChromaSampleStep,ChromaPitch[ChannelNo],x,y,CWidth,CHeight,CoherenceStrength,CoherenceDirectionX,CoherenceDirectionY);
        if (iValue<0)
        {
          /* When inpainting fails on a subsampled channel, then the mask (almost certainly) contains to small holes (not covered by chroma resolution). If otherwise, we have a serious problem with our code (as above in the luma case). */
          #if (defined(InpaintFailError) && ((InpaintFailError)!=0) && defined(InpaintChromaFailError) && ((InpaintChromaFailError)!=0))
          ErrorText = (isYUV && isChromaSubsampled)?"InpaintLogo: inpainting chroma pixel failed (maybe mask contains a small hole not resolved by chroma subsampling, or internal error)":"InpaintLogo internal error: inpainting higher color channel pixel failed";
          break;
          #else
          #if (defined(InpaintChromaFailError) && ((InpaintChromaFailError)!=0))
          if (isYUV && isChromaSubsampled)
          {
            ErrorText = "InpaintLogo: inpainting chroma pixel failed (maybe mask contains a small hole not resolved by chroma subsampling, or internal error)";
            break;
          }
          else
          #elif (defined(InpaintFailError) && ((InpaintFailError)!=0))
          if (!(isYUV && isChromaSubsampled))
          {
            ErrorText = "InpaintLogo internal error: inpainting higher color channel pixel failed";
            break;
          }
          else
          #endif
          {
            iValue = ChromaData[ChromaPitch[ChannelNo]*y+x*ChromaSampleStep];
          }
          #endif
        }
        ChromaData[ChromaPitch[ChannelNo]*y+x*ChromaSampleStep] = static_cast<uint8_t>(iValue);
        if (GchiuChromas[ChannelNo])  AddPreBlurValue(GchiuChromas[ChannelNo],x,y,CWidth,CHeight,iValue,ChromaPreBlurXKernel,ChromaPreBlurXKernelSize,ChromaPreBlurYKernel,ChromaPreBlurYKernelSize);
      }
      if (ErrorText)  break;
      if (GchiChroma)  AddPreBlurValue(GchiChroma,x,y,CWidth,CHeight,1,ChromaPreBlurXKernel,ChromaPreBlurXKernelSize,ChromaPreBlurYKernel,ChromaPreBlurYKernelSize);
    }
    if (chiChroma[CWidth*y+x])  chiChroma[CWidth*y+x]--;
  }
  return  ErrorText;
}



#ifdef ENABLE_AVISYNTH
AVS_VideoFrame *  AVSC_CC  Inpaint_GetFrame(AVS_FilterInfo *  FilterInfo,  int  FrameNo)
{
  InpaintDataStruct * InpaintData;
  InpaintMaskDataStruct * MaskData;
  AVS_VideoFrame * Frame;
  uint8_t * FrameData[MaxChannelCount];
  int  FramePitch[MaxChannelCount];
  const char * ErrorText;

  InpaintData = static_cast<InpaintDataStruct*>(FilterInfo->user_data);
  /* If the logo is dynamic, we prepare the mask stuff */
  MaskData = InpaintData->StaticMaskData;
  if (!MaskData)
  {
    ErrorText = nullptr;
    Frame = avs_get_frame(InpaintData->MaskClip,(FrameNo<InpaintData->MaskFrameCount)?FrameNo:(InpaintData->MaskFrameCount-1));
    if (!Frame)  ErrorText = "InpaintLogo: Could not get mask frame";
    if (!ErrorText)
    {
      switch (AvsInpaintPrepareMask(&MaskData,Frame,InpaintData))
      {
        case InpaintMaskResult::FullMask :
        #if ((InpaintFullMaskError)>=2)
          ErrorText = "InpaintLogo: Mask is full";
        break;
        #endif
        case InpaintMaskResult::NoMask :
          /* (still) no mask data indicates nothing to do */
          break;
        case InpaintMaskResult::GeneralError :
          ErrorText = "InpaintLogo: Mask preparation failed: Internal error";
        break;
        case InpaintMaskResult::FrameError :
          ErrorText = "InpaintLogo: Mask preparation failed: Could not process mask frame";
        break;
        case InpaintMaskResult::MemoryError :
          ErrorText = "InpaintLogo: Mask preparation failed: Out of memory";
        break;
        default :
          ErrorText = nullptr;
        break;
      }
    }
    if (Frame)  avs_release_video_frame(Frame);
    if (ErrorText)
    {
      FilterInfo->error = ErrorText;
      delete MaskData;  MaskData = nullptr;
      return  nullptr;
    }
  }
  /* Get video frame */
  Frame = avs_get_frame(FilterInfo->child,FrameNo);
  if (!(MaskData&&Frame))  /* No mask data? Then we just return the source frame */
  {
    if (!InpaintData->StaticMaskData)  { delete MaskData;  MaskData = nullptr; }
    if (!Frame)  FilterInfo->error = "InpaintLogo: Could not get video frame";
    return  Frame;
  }
  avs_make_writable(FilterInfo->env,&Frame);
  if (!AvsGetFramePlanes(Frame,InpaintData->srcLayout,FrameData,FramePitch) || !FrameData[0])
  {
    if (!InpaintData->StaticMaskData)  { delete MaskData;  MaskData = nullptr; }
    avs_release_video_frame(Frame);
    FilterInfo->error = "InpaintLogo: Could not get pixel data pointers";
    return  nullptr;
  }
  ErrorText = InpaintCore(InpaintData, MaskData, FrameData, FramePitch);
  if (!InpaintData->StaticMaskData)  { delete MaskData;  MaskData = nullptr; }
  if (ErrorText)
  {
    avs_release_video_frame(Frame);
    FilterInfo->error = ErrorText;
    return  nullptr;
  }
  return  Frame;
}



void  AVSC_CC  Inpaint_Free(AVS_FilterInfo *  FilterInfo)
{
  InpaintDataStruct * InpaintData;

  if (FilterInfo->user_data)
  {
    InpaintData = static_cast<InpaintDataStruct*>(FilterInfo->user_data);
    if (InpaintData->StaticMaskData)  { delete InpaintData->StaticMaskData;  InpaintData->StaticMaskData = nullptr; }
    if (InpaintData->MaskClip)  avs_release_clip(InpaintData->MaskClip);
    delete InpaintData;
  }
  FilterInfo->user_data = nullptr;
}
#endif  // ENABLE_AVISYNTH


#ifdef ENABLE_AVISYNTH
/****************************************************************
                       Deblend callbacks
 ****************************************************************/
AVS_Value  AVSC_CC  Deblend_Create(AVS_ScriptEnvironment *  Env,  AVS_Value  Args,  void *  Data)
{
  AVS_Value  SrcClipValue, DestClipValue;
  AVS_Clip * SrcClip, * LogoClip, * AlphaClip, * DestClip;
  const  AVS_VideoInfo * SrcVideoInfo, * LogoVideoInfo, * AlphaVideoInfo;
  AVS_VideoFrame * LogoFrame, * AlphaFrame;
  int  LogoPixelWidth[MaxChannelCount], LogoPixelHeight[MaxChannelCount];
  AVS_FilterInfo * FilterInfo;
  DeblendDataStruct * DeblendData = nullptr;
  int  ChannelCount, ChannelNo;
  const char * ErrorText;

  /* We get all input clips and video info pointers */
  SrcClipValue = avs_array_elt(Args,0);
  SrcClip = avs_take_clip(SrcClipValue,Env);
  LogoClip = avs_take_clip(avs_array_elt(Args,1),Env);
  AlphaClip = avs_is_clip(avs_array_elt(Args,2))?avs_take_clip(avs_array_elt(Args,2),Env):nullptr;
  SrcVideoInfo = avs_get_video_info(SrcClip);
  LogoVideoInfo = avs_get_video_info(LogoClip);
  AlphaVideoInfo = AlphaClip?avs_get_video_info(AlphaClip):nullptr;
  const int  AlphaChannel = AlphaVideoInfo ? AvsMaskChannel(AlphaVideoInfo->pixel_type) : 0;
  /* Check if all color spaces and dimensions and so on are correct */
  ErrorText = nullptr;
  if (!(avs_has_video(SrcVideoInfo) && SrcVideoInfo->num_frames && SrcVideoInfo->width && SrcVideoInfo->height))  ErrorText = "Deblend: Source clip has no video";
  else if (!(avs_is_rgb24(SrcVideoInfo) || avs_is_rgb32(SrcVideoInfo) || avs_is_yuy2(SrcVideoInfo) || avs_is_yv12(SrcVideoInfo) || avs_is_yv16(SrcVideoInfo) || avs_is_yv24(SrcVideoInfo) || avs_is_y8(SrcVideoInfo) || avs_is_planar_rgb(SrcVideoInfo) || avs_is_planar_rgba(SrcVideoInfo)))  ErrorText = "Deblend: Color space unknown (RGB24, RGB32, RGBP, RGBAP, YUY2, YV12/16/24/8 allowed only)";
  else if (!(avs_is_same_colorspace(SrcVideoInfo,LogoVideoInfo) || (avs_is_rgb(SrcVideoInfo) && avs_is_rgb(LogoVideoInfo))))  ErrorText = "Deblend: Source clip and logo clip must have same color spaces (combinations of RGB / RGBA are allowed)";
  else if (!(avs_has_video(LogoVideoInfo) && LogoVideoInfo->num_frames && LogoVideoInfo->width && LogoVideoInfo->height))  ErrorText = "Deblend: Logo clip has no video";
  else if (!((SrcVideoInfo->width==LogoVideoInfo->width) && (SrcVideoInfo->height==LogoVideoInfo->height)))  ErrorText = "Deblend: Source clip and logo clip have different dimensions";
  else if (!(AlphaClip || avs_is_rgb32(LogoVideoInfo) || avs_is_planar_rgba(LogoVideoInfo)))  ErrorText = "Deblend: Alpha clip is missing (needed if logo clip is not RGB32 or RGBAP)";
  else if (AlphaClip && !(avs_has_video(AlphaVideoInfo) && AlphaVideoInfo->num_frames))  ErrorText = "Deblend: Alpha clip has no video";
  else if (AlphaClip && !(avs_is_yuy2(AlphaVideoInfo) || avs_is_yv12(AlphaVideoInfo) || avs_is_yv16(AlphaVideoInfo) || avs_is_yv24(AlphaVideoInfo) || avs_is_y8(AlphaVideoInfo) || avs_is_rgb32(AlphaVideoInfo) || avs_is_planar_rgba(AlphaVideoInfo)))  ErrorText = "Deblend: Alpha color space must have luma channel (YUY2 or YV12/16/24/8) or alpha channel (RGBA, RGBAP)";
  else if (AlphaClip && !((AlphaVideoInfo->width==SrcVideoInfo->width) && (AlphaVideoInfo->height==SrcVideoInfo->height)))  ErrorText = "Deblend: Source clip and alpha clip have different dimensions";
  /* Everything seems to be fine */
  /* Data structure is allocated */
  else
  {
    DeblendData = new (std::nothrow) DeblendDataStruct;
    if (DeblendData)
    {
      DeblendData->PixelType = SrcVideoInfo->pixel_type;
      DeblendData->Width = SrcVideoInfo->width;
      DeblendData->Height = SrcVideoInfo->height;
      DeblendData->LogoPixelType = LogoVideoInfo->pixel_type;
      DeblendData->AlphaPixelType = AlphaVideoInfo?AlphaVideoInfo->pixel_type:0;
      DeblendData->LogoFrameCount = LogoVideoInfo->num_frames;
      DeblendData->AlphaFrameCount = AlphaVideoInfo?AlphaVideoInfo->num_frames:0;
      // only packed RGB is upside down, planar is not.
      DeblendData->FlipLogo = AvsIsBottomUp(LogoVideoInfo) != AvsIsBottomUp(SrcVideoInfo);
      DeblendData->FlipAlpha = AlphaVideoInfo ? (AvsIsBottomUp(AlphaVideoInfo) != AvsIsBottomUp(SrcVideoInfo)) : false;
      DeblendData->layout = FrameLayoutFromPixelType(SrcVideoInfo->pixel_type);
      DeblendData->ChromaLoc = AvsGetChromaLocation(Env, SrcClip, avs_array_elt(Args,3),
        "Deblend: ChromaPlacement must be \"auto\", \"left\" (\"MPEG2\"), \"center\" (\"MPEG1\", \"jpeg\"), \"top_left\", \"top\", \"bottom_left\", \"bottom\" or \"DV\"", &ErrorText);
    }
    else  ErrorText = "Deblend: Could not allocate memory";
  }
  if (!ErrorText && (LogoVideoInfo->num_frames==1) && (!AlphaClip || (AlphaVideoInfo->num_frames==1)))
  {
    LogoFrame = nullptr;
    AlphaFrame = nullptr;
    if (!ErrorText)
    {
      /* Logo and alpha clip will be evaluated only once - no cache is needed */
      avs_set_cache_hints(LogoClip,AVS_CACHE_NOTHING,0);
      if (AlphaClip)  avs_set_cache_hints(AlphaClip,AVS_CACHE_NOTHING,0);
      /* Get first frame */
      LogoFrame = avs_get_frame(LogoClip,0);
      AlphaFrame = AlphaClip?avs_get_frame(AlphaClip,0):nullptr;
      if (!LogoFrame || (AlphaClip && !AlphaFrame))  ErrorText = "Deblend: Could not get logo/alpha frame";
    }
    if (!ErrorText)
    {
      /* How many channels need to be processed (should be 3, or 4 if alpha channels is part of the logo) */
      {
        const FrameLayout  logoFmt = FrameLayoutFromPixelType(LogoVideoInfo->pixel_type);
        ChannelCount = AlphaClip ? logoFmt.channelCount      /* alpha from separate clip: all logo channels are color */
                                 : logoFmt.channelCount - 1; /* alpha embedded in last logo channel: skip it */
        for (int i=0 ; i<logoFmt.channelCount ; i++)  { LogoPixelWidth[i] = logoFmt.pixelWidth[i];  LogoPixelHeight[i] = logoFmt.pixelHeight[i]; }
      }
      /* Memory for static logo data is allocated */
      try
      {
        for (ChannelNo=0 ; ChannelNo<ChannelCount ; ChannelNo++)
          DeblendData->StaticLogoDataChannels[ChannelNo].resize(LogoVideoInfo->width*LogoVideoInfo->height/LogoPixelWidth[ChannelNo]/LogoPixelHeight[ChannelNo]);
        DeblendData->StaticAlphaDataChannels[0].resize(LogoVideoInfo->width*LogoVideoInfo->height);
        if (avs_is_yuy2(SrcVideoInfo) || avs_is_yv12(SrcVideoInfo) || avs_is_yv16(SrcVideoInfo) || avs_is_yv24(SrcVideoInfo) || avs_is_y8(SrcVideoInfo))
        {
          DeblendData->StaticAlphaDataChannels[1].resize(LogoVideoInfo->width*LogoVideoInfo->height);
          /* DV siting: V has its own downsampled alpha */
          if (DeblendData->ChromaLoc == ChromaLoc_DV)  DeblendData->StaticAlphaDataChannels[2].resize(LogoVideoInfo->width*LogoVideoInfo->height);
        }
      }
      catch (std::bad_alloc&)  { ErrorText = "Deblend: Could not allocate memory"; }
    }
    if (!ErrorText)
    {
      /* Core function for static logos is called */
      uint16_t * logoPtrs[MaxChannelCount] = {};
      uint8_t * alphaPtrs[MaxChannelCount] = {};
      for (ChannelNo=0 ; ChannelNo<MaxChannelCount ; ChannelNo++)
      {
        logoPtrs[ChannelNo] = DeblendData->StaticLogoDataChannels[ChannelNo].empty() ? nullptr : DeblendData->StaticLogoDataChannels[ChannelNo].data();
        alphaPtrs[ChannelNo] = DeblendData->StaticAlphaDataChannels[ChannelNo].empty() ? nullptr : DeblendData->StaticAlphaDataChannels[ChannelNo].data();
      }
      if (0>AvsDeblendStaticPrepare(logoPtrs,alphaPtrs,LogoFrame,AlphaFrame,LogoVideoInfo,AlphaVideoInfo,AlphaChannel,DeblendData->FlipLogo,DeblendData->FlipAlpha,DeblendData->ChromaLoc))  ErrorText = "Deblend: Could not gather frame data pointers";
    }
    if (AlphaFrame)  avs_release_video_frame(AlphaFrame);
    if (LogoFrame)  avs_release_video_frame(LogoFrame);
    if (AlphaClip)  avs_release_clip(AlphaClip);
    if (LogoClip)  avs_release_clip(LogoClip);
    LogoClip = nullptr;
    AlphaClip = nullptr;
  }
  else
  {
    if (!ErrorText)
    {
      DeblendData->logoLayout = FrameLayoutFromPixelType(LogoVideoInfo->pixel_type);
      if (AlphaClip)
      {
        const FrameLayout  alphaLayout = FrameLayoutFromPixelType(AlphaVideoInfo->pixel_type);
        DeblendData->AlphaChannelNo = AlphaChannel;
        DeblendData->AlphaSampleStep = alphaLayout.sampleStep[AlphaChannel];
      }
      else  DeblendData->AlphaSampleStep = DeblendData->logoLayout.sampleStep[3];  /* If this happens, LogoClip *must* be RGBA */
      DeblendData->LogoClip = LogoClip;
      DeblendData->AlphaClip = AlphaClip;
    }
  }
  if (SrcClip)  avs_release_clip(SrcClip);
  DestClip = nullptr;
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
  if (DeblendData)  delete DeblendData;
  if (AlphaClip)  avs_release_clip(AlphaClip);
  if (LogoClip)  avs_release_clip(LogoClip);
  return  avs_new_value_error(ErrorText);
}



AVS_VideoFrame *  AVSC_CC  Deblend_Static_GetFrame(AVS_FilterInfo *  FilterInfo,  int  FrameNo)
{
  DeblendDataStruct * DeblendData;
  AVS_VideoFrame * Frame;
  int  ChannelCount, Width, Height, FramePitch[MaxChannelCount];
  const int  * FrameSampleStep, * FramePixelWidth, * FramePixelHeight;
  uint8_t * FrameData[MaxChannelCount], * AlphaData;
  int  ChannelNo;

  /* Get some data from the data structure, load frames and get data pointers */
  DeblendData = static_cast<DeblendDataStruct*>(FilterInfo->user_data);
  Width = DeblendData->Width;
  Height = DeblendData->Height;
  FrameSampleStep    = DeblendData->layout.sampleStep;
  FramePixelWidth  = DeblendData->layout.pixelWidth;
  FramePixelHeight = DeblendData->layout.pixelHeight;
  Frame = avs_get_frame(FilterInfo->child,FrameNo);
  avs_make_writable(FilterInfo->env,&Frame);
  ChannelCount = DeblendData->layout.channelCount;
  if (!AvsGetFramePlanes(Frame,DeblendData->layout,FrameData,FramePitch))
  {
    FilterInfo->error = Frame?"Deblend: Could not get source data pointers":"Deblend: Could not get source frame";
    if (Frame)  avs_release_video_frame(Frame);
    return  nullptr;
  }
  AlphaData = nullptr;
  for (ChannelNo=0 ; ChannelNo<ChannelCount ; ChannelNo++)
  {
    /* Logo with embedded alpha: the source's alpha channel has no logo data and is left untouched */
    if (DeblendData->StaticLogoDataChannels[ChannelNo].empty())  break;
    if (!DeblendData->StaticAlphaDataChannels[ChannelNo].empty())  AlphaData = DeblendData->StaticAlphaDataChannels[ChannelNo].data();
    DeblendStatic(FrameData[ChannelNo],DeblendData->StaticLogoDataChannels[ChannelNo].data(),AlphaData,FramePitch[ChannelNo],FrameSampleStep[ChannelNo],Width/FramePixelWidth[ChannelNo],Height/FramePixelHeight[ChannelNo]);
  }
  return  Frame;
}



AVS_VideoFrame *  AVSC_CC  Deblend_Dynamic_GetFrame(AVS_FilterInfo *  FilterInfo,  int  FrameNo)
{
  DeblendDataStruct * DeblendData;
  AVS_VideoFrame * Frame, * LogoFrame, * AlphaFrame;
  int  ChannelCount, Width, Height, FramePitch[MaxChannelCount];
  const int  * FrameSampleStep, * FramePixelWidth, * FramePixelHeight;
  int  LogoChannelCount, LogoPixelType, LogoFramePitch[MaxChannelCount];
  const int  * LogoFrameSampleStep;
  int  AlphaChannelCount, AlphaPixelType, AlphaFramePitch[MaxChannelCount], AlphaPitch, AlphaSampleStep;
  uint8_t * FrameData[MaxChannelCount];
  const  uint8_t * LogoFrameData[MaxChannelCount], * AlphaFrameData[MaxChannelCount], * AlphaData;
  int  ChannelNo;
  const char * ErrorText;

  /* Get some data from the data structure, load frames and get data pointers */
  DeblendData = static_cast<DeblendDataStruct*>(FilterInfo->user_data);
  Width = DeblendData->Width;
  Height = DeblendData->Height;
  FrameSampleStep    = DeblendData->layout.sampleStep;
  FramePixelWidth  = DeblendData->layout.pixelWidth;
  FramePixelHeight = DeblendData->layout.pixelHeight;
  LogoFrameSampleStep  = DeblendData->logoLayout.sampleStep;
  AlphaSampleStep = DeblendData->AlphaSampleStep;
  Frame = avs_get_frame(FilterInfo->child,FrameNo);
  avs_make_writable(FilterInfo->env,&Frame);
  ChannelCount = AvsGetFramePlanes(Frame,DeblendData->layout,FrameData,FramePitch) ? DeblendData->layout.channelCount : 0;
  LogoPixelType = DeblendData->LogoPixelType;
  LogoFrame = avs_get_frame(DeblendData->LogoClip,(FrameNo<DeblendData->LogoFrameCount)?FrameNo:(DeblendData->LogoFrameCount-1));
  LogoChannelCount = AvsGetFramePlanesRO(LogoFrame,DeblendData->logoLayout,LogoFrameData,LogoFramePitch) ? DeblendData->logoLayout.channelCount : 0;
  if (DeblendData->FlipLogo)
  {
    for (ChannelNo=0 ; ChannelNo<LogoChannelCount ; ChannelNo++)
    {
      LogoFrameData[ChannelNo] += static_cast<ptrdiff_t>(Height/DeblendData->logoLayout.pixelHeight[ChannelNo] - 1) * LogoFramePitch[ChannelNo];
      LogoFramePitch[ChannelNo] *= -1;
    }
  }
  /* Maybe we need the alpha frame, maybe it's part of the logo (rgbA) */
  if (DeblendData->AlphaClip)
  {
    AlphaPixelType = DeblendData->AlphaPixelType;
    AlphaFrame = avs_get_frame(DeblendData->AlphaClip,(FrameNo<DeblendData->AlphaFrameCount)?FrameNo:(DeblendData->AlphaFrameCount-1));
    const FrameLayout  alphaFmt = FrameLayoutFromPixelType(AlphaPixelType);
    AlphaChannelCount = AvsGetFramePlanesRO(AlphaFrame,alphaFmt,AlphaFrameData,AlphaFramePitch) ? alphaFmt.channelCount : 0;
  }
  else
  {
    AlphaPixelType = 0;
    AlphaFrame = nullptr;
    AlphaChannelCount = 0;
  }
  /* If something failed (could not get frame or data pointers), we abort */
  ErrorText = nullptr;
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
  if (!ErrorText && DeblendData->AlphaClip && (AlphaChannelCount<=DeblendData->AlphaChannelNo))  ErrorText = "Deblend internal error: Alpha clip has no such plane";
  if (!ErrorText && (AlphaChannelCount==0) && (DeblendData->logoLayout.channelCount!=4))  ErrorText = "Deblend internal error: No alpha channel at all";
  if (ErrorText)
  {
    FilterInfo->error = ErrorText;
    if (AlphaFrame)  avs_release_video_frame(AlphaFrame);
    if (LogoFrame)  avs_release_video_frame(LogoFrame);
    if (Frame)  avs_release_video_frame(Frame);
    return  nullptr;
  }
  if (AlphaFrame)
  {
    AlphaData = AlphaFrameData[DeblendData->AlphaChannelNo];
    AlphaPitch = AlphaFramePitch[DeblendData->AlphaChannelNo];
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
  SubsampledAlphaCache  AlphaCache;
  for (ChannelNo=0 ; ChannelNo<ChannelCount ; ChannelNo++)
  {
    if (FramePixelWidth[ChannelNo]*FramePixelHeight[ChannelNo]==1)  DeblendDynamic(FrameData[ChannelNo],LogoFrameData[ChannelNo],AlphaData,FramePitch[ChannelNo],FrameSampleStep[ChannelNo],LogoFramePitch[ChannelNo],LogoFrameSampleStep[ChannelNo],AlphaPitch,AlphaSampleStep,Width,Height);
    else  DeblendDynamicSubsampled(FrameData[ChannelNo],LogoFrameData[ChannelNo],AlphaData,FramePitch[ChannelNo],FrameSampleStep[ChannelNo],LogoFramePitch[ChannelNo],LogoFrameSampleStep[ChannelNo],AlphaPitch,AlphaSampleStep,Width,Height,FramePixelWidth[ChannelNo],FramePixelHeight[ChannelNo],DeblendData->ChromaLoc,ChannelNo,AlphaCache);
  }
  if (AlphaFrame)  avs_release_video_frame(AlphaFrame);
  if (LogoFrame)  avs_release_video_frame(LogoFrame);
  return  Frame;
}



void  AVSC_CC  Deblend_Free(AVS_FilterInfo *  FilterInfo)
{
  DeblendDataStruct * DeblendData;

  if (FilterInfo->user_data)
  {
    DeblendData = static_cast<DeblendDataStruct*>(FilterInfo->user_data);
    if (DeblendData->AlphaClip)  avs_release_clip(DeblendData->AlphaClip);
    if (DeblendData->LogoClip)  avs_release_clip(DeblendData->LogoClip);
    delete DeblendData;
  }
}
#endif  // ENABLE_AVISYNTH



#ifdef ENABLE_AVISYNTH
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
  const  uint8_t * MaskData;
  int  MaskSampleStep, MaskPitch;
  bool  MaskGiven, ComputeAlpha;
  double  DeviationWeight, SubsamplingWeight;
  int32_t  MaskSize, AnalyzeResult;
  uint8_t * Mask;
  const char * ErrorText;
  /* For mask-processing */
  int  x, y;

  /* First all pointers are initialized */
  ErrorText = nullptr;
  MaskFrame = nullptr;
  MaskData = nullptr;
  DestClip = nullptr;
  Mask = nullptr;
  MaskPitch = 0;
  MaskSampleStep = 0;
  MaskData = nullptr;
  MaskSize = 0;
  DestFrame = nullptr;
  AnalyzeResult = 0;
  AVS_VideoInfo DestVideoInfoBuf = {};  DestVideoInfo = &DestVideoInfoBuf;
  ComputeAlpha = avs_is_bool(avs_array_elt(Args,2)) ? (avs_as_bool(avs_array_elt(Args,2)) != 0) : true;
  DeviationWeight = avs_is_float(avs_array_elt(Args,3))?avs_as_float(avs_array_elt(Args,3)):0.5;
  if (DeviationWeight<0.0)  return  avs_new_value_error("Analyze: Deviation weight must not be negative");
  SubsamplingWeight = avs_is_float(avs_array_elt(Args,4))?avs_as_float(avs_array_elt(Args,4)):0.5;
  if (SubsamplingWeight<0.0)  return  avs_new_value_error("Analyze: Subsampling weight must not be negative");

  SrcClipValue = avs_array_elt(Args,0);
  SrcClip = avs_take_clip(SrcClipValue,Env);
  MaskGiven = (avs_is_clip(avs_array_elt(Args,1)) != 0);
  MaskClip = MaskGiven?avs_take_clip(avs_array_elt(Args,1),Env):nullptr;
  SrcVideoInfo = avs_get_video_info(SrcClip);
  MaskVideoInfo = MaskGiven?avs_get_video_info(MaskClip):nullptr;
  const FrameLayout  maskFmt = FrameLayoutFromPixelType(MaskGiven ? MaskVideoInfo->pixel_type : SrcVideoInfo->pixel_type);
  const int  MaskChannel = AvsMaskChannel(MaskGiven ? MaskVideoInfo->pixel_type : SrcVideoInfo->pixel_type);
  if (!(avs_has_video(SrcVideoInfo) && SrcVideoInfo->num_frames))  ErrorText = "Analyze: Source clip has no video";
  else if (!(avs_is_rgb24(SrcVideoInfo) || avs_is_rgb32(SrcVideoInfo) || avs_is_yuy2(SrcVideoInfo) || avs_is_yv12(SrcVideoInfo) || avs_is_yv16(SrcVideoInfo) || avs_is_yv24(SrcVideoInfo) || avs_is_y8(SrcVideoInfo) || avs_is_planar_rgb(SrcVideoInfo) || avs_is_planar_rgba(SrcVideoInfo)))  ErrorText = "Analyze: Color space unknown (RGB24, RGB32, RGBP, RGBAP, YUY2, YV12/16/24/8 allowed only)";
  else if ((SrcVideoInfo->width==0) || (SrcVideoInfo->height==0) || (SrcVideoInfo->width==1 && SrcVideoInfo->height==1))  ErrorText = "Analyze: Source clip too small";
  else if (ComputeAlpha && (SrcVideoInfo->num_frames<2))  ErrorText = "Analyze: Source clip must have at least two frames for alpha computation";
  else if (!(MaskClip || avs_is_rgb32(SrcVideoInfo) || avs_is_planar_rgba(SrcVideoInfo)))  ErrorText = "Analyze: Mask is missing (needed if source clip is not RGB32 or RGBAP)";
  else if (MaskClip && !(avs_has_video(MaskVideoInfo) && MaskVideoInfo->num_frames))  ErrorText = "Analyze: Mask clip has no video";
  else if (MaskClip && !avs_is_yuy2(MaskVideoInfo) && !avs_is_yv12(MaskVideoInfo) && !avs_is_yv16(MaskVideoInfo) && !avs_is_yv24(MaskVideoInfo) && !avs_is_y8(MaskVideoInfo) && !avs_is_rgb32(MaskVideoInfo) && !avs_is_planar_rgba(MaskVideoInfo))  ErrorText = "Analyze: Mask color space must have luma channel (YUY2 or YV12/16/24/8) or alpha channel (RGB32, RGBAP)";
  else if (MaskClip && !(MaskVideoInfo->width==SrcVideoInfo->width && MaskVideoInfo->height==SrcVideoInfo->height))  ErrorText = "Analyze: Source clip and mask clip have different dimensions";
  /* Done! All arguments seem to be correct */
  /* Attempt to disable the frame cache for source and mask clips.
     Analyze() is a one-shot batch consumer: it walks all frames exactly once in sequential order
     and never requests the same frame twice, so caching frames would only waste memory.
     In classic Avisynth 2.5/2.6, calling set_cache_hints on an input clip directly was the
     established way to suppress caching of that clip's output.
     In Avisynth+, set_cache_hints is primarily intended to be called by a filter on itself
     (to declare its own caching needs to the Avisynth+ cache subsystem), not on an upstream
     input clip from within _Create.  Whether these calls have any practical effect in Avisynth+
     is uncertain.  Avisynth+'s own LRU eviction policy should limit memory use in practice
     because the strictly sequential, non-repeating access pattern leaves frames cold immediately. */
  if (!ErrorText)  avs_set_cache_hints(SrcClip,AVS_CACHE_NOTHING,0);
  if (!ErrorText && MaskClip)  avs_set_cache_hints(MaskClip,AVS_CACHE_NOTHING,0);
  /* Load the mask frame... */
  if (!ErrorText)
  {
    MaskFrame = avs_get_frame(MaskClip?MaskClip:SrcClip,0);
    if (!MaskFrame)  ErrorText = "Analyze: Could not get mask frame";
  }
  /* ...and get pointers to pixel data (without Mask, MaskChannel is the source alpha) */
  if (!ErrorText)
  {
    const uint8_t * maskPlanes[MaxChannelCount] = {};
    int  maskPitches[MaxChannelCount] = {};
    if (AvsGetFramePlanesRO(MaskFrame, maskFmt, maskPlanes, maskPitches))
    {
      MaskData = maskPlanes[MaskChannel];
      MaskPitch = maskPitches[MaskChannel];
      MaskSampleStep = maskFmt.sampleStep[MaskChannel];
    }
    if (!MaskData)  ErrorText = "Analyze: Could not gather pixel data pointer";
  }
  if (!ErrorText)
  {
    /* If source and mask have different vertical orientations (packed BGR is bottom-up, everything else top-down), flip the mask */
    if (MaskClip && ((avs_is_rgb(SrcVideoInfo) && !avs_is_planar(SrcVideoInfo)) != (avs_is_rgb(MaskVideoInfo) && !avs_is_planar(MaskVideoInfo))))
    {
      MaskData += (SrcVideoInfo->height-1)*MaskPitch;
      MaskPitch *= -1;
    }
  }
  /* Release mask clip */
  if (MaskClip)  avs_release_clip(MaskClip);
  /* Allocate mask memory */
  std::vector<uint8_t> MaskVec;
  if (!ErrorText)
  {
    try  { MaskVec.resize(SrcVideoInfo->width*SrcVideoInfo->height); }
    catch (std::bad_alloc&)  { ErrorText = "Analyze: Could not allocate memory"; }
    Mask = MaskVec.data();
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
          MaskSize += Mask[SrcVideoInfo->width*y+x] = ((MaskThresholdSign)*MaskData[MaskPitch*y+x*MaskSampleStep]>(MaskThresholdSign)*(MaskThreshold));
        }
      }
    }
    else
    {
      for (y=0 ; y<SrcVideoInfo->height ; y++)
      {
        for (x=0 ; x<SrcVideoInfo->width ; x++)
        {
          Mask[SrcVideoInfo->width*y+x] = MaskData[MaskPitch*y+x*MaskSampleStep];
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
    DestVideoInfo[0] = SrcVideoInfo[0];
    if (MaskGiven)  DestVideoInfo->height *= 2;
    DestFrame = avs_new_video_frame(Env,DestVideoInfo);
    if (!DestFrame)  ErrorText = "Analyze: Target frame could not be created";
  }
  /* MAIN PART: Calls core function (analyze), which evaluates all source clip frames and determines logo frame */
  int  ChromaLoc = ChromaLoc_Default;
  if (!ErrorText)  ChromaLoc = AvsGetChromaLocation(Env, SrcClip, avs_array_elt(Args,5),
    "Analyze: ChromaPlacement must be \"auto\", \"left\" (\"MPEG2\"), \"center\" (\"MPEG1\", \"jpeg\"), \"top_left\", \"top\", \"bottom_left\", \"bottom\" or \"DV\"", &ErrorText);
  if (!ErrorText)
  {
    const FrameLayout  srcFmt = FrameLayoutFromPixelType(SrcVideoInfo->pixel_type);
    const bool  destHasAlphaRow = (DestVideoInfo->height > SrcVideoInfo->height);
    uint8_t * destPlanes[MaxChannelCount] = {};
    int  destPitch[MaxChannelCount] = {};
    if (!AvsGetFramePlanes(DestFrame, srcFmt, destPlanes, destPitch))
    {
      ErrorText = "Analyze: Could not write to target frame";
    }
    else
    {
      AVS_VideoFrame * currentSrcFrame = nullptr;
      auto fetchSrcFrame = [&](unsigned int n, const uint8_t* planes[], int pitch[]) -> bool {
        currentSrcFrame = avs_get_frame(SrcClip, static_cast<int>(n));
        if (!currentSrcFrame)  return  false;
        if (!AvsGetFramePlanesRO(currentSrcFrame, srcFmt, planes, pitch))
        {
          avs_release_video_frame(currentSrcFrame);  currentSrcFrame = nullptr;
          return  false;
        }
        return  true;
      };
      auto releaseSrcFrame = [&](unsigned int /*n*/) {
        if (currentSrcFrame)  { avs_release_video_frame(currentSrcFrame);  currentSrcFrame = nullptr; }
      };
      AnalyzeResult = Analyze(srcFmt, SrcVideoInfo->width, SrcVideoInfo->height,
                               static_cast<unsigned int>(SrcVideoInfo->num_frames),
                               fetchSrcFrame, releaseSrcFrame,
                               destPlanes, destPitch, destHasAlphaRow,
                               Mask, MaskSize, ComputeAlpha, DeviationWeight, SubsamplingWeight, ChromaLoc);
      if (AnalyzeResult==Analyze_Result_SrcClipError)  ErrorText = "Analyze: Problem while passing source clip";
      if (AnalyzeResult==Analyze_Result_MemoryError)   ErrorText = "Analyze: Could not allocate memory";
      if (AnalyzeResult==Analyze_Result_GeneralError)  ErrorText = "Analyze: A totally strange error occured while passing the source clip";
    }
  }
  /* Release source clip */
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
    FilterInfo->vi.num_frames = static_cast<int>(AnalyzeResult);
    FilterInfo->vi.audio_samples_per_second = 0;
    FilterInfo->vi.num_audio_samples = 0;
    FilterInfo->vi.nchannels = 0;
    FilterInfo->get_frame = Analyze_GetFrame;
    FilterInfo->get_parity = nullptr;
    FilterInfo->get_audio = nullptr;
    FilterInfo->set_cache_hints = nullptr;
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
  return  avs_copy_video_frame(static_cast<AVS_VideoFrame*>(FilterInfo->user_data));
}



void  AVSC_CC  Analyze_Free(AVS_FilterInfo *  FilterInfo)
{
  if (FilterInfo->user_data)  avs_release_video_frame(static_cast<AVS_VideoFrame*>(FilterInfo->user_data));
}
#endif  // ENABLE_AVISYNTH



#ifdef ENABLE_AVISYNTH
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
  double  PAR, Scale;
  const char * ErrorText;


  /* Extract values from AVS_Value container, throw errors if necessary */
  Scale = avs_is_float(avs_array_elt(Args,1))?avs_as_float(avs_array_elt(Args,1)):1.0;
  PAR = avs_is_float(avs_array_elt(Args,2))?avs_as_float(avs_array_elt(Args,2)):1.0;
  if (PAR<=0.0)  return  avs_new_value_error("DistanceFunction: Pixel aspect ratio must be positive");
  /* Create new filter */
  DestClip = avs_new_c_filter(Env,&FilterInfo,avs_array_elt(Args,0),1);
  if (!DestClip)  return  avs_new_value_error("DistanceFunction: Could not create filter");
  VideoInfo = &FilterInfo->vi;
  /* If something fails here, we store a note in ErrorText. This will be checked later, and memory will be cleaned up before leaving */
  ErrorText = nullptr;
  DistFcnData = nullptr;
  /* Check all video properties */
  if (!(avs_is_yuy2(VideoInfo) || avs_is_yv12(VideoInfo) || avs_is_yv16(VideoInfo) || avs_is_yv24(VideoInfo) || avs_is_y8(VideoInfo) || avs_is_rgb32(VideoInfo) || avs_is_planar_rgba(VideoInfo)))  ErrorText = "DistanceFunction: Source clip must have luma channel (YUY2 or YV12/16/24/8) or alpha channel (RGB32, RGBAP)";
  else if (!(avs_has_video(VideoInfo) && VideoInfo->num_frames && VideoInfo->width && VideoInfo->height))  ErrorText = "DistanceFunction: Source clip has no video";
  else
  {
    /* Allocate data struct */
    DistFcnData = new (std::nothrow) DistanceFunctionDataStruct;
    if (!DistFcnData)  ErrorText = "DistanceFunction: Could not allocate memory";
  }
  if (!ErrorText)
  {
    DistFcnData->PixelType = VideoInfo->pixel_type;
    DistFcnData->LumaChannelNo = (avs_is_rgb32(VideoInfo) || avs_is_planar_rgba(VideoInfo))?3:0;
    DistFcnData->Width = VideoInfo->width;
    DistFcnData->Height = VideoInfo->height;
    DistFcnData->layout = FrameLayoutFromPixelType(DistFcnData->PixelType);
    if (PAR>1.0)
    {
      DistFcnData->PixelWidth = 1.0;
      DistFcnData->PixelHeight = 1.0/PAR;
      DistFcnData->Scale = std::fabs(Scale)*std::sqrt(PAR);
    }
    else
    {
      DistFcnData->PixelWidth = PAR;
      DistFcnData->PixelHeight = 1.0;
      DistFcnData->Scale = std::fabs(Scale)/std::sqrt(PAR);
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
#endif  // ENABLE_AVISYNTH


template<int SS>
static const char *  DistanceFunctionCoreImpl(uint8_t *  data,  int  pitch,  int  SampleStep,  int  width,  int  height,
                                              double  scale,  int  scaleSign,  double  pixelWidth,  double  pixelHeight)
{
  const int stride = (SS > 0) ? SS : SampleStep;
  int  x, y;
  if (scale == 0.0)
  {
    uint8_t * DataPtr = data;
    for (y=0 ; y<height ; y++)
    {
      for (x=0 ; x<width ; x++)
      {
        *DataPtr = ((*DataPtr)>(MaskThreshold))?0xFF:0x00;
        DataPtr += stride;
      }
      DataPtr += pitch-width*stride;
    }
    return  nullptr;
  }
  std::vector<float> DistFcnVec;
  try  { DistFcnVec.resize(width*height); }
  catch (std::bad_alloc&)  { return  "DistanceFunction: Could not allocate memory"; }
  float * DistFcn = DistFcnVec.data();
  uint8_t * DataPtr = data;
  for (y=0 ; y<height ; y++)
  {
    const int distRow = y*width;
    for (x=0 ; x<width ; x++)
    {
      DistFcn[distRow+x] = (scaleSign*(MaskThresholdSign)*(*DataPtr)>scaleSign*(MaskThresholdSign)*(MaskThreshold))?0.0f:1.0f;
      DataPtr += stride;
    }
    DataPtr += pitch-width*stride;
  }
  if (!FastMarching(DistFcn,nullptr,width,height,pixelWidth,pixelHeight))
    return  "DistanceFunction: Mask processing failed: Out of memory";
  for (float& v : DistFcnVec)  v *= static_cast<float>(scale);
  DataPtr = data;
  for (y=0 ; y<height ; y++)
  {
    const int distRow = y*width;
    for (x=0 ; x<width ; x++)
    {
      const float v = DistFcn[distRow+x];
      int iv = static_cast<int>(v + 0.5f);
      iv = std::min(255, std::max(0, iv));
      *DataPtr = static_cast<uint8_t>(iv);
      DataPtr += stride;
    }
    DataPtr += pitch-width*stride;
  }
  if (scaleSign*MaskThresholdSign>0)
  {
    DataPtr = data;
    for (y=0 ; y<height ; y++)
    {
      for (x=0 ; x<width ; x++)
      {
        *DataPtr ^= 0xFF;
        DataPtr += stride;
      }
      DataPtr += pitch-width*stride;
    }
  }
  return  nullptr;
}

const char *  DistanceFunctionCore(uint8_t *  data,  int  pitch,  int  SampleStep,  int  width,  int  height,
                                   double  scale,  int  scaleSign,  double  pixelWidth,  double  pixelHeight)
{
  switch (SampleStep)
  {
    case 1:  return DistanceFunctionCoreImpl<1>(data, pitch, SampleStep, width, height, scale, scaleSign, pixelWidth, pixelHeight);  /* planar: all VS4, YUV planar, Y8, RGBP, RGBAP alpha */
    case 2:  return DistanceFunctionCoreImpl<2>(data, pitch, SampleStep, width, height, scale, scaleSign, pixelWidth, pixelHeight);  /* YUY2 luma */
    case 3:  return DistanceFunctionCoreImpl<3>(data, pitch, SampleStep, width, height, scale, scaleSign, pixelWidth, pixelHeight);  /* RGB24 */
    case 4:  return DistanceFunctionCoreImpl<4>(data, pitch, SampleStep, width, height, scale, scaleSign, pixelWidth, pixelHeight);  /* YUY2 chroma / RGB32 */
    default: return DistanceFunctionCoreImpl<0>(data, pitch, SampleStep, width, height, scale, scaleSign, pixelWidth, pixelHeight);  /* unknown: runtime fallback */
  }
}



#ifdef ENABLE_AVISYNTH
AVS_VideoFrame *  AVSC_CC  DistanceFunction_GetFrame(AVS_FilterInfo *  FilterInfo,  int  FrameNo)
{
  DistanceFunctionDataStruct * DistFcnData = static_cast<DistanceFunctionDataStruct*>(FilterInfo->user_data);
  AVS_VideoFrame * Frame = avs_get_frame(FilterInfo->child,FrameNo);
  if (Frame)  avs_make_writable(FilterInfo->env,&Frame);
  uint8_t * FrameData[MaxChannelCount] = {};
  int  FramePitch[MaxChannelCount] = {};
  if (Frame)  AvsGetFramePlanes(Frame,DistFcnData->layout,FrameData,FramePitch);
  uint8_t * DataPtr = FrameData[DistFcnData->LumaChannelNo];
  if (!DataPtr)
  {
    if (Frame)  avs_release_video_frame(Frame);
    FilterInfo->error = Frame ? "DistanceFunction: Could not get pixel data pointers" : "DistanceFunction: Could not get video frame";
    return  nullptr;
  }
  const char * ErrorText = DistanceFunctionCore(DataPtr, FramePitch[DistFcnData->LumaChannelNo],
    DistFcnData->layout.sampleStep[DistFcnData->LumaChannelNo],
    DistFcnData->Width, DistFcnData->Height,
    DistFcnData->Scale, DistFcnData->ScaleSign,
    DistFcnData->PixelWidth, DistFcnData->PixelHeight);
  if (ErrorText)
  {
    avs_release_video_frame(Frame);
    FilterInfo->error = ErrorText;
    return  nullptr;
  }
  return  Frame;
}



void  AVSC_CC  DistanceFunction_Free(AVS_FilterInfo *  FilterInfo)
{
  if (FilterInfo->user_data)  delete static_cast<DistanceFunctionDataStruct*>(FilterInfo->user_data);
}
#endif  // ENABLE_AVISYNTH



/****************************************************************
                      Common functions
 ****************************************************************/
#ifdef ENABLE_AVISYNTH
FrameLayout  FrameLayoutFromPixelType(int  PixelType)
/* Returns the pixel-format-derived layout for the given AviSynth pixel_type constant.
   channelCount == 0 signals an unsupported / unknown format.                          */
{
  FrameLayout  L;
  switch (PixelType)
  {
    case AVS_CS_BGR24:   L.channelCount = 3;  L.colorFamily = ColorFamily::RGB;  break;
    case AVS_CS_BGR32:   L.channelCount = 4;  L.colorFamily = ColorFamily::RGB;  break;
    case AVS_CS_YUY2:    L.channelCount = 3;  L.colorFamily = ColorFamily::YUV;  break;
    case AVS_CS_YV12:
    case AVS_CS_I420:    L.channelCount = 3;  L.colorFamily = ColorFamily::YUV;  break;
    case AVS_CS_YV16:    L.channelCount = 3;  L.colorFamily = ColorFamily::YUV;  break;
    case AVS_CS_YV24:    L.channelCount = 3;  L.colorFamily = ColorFamily::YUV;  break;
    case AVS_CS_Y8:      L.channelCount = 1;  L.colorFamily = ColorFamily::Gray; break;
    case AVS_CS_RGBP:    L.channelCount = 3;  L.colorFamily = ColorFamily::RGB;  break;
    case AVS_CS_RGBAP:   L.channelCount = 4;  L.colorFamily = ColorFamily::RGB;  break;
    default:             return L;  /* channelCount stays 0 */
  }
  /* sampleStep: bytes between adjacent samples in the same row */
  switch (PixelType)
  {
    case AVS_CS_BGR24:
      for (int i=0 ; i<L.channelCount ; i++)  L.sampleStep[i] = 3;
    break;
    case AVS_CS_BGR32:
      for (int i=0 ; i<L.channelCount ; i++)  L.sampleStep[i] = 4;
    break;
    case AVS_CS_YUY2:
      L.sampleStep[0] = 2;
      for (int i=1 ; i<L.channelCount ; i++)  L.sampleStep[i] = 4;
    break;
    case AVS_CS_YV12:
    case AVS_CS_I420:
    case AVS_CS_YV16:
    case AVS_CS_YV24:
    case AVS_CS_Y8:
    case AVS_CS_RGBP:
    case AVS_CS_RGBAP:
      for (int i=0 ; i<L.channelCount ; i++)  L.sampleStep[i] = 1;
    break;
  }
  /* pixelWidth: horizontal subsampling factor */
  switch (PixelType)
  {
    case AVS_CS_BGR24:
    case AVS_CS_BGR32:
    case AVS_CS_YV24:
    case AVS_CS_Y8:
    case AVS_CS_RGBP:
    case AVS_CS_RGBAP:
      for (int i=0 ; i<L.channelCount ; i++)  L.pixelWidth[i] = 1;
    break;
    case AVS_CS_YUY2:
    case AVS_CS_YV12:
    case AVS_CS_I420:
    case AVS_CS_YV16:
      L.pixelWidth[0] = 1;
      for (int i=1 ; i<L.channelCount ; i++)  L.pixelWidth[i] = 2;
    break;
  }
  /* pixelHeight: vertical subsampling factor */
  switch (PixelType)
  {
    case AVS_CS_BGR24:
    case AVS_CS_BGR32:
    case AVS_CS_YUY2:
    case AVS_CS_YV24:
    case AVS_CS_YV16:
    case AVS_CS_Y8:
    case AVS_CS_RGBP:
    case AVS_CS_RGBAP:
      for (int i=0 ; i<L.channelCount ; i++)  L.pixelHeight[i] = 1;
    break;
    case AVS_CS_YV12:
    case AVS_CS_I420:
      L.pixelHeight[0] = 1;
      for (int i=1 ; i<L.channelCount ; i++)  L.pixelHeight[i] = 2;
    break;
  }
  return L;
}



/* Internal helper shared by AvsGetFramePlanes and AvsGetFramePlanesRO:
   fills Pitch[] for the given frame and layout.                        */
static void  AvsFillPitch(AVS_VideoFrame *  Frame,  const FrameLayout &  L,  int *  Pitch)
{
  for (int i=0 ; i<L.channelCount ; i++)  Pitch[i] = 0;
  if (L.sampleStep[0] > 1)  /* packed format: all channels share the same row-pitch */
  {
    Pitch[0] = avs_get_pitch_p(Frame, AVS_DEFAULT_PLANE);
    for (int i=1 ; i<L.channelCount ; i++)  Pitch[i] = Pitch[0];
  }
  else if (L.colorFamily == ColorFamily::YUV || L.colorFamily == ColorFamily::Gray)
  {
    Pitch[0] = avs_get_pitch_p(Frame, AVS_PLANAR_Y);
    if (L.channelCount > 1)  Pitch[1] = avs_get_pitch_p(Frame, AVS_PLANAR_U);
    if (L.channelCount > 2)  Pitch[2] = avs_get_pitch_p(Frame, AVS_PLANAR_V);
  }
  else  /* planar RGB / RGBA */
  {
    Pitch[0] = avs_get_pitch_p(Frame, AVS_PLANAR_B);
    if (L.channelCount > 1)  Pitch[1] = avs_get_pitch_p(Frame, AVS_PLANAR_G);
    if (L.channelCount > 2)  Pitch[2] = avs_get_pitch_p(Frame, AVS_PLANAR_R);
    if (L.channelCount > 3)  Pitch[3] = avs_get_pitch_p(Frame, AVS_PLANAR_A);
  }
}


bool  AvsGetFramePlanes(AVS_VideoFrame *  Frame,  const FrameLayout &  L,  uint8_t **  Planes,  int *  Pitch)
/* Obtain write-accessible plane pointers and row-pitches for Frame.
   Returns false if Frame is null or any pointer cannot be obtained.   */
{
  if (!Frame || !L.channelCount)  return  false;
  for (int i=0 ; i<L.channelCount ; i++)  Planes[i] = nullptr;
  AvsFillPitch(Frame, L, Pitch);
  if (L.sampleStep[0] > 1)  /* packed */
  {
    Planes[0] = avs_get_write_ptr_p(Frame, AVS_DEFAULT_PLANE);
    if (!Planes[0])  return  false;
    for (int i=1 ; i<L.channelCount ; i++)  Planes[i] = Planes[i-1]+1;
    if (L.colorFamily == ColorFamily::YUV && L.channelCount > 2)  Planes[2]++;  /* YUY2: V is 2 bytes after U */
  }
  else if (L.colorFamily == ColorFamily::YUV || L.colorFamily == ColorFamily::Gray)
  {
    Planes[0] = avs_get_write_ptr_p(Frame, AVS_PLANAR_Y);
    if (L.channelCount > 1)  Planes[1] = avs_get_write_ptr_p(Frame, AVS_PLANAR_U);
    if (L.channelCount > 2)  Planes[2] = avs_get_write_ptr_p(Frame, AVS_PLANAR_V);
    for (int i=0 ; i<L.channelCount ; i++)  if (!Planes[i])  return  false;
  }
  else  /* planar RGB / RGBA */
  {
    Planes[0] = avs_get_write_ptr_p(Frame, AVS_PLANAR_B);
    if (L.channelCount > 1)  Planes[1] = avs_get_write_ptr_p(Frame, AVS_PLANAR_G);
    if (L.channelCount > 2)  Planes[2] = avs_get_write_ptr_p(Frame, AVS_PLANAR_R);
    if (L.channelCount > 3)  Planes[3] = avs_get_write_ptr_p(Frame, AVS_PLANAR_A);
    for (int i=0 ; i<L.channelCount ; i++)  if (!Planes[i])  return  false;
  }
  return  true;
}


bool  AvsGetFramePlanesRO(AVS_VideoFrame *  Frame,  const FrameLayout &  L,  const uint8_t **  Planes,  int *  Pitch)
/* Read-only variant of AvsGetFramePlanes.                             */
{
  if (!Frame || !L.channelCount)  return  false;
  for (int i=0 ; i<L.channelCount ; i++)  Planes[i] = nullptr;
  AvsFillPitch(Frame, L, Pitch);
  if (L.sampleStep[0] > 1)  /* packed */
  {
    Planes[0] = avs_get_read_ptr_p(Frame, AVS_DEFAULT_PLANE);
    if (!Planes[0])  return  false;
    for (int i=1 ; i<L.channelCount ; i++)  Planes[i] = Planes[i-1]+1;
    if (L.colorFamily == ColorFamily::YUV && L.channelCount > 2)  Planes[2]++;  /* YUY2: V is 2 bytes after U */
  }
  else if (L.colorFamily == ColorFamily::YUV || L.colorFamily == ColorFamily::Gray)
  {
    Planes[0] = avs_get_read_ptr_p(Frame, AVS_PLANAR_Y);
    if (L.channelCount > 1)  Planes[1] = avs_get_read_ptr_p(Frame, AVS_PLANAR_U);
    if (L.channelCount > 2)  Planes[2] = avs_get_read_ptr_p(Frame, AVS_PLANAR_V);
    for (int i=0 ; i<L.channelCount ; i++)  if (!Planes[i])  return  false;
  }
  else  /* planar RGB / RGBA */
  {
    Planes[0] = avs_get_read_ptr_p(Frame, AVS_PLANAR_B);
    if (L.channelCount > 1)  Planes[1] = avs_get_read_ptr_p(Frame, AVS_PLANAR_G);
    if (L.channelCount > 2)  Planes[2] = avs_get_read_ptr_p(Frame, AVS_PLANAR_R);
    if (L.channelCount > 3)  Planes[3] = avs_get_read_ptr_p(Frame, AVS_PLANAR_A);
    for (int i=0 ; i<L.channelCount ; i++)  if (!Planes[i])  return  false;
  }
  return  true;
}
#endif  // ENABLE_AVISYNTH



bool  FastMarching(float  u[],  unsigned int *  Order,  int  Width,  int  Height,  double  PixelWidth,  double  PixelHeight)
{
  int  k, x, y, h;
  bool  Error;
  FMMHeapStruct  Heap;

  /* Initialize close matrix */
  std::vector<int> CloseVec(Width*Height, 0);
  int * Close = CloseVec.data();
  for (k=0 ; k<Width*Height ; k++)  if (u[k]>(float)0.0)  u[k] = FMMFarMarker;
  /* Initialize heap struct */
  Heap.Delta = FMMHeapDelta;
  Heap.MaxSize = 4*(Width+Height+FMMHeapDelta);  /* Enough memory for slightly unregular domains */
  std::vector<int> HeapVec(Heap.MaxSize);
  Heap.MemPointer = HeapVec.data();
  Heap.Size = 0;
  Heap.Heap = Heap.MemPointer-1;  /* 1-indexed heap: Heap[1] is root (= MemPointer[0]); Heap[0] is never accessed */
  /* Initialize close region (around initial values) */
  Error = false;
  for (k=0 ; k<Width*Height ; k++)
  {
    if (u[k]!=FMMFarMarker && !Close[k])
    {
      y = k/Width;
      x = k%Width;
      if (x>0)  Error |= !FMMUpdateCloseValue(u,Close,x-1,y,Width,Height,PixelWidth,PixelHeight,&Heap,HeapVec);
      if (y>0)  Error |= !FMMUpdateCloseValue(u,Close,x,y-1,Width,Height,PixelWidth,PixelHeight,&Heap,HeapVec);
      if (x<Width-1)  Error |= !FMMUpdateCloseValue(u,Close,x+1,y,Width,Height,PixelWidth,PixelHeight,&Heap,HeapVec);
      if (y<Height-1)  Error |= !FMMUpdateCloseValue(u,Close,x,y+1,Width,Height,PixelWidth,PixelHeight,&Heap,HeapVec);
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
    if (x>0)  Error |= !FMMUpdateCloseValue(u,Close,x-1,y,Width,Height,PixelWidth,PixelHeight,&Heap,HeapVec);
    if (y>0)  Error |= !FMMUpdateCloseValue(u,Close,x,y-1,Width,Height,PixelWidth,PixelHeight,&Heap,HeapVec);
    if (x<Width-1)  Error |= !FMMUpdateCloseValue(u,Close,x+1,y,Width,Height,PixelWidth,PixelHeight,&Heap,HeapVec);
    if (y<Height-1)  Error |= !FMMUpdateCloseValue(u,Close,x,y+1,Width,Height,PixelWidth,PixelHeight,&Heap,HeapVec);
  }
  return  !Error;
}



bool  FMMUpdateCloseValue(float  u[],  int  Close[],  int  x,  int  y,  int  Width,  int  Height,  double  PixelWidth,  double  PixelHeight,  FMMHeapStruct *  Heap,  std::vector<int> &  HeapVec)
{
  int  k, h;  /* k = Position in matrix,   h = position in heap */
  int  xm, xp, ym, yp;
  // for large (even FullHD) frame sizes 'float' is not enough
  // process sometimes does not converge
  double  p, uxm, uxp, uym, uyp, ux, uy;

  k = Width*y+x;  /* Position in arrays */
  if ((!Close[k]) && (u[k]!=FMMFarMarker))  return  true;  /* If not close and not far, it is alive, hence cannot be updated anymore */
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
    u[k] = static_cast<float>(ux+PixelWidth);
  }
  else  if (uy+PixelHeight<ux)
  {
    u[k] = static_cast<float>(uy+PixelHeight);
  }
  else
  {
    PixelWidth *= PixelWidth;
    PixelHeight *= PixelHeight;
    p = PixelWidth*uy+PixelHeight*ux;
    p /= PixelWidth+PixelHeight;
    u[k] = static_cast<float>(p+std::sqrt(p*p-(PixelWidth*uy*uy+PixelHeight*ux*ux-PixelWidth*PixelHeight)/(PixelWidth+PixelHeight)));
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
      try  { HeapVec.resize(Heap->MaxSize); }  catch (std::bad_alloc&)  { return  false; }
      Heap->MemPointer = HeapVec.data();
      Heap->Heap = Heap->MemPointer-1;
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
  return  true;
}



/****************************************************************
                         Inpaint tools
 ****************************************************************/
template<int SS>
static InpaintMaskResult  InpaintPrepareMaskImpl(InpaintMaskDataStruct **  MaskDataPtr,  const FrameLayout &  maskLayout,  const uint8_t *  planes[],  int  pitch[],  const InpaintDataStruct *  InpaintData)
/* If mask is full or empty, MaskData is cleared */
{
  InpaintMaskDataStruct * MaskData;
  double  PixelWidth, PixelHeight;
  float * DistFcn, * u, * Gchi;
  int  x, y, xmin, xmax, ymin, ymax, Width, Height, k, RowStep;
  int  ChromaWidth = 0, ChromaHeight = 0;
  int  Pitch;
  signed int  MaskSize;
  const  uint8_t * DataPtr;
  uint8_t * chi;
  InpaintMaskResult  Result;
  bool  MaskIsSet;

  MaskData = *MaskDataPtr;
  if (MaskData)
  {
    delete MaskData;
    MaskData = nullptr;
    *MaskDataPtr = nullptr;
  }
  if (!planes[0])  return  InpaintMaskResult::FrameError;
  MaskData = new (std::nothrow) InpaintMaskDataStruct;
  if (!MaskData)  return  InpaintMaskResult::MemoryError;
  MaskData->Width = Width = InpaintData->Width;
  MaskData->Height = Height = InpaintData->Height;
  std::vector<float> DistFcnVec;
  try  { DistFcnVec.resize(Width*Height); }
  catch (std::bad_alloc&)  { delete MaskData;  return  InpaintMaskResult::MemoryError; }
  DistFcn = DistFcnVec.data();
  Pitch = pitch[InpaintData->MaskChannelNo];
  const int SampleStep = (SS > 0) ? SS : maskLayout.sampleStep[InpaintData->MaskChannelNo];
  MaskSize = 0;
  DataPtr = planes[InpaintData->MaskChannelNo];
  u = DistFcn;
  if (InpaintData->FlipMask)
  {
    Pitch *= -1;
    DataPtr -= (InpaintData->Height-1)*Pitch;
  }
  RowStep = Pitch - Width*SampleStep;
  for (y=0 ; y<Height ; y++)
  {
    for (x=0 ; x<Width ; x++)
    {
      MaskSize += (MaskIsSet = ((MaskThresholdSign)*(*DataPtr)>(MaskThresholdSign)*(MaskThreshold))) ? 1 : 0;
      *(u++) = MaskIsSet?0.0f:1.0f;
      DataPtr += SampleStep;
    }
    DataPtr += RowStep;
  }
  if (!MaskSize || (MaskSize==Width*Height))
  {
    delete MaskData;
    return  (MaskSize)?InpaintMaskResult::FullMask:InpaintMaskResult::NoMask;
  }
  /* We create a halo around the inpainting mask. Outside of this halo nothing will happen! */
  /* These variables are not the size of a pixel, but used to rescale the aspect ratio of the halo */
  PixelWidth = InpaintData->ChromaPreBlurXKernelSize+std::max<double>(InpaintData->Chroma4LumaPostBlurXKernelSize,InpaintData->Chroma4ChromaPostBlurXKernelSize);
  PixelWidth += 1.5;
  PixelWidth *= InpaintData->ChromaPixelWidth;
  PixelWidth = std::max<double>(PixelWidth,1.5+InpaintData->LumaPreBlurXKernelSize+std::max<double>(InpaintData->Luma4LumaPostBlurXKernelSize,InpaintData->Luma4ChromaPostBlurXKernelSize));
  PixelWidth += InpaintData->ChromaPixelWidth;
  PixelWidth = std::max<double>(PixelWidth,2.0);
  PixelHeight = InpaintData->ChromaPreBlurYKernelSize+std::max<double>(InpaintData->Chroma4LumaPostBlurYKernelSize,InpaintData->Chroma4ChromaPostBlurYKernelSize);
  PixelHeight += 1.5;
  PixelHeight *= InpaintData->ChromaPixelHeight;
  PixelHeight = std::max<double>(PixelHeight,1.5+InpaintData->LumaPreBlurYKernelSize+std::max<double>(InpaintData->Luma4LumaPostBlurYKernelSize,InpaintData->Luma4ChromaPostBlurYKernelSize));
  PixelHeight += InpaintData->ChromaPixelHeight;
  PixelHeight = std::max<double>(PixelHeight,2.0);
  if (!FastMarching(DistFcn,nullptr,Width,Height,1.0/PixelWidth,1.0/PixelHeight))
  {
    delete MaskData;
    return  InpaintMaskResult::MemoryError;
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
  Result = InpaintMaskResult::OK;
  if ((xmin>xmax) || (ymin>ymax))  Result = InpaintMaskResult::GeneralError;
  if (Result==InpaintMaskResult::OK)
  {
    /* xymin is first pixel inside the processing area
       xymax is last pixel inside the processing area */
    if (InpaintData->ChromaChannelCount) {
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
    }
    else {
      // Y8
      xmax++;
      ymax++;
    }
    /* xymin is first pixel inside the processing area
       xymax is first pixel outside the processing area */
    MaskData->OffsetX = xmin;
    MaskData->OffsetY = ymin;
    MaskData->Width = Width = xmax-xmin;
    MaskData->Height = Height = ymax-ymin;
    if (InpaintData->ChromaChannelCount)
    {
      ChromaWidth  = Width  / InpaintData->ChromaPixelWidth;
      ChromaHeight = Height / InpaintData->ChromaPixelHeight;
    }
    RowStep = Pitch - Width*SampleStep;
    try
    {
      MaskData->chiLuma.resize(Width*Height);
      if (InpaintData->ChromaChannelCount)
        MaskData->chiChroma.resize(ChromaWidth*ChromaHeight, 0);
    }
    catch (std::bad_alloc&)  { Result = InpaintMaskResult::MemoryError; }
    if (MaskSize==Width*Height)  Result = InpaintMaskResult::GeneralError;
  }
  if (Result==InpaintMaskResult::OK)
  {
    /* The halo ist still stored in DistFcn */
    u = DistFcn+MaskData->OffsetY*InpaintData->Width+MaskData->OffsetX;
    chi = MaskData->chiLuma.data();
    /* Transfer the halo to chi: chi=0 for inpainting domain or halo, chi=1 in far-away-region
       chi should also be 1 in the inpainting domain - we'll set that later */
    for (y=0 ; y<Height ; y++)
    {
      for (x=0 ; x<Width ; x++)  *(chi++) = ((*(u++))==0.0);
      u += InpaintData->Width-Width;
    }
    /* Save MaskSize, then MaskSize will be decreased for each pixel set. If it doesn't vanish after processing the mask again, an internal error occured */
    MaskData->MaskPixelCount = MaskSize;
    DataPtr = planes[InpaintData->MaskChannelNo]+MaskData->OffsetY*Pitch+MaskData->OffsetX*SampleStep;
    u = DistFcn;
    chi = MaskData->chiLuma.data();
    if (InpaintData->FlipMask)  DataPtr -= (InpaintData->Height-1)*Pitch;  /* Pitch is negative already ! */
    /* We set chi=1 in the inpainting domain */
    for (y=0 ; y<Height ; y++)
    {
      for (x=0 ; x<Width ; x++)
      {
        MaskSize -= (MaskIsSet = ((MaskThresholdSign)*(*DataPtr)>(MaskThresholdSign)*(MaskThreshold))) ? 1 : 0;
        chi[0] = chi[0] || MaskIsSet;
        chi++;
        *(u++) = MaskIsSet?1.0f:0.0f;
        DataPtr += SampleStep;
      }
      DataPtr += RowStep;
    }
    if (MaskSize)  Result = InpaintMaskResult::GeneralError;
  }
  if (Result==InpaintMaskResult::OK)
  {
    if (!MaskData->chiChroma.empty())
    {
      /* ly is outer to x: chi advances in scanline order (all x for a luma row, then next row).
         For each chroma row block, every luma sub-row (ly) must be fully consumed across all
         chroma columns (x) before advancing ly — swapping ly and x would break scanline order.
         The lambda receives CPW/CPH as std::integral_constant so they are compile-time constants
         inside the body
         Combinations: (1,1)=RGB/4:4:4, (2,1)=4:2:2/YUY2, (2,2)=4:2:0. */
      chi = MaskData->chiLuma.data();
      auto accumulateChroma = [&](auto cpwConst, auto cphConst)
      {
        constexpr int CPW = decltype(cpwConst)::value;
        constexpr int CPH = decltype(cphConst)::value;
        for (y=0 ; y<ChromaHeight ; y++)
        {
          uint8_t * const chromaRowPtr = &MaskData->chiChroma[y*ChromaWidth];
          for (int ly=0 ; ly<CPH ; ly++)
            for (x=0 ; x<ChromaWidth ; x++)
            {
              if constexpr (CPW == 1)
                chromaRowPtr[x] += *(chi++);
              else if constexpr (CPW == 2)
              {
                chromaRowPtr[x] += chi[0] + chi[1];
                chi += 2;
              }
              else
              {
                int sum = 0;
                for (int lx=0 ; lx<CPW ; lx++)  sum += *(chi++);
                chromaRowPtr[x] += static_cast<uint8_t>(sum);
              }
            }
        }
      };
      if      (InpaintData->ChromaPixelWidth==1 && InpaintData->ChromaPixelHeight==1)
        accumulateChroma(std::integral_constant<int,1>{}, std::integral_constant<int,1>{});
      else if (InpaintData->ChromaPixelWidth==2 && InpaintData->ChromaPixelHeight==1)
        accumulateChroma(std::integral_constant<int,2>{}, std::integral_constant<int,1>{});
      else if (InpaintData->ChromaPixelWidth==2 && InpaintData->ChromaPixelHeight==2)
        accumulateChroma(std::integral_constant<int,2>{}, std::integral_constant<int,2>{});
      else  /* unsupported subsampling (e.g. 4:1:1): general runtime fallback */
      {
        for (y=0 ; y<ChromaHeight ; y++)
        {
          uint8_t * const chromaRowPtr = &MaskData->chiChroma[y*ChromaWidth];
          for (int ly=0 ; ly<InpaintData->ChromaPixelHeight ; ly++)
            for (x=0 ; x<ChromaWidth ; x++)
            {
              int sum = 0;
              for (int lx=0 ; lx<InpaintData->ChromaPixelWidth ; lx++)  sum += *(chi++);
              chromaRowPtr[x] += static_cast<uint8_t>(sum);
            }
        }
      }
    }
    try  { MaskData->MaskPixelOrder.resize(MaskData->MaskPixelCount); }
    catch (std::bad_alloc&)  { Result = InpaintMaskResult::MemoryError; }
    /* We call FastMarching to determine the inpainting pixel order */
    if (Result==InpaintMaskResult::OK && !FastMarching(DistFcn,MaskData->MaskPixelOrder.data(),Width,Height,InpaintData->PixelWidth,InpaintData->PixelHeight))  Result = InpaintMaskResult::MemoryError;
  }
  /* DistFcnVec goes out of scope here and releases automatically */
  if (Result==InpaintMaskResult::OK)
  {
    if ((InpaintData->MaxSteps>=0)&&(InpaintData->MaxSteps<MaskData->MaskPixelCount))
    {
      MaskData->MaskPixelCount = InpaintData->MaxSteps;
      MaskData->MaskPixelOrder.resize(MaskData->MaskPixelCount);  /* shrink: no reallocation failure possible */
    }
    try
    {
      if (InpaintData->LumaWeight!=0.0)    MaskData->GchiLuma.resize(Width*Height);
      if (InpaintData->ChromaWeight!=0.0)  MaskData->GchiChroma.resize(ChromaWidth*ChromaHeight);
    }
    catch (std::bad_alloc&)  { Result = InpaintMaskResult::MemoryError; }
  }
  if (Result==InpaintMaskResult::OK)
  {
    if (!MaskData->GchiLuma.empty())
    {
      chi = MaskData->chiLuma.data();
      Gchi = MaskData->GchiLuma.data();
      for (y=0 ; y<Height ; y++)
      {
        const int rowBase = y*Width;
        for (x=0 ; x<Width ; x++)
          if (!chi[rowBase+x])
            AddPreBlurValue(Gchi, x, y, Width, Height, 1, InpaintData->LumaPreBlurXKernel, InpaintData->LumaPreBlurXKernelSize, InpaintData->LumaPreBlurYKernel, InpaintData->LumaPreBlurYKernelSize);
      }
    }
    if (!MaskData->GchiChroma.empty())
    {
      chi = MaskData->chiChroma.data();
      Gchi = MaskData->GchiChroma.data();
      for (y = 0; y < ChromaHeight; y++)
      {
        const int rowBase = y * ChromaWidth;
        for (x = 0; x < ChromaWidth; x++)
          if (!chi[rowBase + x])
            AddPreBlurValue(Gchi, x, y, ChromaWidth, ChromaHeight, 1, InpaintData->ChromaPreBlurXKernel, InpaintData->ChromaPreBlurXKernelSize, InpaintData->ChromaPreBlurYKernel, InpaintData->ChromaPreBlurYKernelSize);
      }
    }
  }
  if (Result!=InpaintMaskResult::OK)  { delete MaskData;  MaskData = nullptr; }
  *MaskDataPtr = MaskData;
  return  Result;
}

InpaintMaskResult  InpaintPrepareMask(InpaintMaskDataStruct **  MaskDataPtr,  const FrameLayout &  maskLayout,  const uint8_t *  planes[],  int  pitch[],  const InpaintDataStruct *  InpaintData)
{
  switch (maskLayout.sampleStep[InpaintData->MaskChannelNo])
  {
    case 1:  return InpaintPrepareMaskImpl<1>(MaskDataPtr, maskLayout, planes, pitch, InpaintData);  /* planar: all VS4, YUV planar, Y8, RGBP */
    case 2:  return InpaintPrepareMaskImpl<2>(MaskDataPtr, maskLayout, planes, pitch, InpaintData);  /* YUY2 luma */
    case 3:  return InpaintPrepareMaskImpl<3>(MaskDataPtr, maskLayout, planes, pitch, InpaintData);  /* RGB24 */
    case 4:  return InpaintPrepareMaskImpl<4>(MaskDataPtr, maskLayout, planes, pitch, InpaintData);  /* YUY2 chroma / RGB32 */
    default: return InpaintPrepareMaskImpl<0>(MaskDataPtr, maskLayout, planes, pitch, InpaintData);  /* unknown: runtime fallback */
  }
}


#ifdef ENABLE_AVISYNTH
static InpaintMaskResult  AvsInpaintPrepareMask(InpaintMaskDataStruct **  MaskDataPtr,  AVS_VideoFrame *  Frame,  const InpaintDataStruct *  InpaintData)
/* Thin AVS wrapper: extracts plane pointers from the frame and calls the API-agnostic InpaintPrepareMask. */
{
  const uint8_t * planes[MaxChannelCount];
  int  pitch[MaxChannelCount];
  if (!Frame || !AvsGetFramePlanesRO(Frame, InpaintData->maskLayout, planes, pitch) || !planes[0])
    return  Frame ? InpaintMaskResult::FrameError : InpaintMaskResult::NoMask;
  return  InpaintPrepareMask(MaskDataPtr, InpaintData->maskLayout, planes, pitch, InpaintData);
}
#endif  // ENABLE_AVISYNTH

void AddPreBlurValue(float G[], int x, int y, int Width, int Height, int Value, double BlurXKernel[], int BlurXKernelSize, double BlurYKernel[], int BlurYKernelSize)
{
  const int xmin = (x < BlurXKernelSize) ? -x : -BlurXKernelSize;
  const int xmax = ((Width - 1 - x) < BlurXKernelSize) ? (Width - 1 - x) : BlurXKernelSize;
  const int ymin = (y < BlurYKernelSize) ? -y : -BlurYKernelSize;
  const int ymax = ((Height - 1 - y) < BlurYKernelSize) ? (Height - 1 - y) : BlurYKernelSize;
  const double fValue = Value;
  /* Time-critical loop */
  for (int y0 = ymin; y0 <= ymax; ++y0)
  {
    float* G_row = &G[Width * (y + y0) + x];
    const double weightedY = fValue * BlurYKernel[y0];

    for (int x0 = xmin; x0 <= xmax; ++x0)
    {
      G_row[x0] += static_cast<float>(weightedY * BlurXKernel[x0]);
    }
  }
}

template<int SS>
static int InpaintPixelImpl(double RadiusX, double RadiusY, uint8_t u[], uint8_t chi[], int SampleStep, int Pitch, int x, int y, int Width, int Height, double CoherenceStrength, double CoherenceDirectionX, double CoherenceDirectionY)
{
  // When the template-SampleStep is valid, it is used as a quasi-constexpr
  const int stride = (SS > 0) ? SS : SampleStep;
  const int xmin = (x > RadiusX) ? static_cast<int>(std::ceil(x - RadiusX)) : 0;
  const int ymin = (y > RadiusY) ? static_cast<int>(std::ceil(y - RadiusY)) : 0;
  const int xmax = std::min(Width - 1, static_cast<int>(std::floor(x + RadiusX)));
  const int ymax = std::min(Height - 1, static_cast<int>(std::floor(y + RadiusY)));

  double u0 = 0.0, w0 = 0.0;
  const double RX2 = RadiusX * RadiusX;
  const double RY2 = RadiusY * RadiusY;
  const double RadiusSq = RX2 * RY2;
  
  if (CoherenceStrength > 0.0)
  {
    const double invNorm = CoherenceStrength / std::sqrt(2.0 * RadiusX * RadiusY);
    const double CohDirX = CoherenceDirectionX * invNorm;
    const double CohDirY = CoherenceDirectionY * invNorm;
    const double CohStr = CoherenceStrength * 1.2533141373155; // sqrt(pi/2) = std::sqrt(2.0*std::atan(1.0));

    /* Time-critical loop !! */
    for (int y0 = ymin; y0 <= ymax; ++y0)
    {
      const int dy = y0 - y;
      const int dy2 = dy * dy;

      // Hoist row-constant calculations
      const double dy2RX2 = (double)dy2 * RX2;
      const double dy_term = CohDirX * dy;

      // Row pointers
      const uint8_t* chi_row = &chi[Width * y0];
      const uint8_t* u_row = &u[Pitch * y0];

      for (int x0 = xmin; x0 <= xmax; ++x0)
      {
        if (chi_row[x0]) continue;

        const int dx = x0 - x;
        const int dx2 = dx * dx;

        // Check integer ellipse bounds
        if (dx2 * RY2 + dy2RX2 > RadiusSq) continue;

        // Check integer zero (center pixel)
        const int d2 = dx2 + dy2; // dy2 is hoisted from outer loop
        if (d2 == 0) continue;

        const double distSq = static_cast<double>(d2);
        /* Formular (11) of Bornemann/Maerz goes here */
        double w_arg = CohDirY * dx - dy_term;
        double w = (CohStr / std::sqrt(distSq)) * std::exp(-(w_arg * w_arg));

        u0 += w * u_row[x0 * stride];
        w0 += w;
      }
    }
  }

  // Fallback block (w0 == 0.0)
  if (w0 == 0.0)
  {
    for (int y0 = ymin; y0 <= ymax; ++y0)
    {
      const int dy = y0 - y;
      const int dy2 = dy * dy;
      const double dy2RX2 = (double)dy2 * RX2;
      const uint8_t* chi_row = &chi[Width * y0];
      const uint8_t* u_row = &u[Pitch * y0];

      for (int x0 = xmin; x0 <= xmax; ++x0)
      {
        if (chi_row[x0]) continue;
        const int dx = x0 - x;
        const int dx2 = dx * dx;
        if (dx2 * RY2 + dy2RX2 > RadiusSq) continue;

        // Check integer zero (center pixel)
        const int d2 = dx2 + dy2; // dy2 is hoisted from outer loop
        if (d2 == 0) continue;

        const double distSq = static_cast<double>(d2);
        /* No coherence ==> (11) collapses to 1/|x-y| */
        double w = 1.0 / std::sqrt(distSq);
        u0 += w * u_row[x0 * stride];
        w0 += w;
        }
      }
    }

  if (w0 == 0.0) return -1; // weight 0, invalid, cannot divide
  u0 /= w0;
  const int iValue = static_cast<int>(u0 + 0.5);
  /* Just to be sure - maybe this is not necessary */
  return (iValue < 0) ? 0 : (iValue > 255) ? 255 : iValue;
}


const char*  InpaintCore(const InpaintDataStruct *  InpaintData,  InpaintMaskDataStruct *  MaskData,  uint8_t *  FrameData[],  const int  FramePitch[])
{
  const int ls = InpaintData->srcLayout.sampleStep[0];
  const int cs = InpaintData->srcLayout.sampleStep[1];
  if (ls == 1 && cs == 1)  return InpaintCoreImpl<1,1>(InpaintData, MaskData, FrameData, FramePitch);  /* planar: YUV, Y, RGBP, RGBAP — all VS4 */
  if (ls == 2 && cs == 4)  return InpaintCoreImpl<2,4>(InpaintData, MaskData, FrameData, FramePitch);  /* YUY2 */
  if (ls == 3 && cs == 3)  return InpaintCoreImpl<3,3>(InpaintData, MaskData, FrameData, FramePitch);  /* RGB24 */
  if (ls == 4 && cs == 4)  return InpaintCoreImpl<4,4>(InpaintData, MaskData, FrameData, FramePitch);  /* RGB32 */
  return InpaintCoreImpl<0,0>(InpaintData, MaskData, FrameData, FramePitch);                           /* unknown: runtime fallback */
}


double  GetCoherenceData(double *  CoherenceDirectionX,  double *  CoherenceDirectionY,  double  TensorXX,  double  TensorXY,  double  TensorYY,  double  Sharpness)
/* Solves the eigenvalue problem of formular (22) and computes formular (20) in Bornemann/Maerz */
{
  double  a;
  double  b;
  double  c;

  a = TensorXX+TensorYY;  /* trace */
  if (a==0.0)  return  0.0;
  c = TensorXY*TensorXY;
  b = TensorXX*TensorYY-c;  /* determinant */
  a += std::sqrt(a*a-4.0*b);
  a /= 2.0;  /* bigger eigenvalue */
  b /= a;  /* smaller eigenvalue */
  a -= b;  /* difference between eigenvalues */
  if (a==0.0)  return  0.0;
  a = Sharpness*std::exp(-1.0/(a*a));
  if (a==0.0)  return  0.0;
  if ((!(TensorXX+TensorYY<2*b)) ^ (!(TensorXX<TensorYY)))
  {
    b -= TensorYY;
    c += b*b;  /* Squared norm of eigenvector */
    if (c==0.0)  return  0.0;
    c = std::sqrt(c);  /* Norm of eigenvector */
    *CoherenceDirectionX = b/c;  /* Normalize eigenvector for output */
    *CoherenceDirectionY = TensorXY/c;
  }
  else
  {
    b -= TensorXX;
    c += b*b;  /* Squared norm of eigenvector */
    if (c==0.0)  return  0.0;
    c = std::sqrt(c);  /* Norm of eigenvector */
    *CoherenceDirectionX = TensorXY/c;  /* Normalize eigenvector for output */
    *CoherenceDirectionY = b/c;
  }
  return  a+1.0;
}



void  GetStructureTensor(double *  TensorXX,  double *  TensorXY,  double *  TensorYY,
                         float *  GchiLuma,  float *  GchiuLuma,  uint8_t *  chiLuma,  double  LumaWeight,
                         float *  GchiChroma,  float **  GchiuChromas,  uint8_t *  chiChroma,  double  ChromaWeight,
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



bool GetChannelTensor(double* TensorXX, double* TensorXY, double* TensorYY, float Gchi[], float Gchiu[], uint8_t chi[], int x, int y, int Width, int Height, double BlurKernelX[], int BlurKernelXSize, double BlurKernelY[], int BlurKernelYSize)
{
  // This (fairly) realized formula (21) of Bornemann/März-Paper
  // This function uses forward differences. Anything else must be done by providing an appropriate kernel and by modifying x,y
  double xx = 0, xy = 0, yy = 0, chi0 = 0;

  // Bounds calculation
  int xmin = -(std::min(x, BlurKernelXSize));
  int xmax = std::min(Width - x - 2, BlurKernelXSize);
  int ymin = -(std::min(y, BlurKernelYSize));
  int ymax = std::min(Height - y - 2, BlurKernelYSize);

  /* Time-critical loop !! */
  for (int y0 = ymin; y0 <= ymax; y0++)
  {
    const int currY = y + y0;
    const int nextY = currY + 1;
    const int rowOffset = Width * currY;
    const int nextRowOffset = Width * nextY;

    // Hoist pointers to the start of the "scanline" segment
    const uint8_t* chiPtr = &chi[rowOffset + x];
    const float* GchiPtr = &Gchi[rowOffset + x];
    const float* GchiNext = &Gchi[nextRowOffset + x];
    const float* GchiuPtr = &Gchiu[rowOffset + x];
    const float* GchiuNext = &Gchiu[nextRowOffset + x];

    const double blurY = BlurKernelY[y0];

    for (int x0 = xmin; x0 <= xmax; x0++)
    {
      // fast mask check
      if (chiPtr[x0]) continue; // This line is due to the 1I_O(x) factor in the numerator of J_s,r(x) in formula (21)

      // fetching neighbors (C = Center, E = East, S = South)
      float chiC = GchiPtr[x0];       /* 0----------  These lines fetch K_s */
      float chiE = GchiPtr[x0 + 1];   /* |   C E                            */
      float chiS = GchiNext[x0];      /* |   S                              */

      if (chiC == 0.0f || chiE == 0.0f || chiS == 0.0f) continue;

      // This is v_s(x)
      double vsC = static_cast<double>(GchiuPtr[x0]) / chiC;
      // This subtracts v_x(x+h*e1)
      double dx = vsC - (static_cast<double>(GchiuPtr[x0 + 1]) / chiE);
      // This subtracts v_x(x+h*e2)
      double dy = vsC - (static_cast<double>(GchiuNext[x0]) / chiS);

      // weighting
      double blur = BlurKernelX[x0] * blurY;
      chi0 += blur; /* This is K_r */
      xx += dx * dx * blur;
      xy += dx * dy * blur;
      yy += dy * dy * blur;
    }
  }

  if (chi0 == 0.0) return false;

  double invChi0 = 1.0 / chi0; // Use multiplication instead of 3 divisions
  *TensorXX = xx * invChi0;
  *TensorXY = xy * invChi0;
  *TensorYY = yy * invChi0;
  return true;
}



double *  CreateGaussKernel(double  Kernel[],  int  Size,  double  Deviation,  double  Center)
{
  signed int  k;

  Kernel += Size;
  if (Deviation>std::sqrt(4*std::sqrt(256.0*DBL_EPSILON/2.0*4.0*std::atan(1.0)*std::exp(1.0))))  /* 4 for safety */
  {
    Deviation = -0.5/(Deviation*Deviation);
    for (k=-Size ; k<=Size ; k++)  Kernel[k] = std::exp(Deviation*(Center-k)*(Center-k));
  }
  else
  {
    for (k=-Size ; k<=Size ; k++)  Kernel[k] = (std::abs(Center-k)<1.0)?(1.0-std::abs(Center-k)):0.0;
  }
  if (!Size)  Kernel[0] = 1.0;
  return  Kernel;
}



/****************************************************************
                        Deblend tools
 ****************************************************************/
void  DeblendDynamic(uint8_t  Data[],  const  uint8_t  LogoData[],  const  uint8_t  AlphaData[],  int  Pitch,  int  SampleStep,  int  LogoPitch,  int  LogoSampleStep,  int  AlphaPitch,  int  AlphaSampleStep,  int  Width,  int  Height)
/* Performs a simple deblend with given logo and alpha data         */
{
  int  x;
  float  Value;

  AlphaPitch -= AlphaSampleStep*Width;
  LogoPitch -= LogoSampleStep*Width;
  Pitch -= SampleStep*Width;
  while (Height--)
  {
    x = Width;
    while (x--)
    {
      const int  Alpha0    = AlphaData[0];
      const int  InvAlpha0 = 0xFF - Alpha0;
      if (InvAlpha0)
      {
        Value = static_cast<float>(0xFF*((int)(Data[0])) - Alpha0*((int)(LogoData[0])));
        Value *= 1.0f / static_cast<float>(InvAlpha0);
        int iValue = static_cast<int>(Value + 0.5f);
        iValue = std::min(std::max(iValue, 0), 255);
        Data[0] = static_cast<uint8_t>(iValue);
      }
      AlphaData += AlphaSampleStep;
      LogoData += LogoSampleStep;
      Data += SampleStep;
    }
    AlphaData += AlphaPitch;
    LogoData += LogoPitch;
    Data += Pitch;
  }
}



int  ParseChromaPlacement(const char *  Name)
{
  if (!Name || !*Name)  return  ChromaLoc_Auto;
  std::string  s(Name);
  for (auto& c : s)  c = static_cast<char>(std::tolower(static_cast<unsigned char>(c)));
  if (s == "auto")  return  ChromaLoc_Auto;
  if (s == "left" || s == "mpeg2")  return  ChromaLoc_Left;
  if (s == "center" || s == "mpeg1" || s == "jpeg")  return  ChromaLoc_Center;
  if (s == "top_left")  return  ChromaLoc_TopLeft;
  if (s == "top")  return  ChromaLoc_Top;
  if (s == "bottom_left")  return  ChromaLoc_BottomLeft;
  if (s == "bottom")  return  ChromaLoc_Bottom;
  if (s == "dv")  return  ChromaLoc_DV;
  return  ChromaLoc_Invalid;
}



int  ResolveChromaLocation(int  ChromaLoc,  int  FrameProp)
{
  if (ChromaLoc != ChromaLoc_Auto)  return  ChromaLoc;
  return  (FrameProp >= ChromaLoc_Left && FrameProp <= ChromaLoc_Bottom) ? FrameProp : ChromaLoc_Default;
}



/* Siting of channel ChannelNo along one axis: 0 = centered in the block, 1 = co-sited with its first, 2 = with its last luma sample */
static void  ChromaSiting(int  ChromaLoc,  int  ChannelNo,  int &  SitingX,  int &  SitingY)
{
  if (ChromaLoc == ChromaLoc_DV)  ChromaLoc = (ChannelNo == 2) ? ChromaLoc_TopLeft : ChromaLoc_BottomLeft;  /* V top, U bottom */
  SitingX = (ChromaLoc == ChromaLoc_Left || ChromaLoc == ChromaLoc_TopLeft || ChromaLoc == ChromaLoc_BottomLeft) ? 1 : 0;
  SitingY = (ChromaLoc == ChromaLoc_TopLeft || ChromaLoc == ChromaLoc_Top) ? 1 :
            (ChromaLoc == ChromaLoc_BottomLeft || ChromaLoc == ChromaLoc_Bottom) ? 2 : 0;
}



/* Taps (source index, weight) of each output sample along one axis */
static std::vector<std::vector<std::pair<int, float>>>  DownsampleTaps(int  Size,  int  P,  int  Siting)
{
  std::vector<std::vector<std::pair<int, float>>>  Taps(Size / P);
  for (int i = 0 ; i < Size / P ; i++)
  {
    if (Siting == 0)
    {
      for (int d = 0 ; d < P ; d++)  Taps[i].push_back({P*i + d, 1.0f / static_cast<float>(P)});
    }
    else
    {
      /* Triangle of width 2P around the co-sited luma sample; taps outside the frame are clamped to the edge */
      const int  c = P*i + ((Siting == 2) ? P - 1 : 0);
      for (int d = -(P - 1) ; d <= P - 1 ; d++)
        Taps[i].push_back({std::min(std::max(c + d, 0), Size - 1), static_cast<float>(P - std::abs(d)) / static_cast<float>(P*P)});
    }
  }
  return  Taps;
}



void  DownsampleAlpha(float  Dest[],  const float  Src[],  int  Width,  int  Height,  int  PixelWidth,  int  PixelHeight,  int  ChromaLoc,  int  ChannelNo)
{
  const int  ChWidth = Width / PixelWidth, ChHeight = Height / PixelHeight;
  int  SitingX, SitingY;
  ChromaSiting(ChromaLoc, ChannelNo, SitingX, SitingY);
  if (PixelWidth == 1)  SitingX = 0;
  if (PixelHeight == 1)  SitingY = 0;
  if (!SitingX && !SitingY)
  {
    /* Block average (the original behaviour, kept bit-exact) */
    std::fill(Dest, Dest + ChWidth*ChHeight, 0.0f);
    const float  InvPxCount = 1.0f / static_cast<float>(PixelWidth*PixelHeight);
    for (int y = 0 ; y < Height ; y++)
    {
      const int  ChRowStart = ChWidth * (y / PixelHeight);
      for (int x = 0 ; x < Width ; x++)  Dest[ChRowStart + x / PixelWidth] += Src[Width*y + x] * InvPxCount;
    }
    return;
  }
  const auto  TapsX = DownsampleTaps(Width, PixelWidth, SitingX);
  const auto  TapsY = DownsampleTaps(Height, PixelHeight, SitingY);
  std::vector<float>  Tmp(static_cast<size_t>(ChWidth) * Height);
  for (int y = 0 ; y < Height ; y++)
    for (int x = 0 ; x < ChWidth ; x++)
    {
      float  s = 0.0f;
      for (const auto& t : TapsX[x])  s += t.second * Src[Width*y + t.first];
      Tmp[ChWidth*y + x] = s;
    }
  for (int y = 0 ; y < ChHeight ; y++)
    for (int x = 0 ; x < ChWidth ; x++)
    {
      float  s = 0.0f;
      for (const auto& t : TapsY[y])  s += t.second * Tmp[ChWidth*t.first + x];
      Dest[ChWidth*y + x] = s;
    }
}



/* DownsampleAlpha on 8-bit alpha data for subsampling factors up to 2 (all supported formats).
   The kernels then only have weights k/2 or k/4 per axis, so integer sums scaled by a power of 2
   give exactly the float result of DownsampleAlpha, without a full-resolution float copy. */
static void  DownsampleAlpha8(float  Dest[],  const uint8_t  Src[],  int  Pitch,  int  Step,  int  Width,  int  Height,  int  PixelWidth,  int  PixelHeight,  int  SitingX,  int  SitingY)
{
  const int  ChWidth = Width / PixelWidth, ChHeight = Height / PixelHeight;
  const int  WeightX = (PixelWidth == 1) ? 1 : (SitingX ? 4 : 2);
  const int  WeightY = (PixelHeight == 1) ? 1 : (SitingY ? 4 : 2);
  const float  Norm = 1.0f / static_cast<float>(WeightX * WeightY);
  std::vector<uint16_t>  RowBuf(static_cast<size_t>(3) * ChWidth);
  uint16_t * Rows[3] = { RowBuf.data(), RowBuf.data() + ChWidth, RowBuf.data() + 2*ChWidth };
  int  RowY[3] = { -1, -1, -1 };
  /* Horizontal sums of luma row y, cached in one of three row buffers */
  auto  HSum = [&](int  y) -> const uint16_t *
  {
    for (int k = 0 ; k < 3 ; k++)  if (RowY[k] == y)  return  Rows[k];
    const int  k = y % 3;
    uint16_t * Out = Rows[k];
    RowY[k] = y;
    const uint8_t * r = Src + static_cast<ptrdiff_t>(Pitch) * y;
    if (PixelWidth == 1)
      for (int x = 0 ; x < ChWidth ; x++)  Out[x] = r[x*Step];
    else if (!SitingX)
      for (int x = 0 ; x < ChWidth ; x++)  Out[x] = static_cast<uint16_t>(r[2*x*Step] + r[(2*x+1)*Step]);
    else
    {
      /* [1,2,1] around the left luma sample; the sample left of the frame is clamped to the edge */
      Out[0] = static_cast<uint16_t>(3*r[0] + r[Step]);
      for (int x = 1 ; x < ChWidth ; x++)  Out[x] = static_cast<uint16_t>(r[(2*x-1)*Step] + 2*r[2*x*Step] + r[(2*x+1)*Step]);
    }
    return  Out;
  };
  for (int y = 0 ; y < ChHeight ; y++)
  {
    float * d = Dest + static_cast<size_t>(ChWidth) * y;
    if (PixelHeight == 1)
    {
      const uint16_t * a = HSum(y);
      for (int x = 0 ; x < ChWidth ; x++)  d[x] = static_cast<float>(a[x]) * Norm;
    }
    else if (!SitingY)
    {
      const uint16_t * a = HSum(2*y), * b = HSum(2*y + 1);
      for (int x = 0 ; x < ChWidth ; x++)  d[x] = static_cast<float>(a[x] + b[x]) * Norm;
    }
    else
    {
      /* [1,2,1] around the top (SitingY 1) or bottom (SitingY 2) luma row, clamped to the frame */
      const int  c = 2*y + ((SitingY == 2) ? 1 : 0);
      const uint16_t * a = HSum(std::max(c - 1, 0)), * b = HSum(c), * e = HSum(std::min(c + 1, Height - 1));
      for (int x = 0 ; x < ChWidth ; x++)  d[x] = static_cast<float>(a[x] + 2*b[x] + e[x]) * Norm;
    }
  }
}



void  DeblendDynamicSubsampled(uint8_t  Data[],  const  uint8_t  LogoData[],  const  uint8_t  AlphaData[],  int  Pitch,  int  SampleStep,  int  LogoPitch,  int  LogoSampleStep,  int  AlphaPitch,  int  AlphaSampleStep,  int  Width,  int  Height,  int  PixelWidth,  int  PixelHeight,  int  ChromaLoc,  int  ChannelNo,  SubsampledAlphaCache &  Cache)
/* Performs a simple deblend with given logo and alpha data, where
   the source and logo data are subsampled with respect to the
   alpha data.                                                      */
{
  int  SitingX, SitingY;
  ChromaSiting(ChromaLoc, ChannelNo, SitingX, SitingY);
  if (PixelWidth == 1)  SitingX = 0;
  if (PixelHeight == 1)  SitingY = 0;
  const int  Key = ((SitingX*3 + SitingY)*8 + PixelWidth)*8 + PixelHeight;
  if (Cache.Key != Key)
  {
    Cache.Alpha.resize(static_cast<size_t>(Width / PixelWidth) * (Height / PixelHeight));
    if (PixelWidth <= 2 && PixelHeight <= 2)
      DownsampleAlpha8(Cache.Alpha.data(), AlphaData, AlphaPitch, AlphaSampleStep, Width, Height, PixelWidth, PixelHeight, SitingX, SitingY);
    else
    {
      std::vector<float>  FullAlpha(static_cast<size_t>(Width) * Height);
      for (int y = 0 ; y < Height ; y++)
        for (int x = 0 ; x < Width ; x++)
          FullAlpha[static_cast<size_t>(Width)*y + x] = static_cast<float>(AlphaData[static_cast<ptrdiff_t>(AlphaPitch)*y + x*AlphaSampleStep]);
      DownsampleAlpha(Cache.Alpha.data(), FullAlpha.data(), Width, Height, PixelWidth, PixelHeight, ChromaLoc, ChannelNo);
    }
    Cache.Key = Key;
  }
  const float * AlphaPtr = Cache.Alpha.data();
  Width /= PixelWidth;
  Height /= PixelHeight;
  LogoPitch -= LogoSampleStep*Width;
  Pitch -= SampleStep*Width;
  while (Height--)
  {
    int  x = Width;
    while (x--)
    {
      /* Same as (255*Data - Alpha*Logo) / (255 - Alpha); for the block average this is bit-identical
         to the original integer sums, as both only differ by the power-of-2 factor PixelWidth*PixelHeight */
      const float  a = *(AlphaPtr++);
      const float  InvAlpha = 255.0f - a;
      if (InvAlpha > 0.0f)
      {
        float  Value = 255.0f*static_cast<float>(Data[0]) - a*static_cast<float>(LogoData[0]);
        Value *= 1.0f / InvAlpha;
        int iValue = static_cast<int>(Value + 0.5f);
        iValue = std::min(std::max(iValue, 0), 255);
        Data[0] = static_cast<uint8_t>(iValue);
      }
      LogoData += LogoSampleStep;
      Data += SampleStep;
    }
    LogoData += LogoPitch;
    Data += Pitch;
  }
}



void  DeblendStatic(uint8_t  Data[],  const  uint16_t  LogoData[],  const  uint8_t  AlphaData[],  int  Pitch,  int  SampleStep,  int  Width,  int  Height)
/* This deblends a logo with preprocessed data (by
   DeblendStaticPrepare)                                            */
{
  float  Value;
  int  W;

  Pitch -= SampleStep*Width;
  while (Height--)
  {
    W = Width;
    while (W--)
    {
      const int  InvAlpha0 = AlphaData[0];  /* stored as 255-alpha */
      if (InvAlpha0)
      {
        Value = static_cast<float>(255*Data[0] - LogoData[0]);
        Value *= 1.0f / static_cast<float>(InvAlpha0);
        int iValue = static_cast<int>(Value + 0.5f);
        iValue = std::min(std::max(iValue, 0), 255);
        Data[0] = static_cast<uint8_t>(iValue);
      }
      AlphaData++;
      LogoData++;
      Data += SampleStep;
    }
    Data += Pitch;
  }
}



int  DeblendStaticPrepare(uint16_t *  LogoDataChannels[],  uint8_t *  AlphaDataChannels[],
                           const FrameLayout &  logoFmt,  int  Width,  int  Height,
                           const uint8_t *  logoPlanes[],  int  logoPitch[],
                           const uint8_t *  alphaPlane,  int  alphaPitch,  int  alphaSampleStep,
                           int  ChromaLoc)
/* If the logo to be deblended is static (not moving), some values
   can be prepared prior to deblending. Those values will be
   stored in the LogoData/AlphaData vectors, which are later
   handed over to the static-deblend-function.
   alphaPlane==nullptr means alpha is embedded in the last channel of logoPlanes;
   otherwise the caller has already applied any packed-format offset and row-flip. */
{
  const  uint8_t * SrcData, * AlphaData0;
  uint8_t * AlphaDestData;
  uint16_t * DestData;
  int  AlphaPitch0, AlphaSampleStep0;
  int  ChannelNo, ChannelCount, x, y, Alpha, Logo;

  ChannelCount = logoFmt.channelCount;
  if ((!ChannelCount) || ((ChannelCount<4) && (!alphaPlane)))  return  -1;
  if (alphaPlane)
  {
    AlphaData0 = alphaPlane;
    AlphaPitch0 = alphaPitch;
    AlphaSampleStep0 = alphaSampleStep;
  }
  else
  {
    ChannelCount--;
    AlphaData0 = logoPlanes[ChannelCount];
    AlphaPitch0 = logoPitch[ChannelCount];
    AlphaSampleStep0 = logoFmt.sampleStep[ChannelCount];
  }
  std::vector<float>  FullAlpha(static_cast<size_t>(Width) * Height), ChannelAlpha;
  for (y=0 ; y<Height ; y++)
    for (x=0 ; x<Width ; x++)
      FullAlpha[static_cast<size_t>(Width)*y + x] = static_cast<float>(AlphaData0[static_cast<ptrdiff_t>(AlphaPitch0)*y + x*AlphaSampleStep0]);
  for (ChannelNo=0 ; ChannelNo<ChannelCount ; ChannelNo++)
  {
    const int    PW            = logoFmt.pixelWidth[ChannelNo];
    const int    PH            = logoFmt.pixelHeight[ChannelNo];
    const int    ChSampleStep  = logoFmt.sampleStep[ChannelNo];
    DestData = LogoDataChannels[ChannelNo];
    if (!DestData)  continue;
    SrcData = logoPlanes[ChannelNo];
    AlphaDestData = AlphaDataChannels[ChannelNo];
    ChannelAlpha.resize(static_cast<size_t>(Width/PW) * (Height/PH));
    DownsampleAlpha(ChannelAlpha.data(), FullAlpha.data(), Width, Height, PW, PH, ChromaLoc, ChannelNo);
    for (y=0 ; y<Height/PH ; y++)
    {
      for (x=0 ; x<Width/PW ; x++)
      {
        Alpha = static_cast<int>(ChannelAlpha[Width/PW*y+x] + 0.5f);
        Logo = Alpha*SrcData[logoPitch[ChannelNo]*y+x*ChSampleStep];
        if (AlphaDestData)  AlphaDestData[Width/PW*y+x] = static_cast<uint8_t>(0xFF-Alpha);
        DestData[Width/PW*y+x] = static_cast<uint16_t>(Logo);
      }
    }
  }
  return  0;
}



#ifdef ENABLE_AVISYNTH
int  AvsDeblendStaticPrepare(uint16_t *  LogoDataChannels[],  uint8_t *  AlphaDataChannels[],
                              AVS_VideoFrame *  LogoFrame,  AVS_VideoFrame *  AlphaFrame,
                              const  AVS_VideoInfo *  LogoVideoInfo,  const  AVS_VideoInfo *  AlphaVideoInfo,  int  AlphaChannel,
                              bool  FlipLogo,  bool  FlipAlpha,  int  ChromaLoc)
/* FlipLogo/FlipAlpha: the clip is stored in the other vertical direction than the source (packed vs planar RGB) */
{
  const FrameLayout  logoFmt = FrameLayoutFromPixelType(LogoVideoInfo->pixel_type);
  const uint8_t * logoPlanes[MaxChannelCount] = {};
  int  logoPitch[MaxChannelCount] = {};
  if (!AvsGetFramePlanesRO(LogoFrame, logoFmt, logoPlanes, logoPitch))  return  -1;
  if (FlipLogo)
  {
    for (int i=0 ; i<logoFmt.channelCount ; i++)
    {
      logoPlanes[i] += static_cast<ptrdiff_t>(LogoVideoInfo->height/logoFmt.pixelHeight[i] - 1) * logoPitch[i];
      logoPitch[i] *= -1;
    }
  }
  const uint8_t * alphaPlane = nullptr;
  int  alphaPitch = 0, alphaSampleStep = 0;
  if (AlphaFrame)
  {
    const FrameLayout  alphaFmt = FrameLayoutFromPixelType(AlphaVideoInfo->pixel_type);
    const uint8_t * alphaPlanes[MaxChannelCount] = {};
    int  alphaPlanesPitch[MaxChannelCount] = {};
    if (!AvsGetFramePlanesRO(AlphaFrame, alphaFmt, alphaPlanes, alphaPlanesPitch))  return  -1;
    alphaPlane = alphaPlanes[AlphaChannel];
    alphaPitch = alphaPlanesPitch[AlphaChannel];
    alphaSampleStep = alphaFmt.sampleStep[AlphaChannel];
    if (FlipAlpha)
    {
      alphaPlane += static_cast<ptrdiff_t>(LogoVideoInfo->height - 1) * alphaPitch;
      alphaPitch *= -1;
    }
  }
  return  DeblendStaticPrepare(LogoDataChannels, AlphaDataChannels,
                                logoFmt, LogoVideoInfo->width, LogoVideoInfo->height,
                                logoPlanes, logoPitch,
                                alphaPlane, alphaPitch, alphaSampleStep, ChromaLoc);
}



/* ChromaPlacement argument -> ChromaLocation. "auto" (or not given) takes _ChromaLocation of the first
   frame of Clip, if the host supports frame properties and it is set. */
static int  AvsGetChromaLocation(AVS_ScriptEnvironment *  Env,  AVS_Clip *  Clip,  const AVS_Value &  Arg,  const char *  InvalidText,  const char **  ErrorText)
{
  const int  ChromaLoc = ParseChromaPlacement(avs_is_string(Arg) ? avs_as_string(Arg) : nullptr);
  if (ChromaLoc == ChromaLoc_Invalid)  { *ErrorText = InvalidText;  return  ChromaLoc_Default; }
  int  FrameProp = -1;
  if (ChromaLoc == ChromaLoc_Auto && avs_get_frame_props_ro && avs_prop_get_int)
  {
    AVS_VideoFrame * Frame = avs_get_frame(Clip, 0);
    if (Frame)
    {
      int  err = 0;
      const int64_t  v = avs_prop_get_int(Env, avs_get_frame_props_ro(Env, Frame), "_ChromaLocation", 0, &err);
      if (!err)  FrameProp = static_cast<int>(v);
      avs_release_video_frame(Frame);
    }
  }
  return  ResolveChromaLocation(ChromaLoc, FrameProp);
}
#endif  // ENABLE_AVISYNTH



/****************************************************************
                      Analyze tools
 ****************************************************************/
int32_t  Analyze(const FrameLayout &  srcFmt,  int  Width,  int  Height,  unsigned int  FrameCount,
                 std::function<bool(unsigned int, const uint8_t*[], int[])>  fetchSrcFrame,
                 std::function<void(unsigned int)>                           releaseSrcFrame,
                 uint8_t *  destPlanes[],  int  destPitch[],  bool  destHasAlphaRow,
                 unsigned char  Mask[],  int32_t  MaskSize,  int  ComputeAlpha,
                 double  DeviationWeight,  double  SubsamplingWeight,  int  ChromaLoc)
/* This function evaluates the source frames and estimates the logo */
{
  const unsigned int Sum1MaxDelta = 0x80u;
  const uint32_t Sum2MaxDelta = static_cast<uint32_t>(Sum1MaxDelta)*static_cast<uint32_t>(Sum1MaxDelta);
  #define  KahanAdditionMakro(S,A,C,T,Y)  {Y=(A);Y-=C;T=S+Y;C=T-S;C-=Y;S=T;}  /* Kahan's summation algorithm reduces numerical problems - hopefully compiler optimization won't destroy this */

  std::array<std::vector<Analyze_Sum1Type>, MaxChannelCount> Sum1;
  std::array<std::vector<Analyze_Sum2Type>, MaxChannelCount> Sum2;
  Analyze_Sum1Type * Sum1Ptr;
  Analyze_Sum2Type * Sum2Ptr;
  int  x, y, ChannelNo, ChannelCount;
  int32_t  k;
  double  ChannelNMVariance[MaxChannelCount], ChannelNMAverage[MaxChannelCount];
  int  Result;
  int  FramePitch[MaxChannelCount];  /* used in Phase 1 for source frame pitches */
  /* Variables for Phase 1 */
  unsigned int  FrameNo, FramesLeft;
  Analyze_Sum1Type  Sum1Max;
  Analyze_Sum2Type  Sum2Max;
  signed int  Value12;
  const  uint8_t * SrcData[MaxChannelCount];
  const  uint8_t * SrcPtr;
  int  SrcPtrDelta, DividedWidth, SrcSampleStep;
  /* Variables for Phase 2 */
  uint8_t * ChannelMask;
  /* Downsampled logo mask for subsampled chroma channels (same layout as ChannelAlphaVector).
     Each macro-pixel is masked if any of its full-res pixels is masked.
     Not needed for RGB or YV24 where all channels share full resolution. */
  std::vector<uint8_t> ChannelMaskVector;
  /* Non-masked pixel sums collected per channel for computing average and variance.
     Allocated at full-frame size (Width*Height-MaskSize) as an upper bound;
     only the first k elements are filled and sorted per channel iteration. */
  std::vector<Analyze_Sum1Type> NMSum1;
  std::vector<Analyze_Sum2Type> NMSum2;  // only populated when ComputeAlpha
  double  ChannelNMSum1[MaxChannelCount], ChannelNMSum2[MaxChannelCount], KahanSum, KahanC, KahanY, KahanT;
  int  ChannelMaskSize;
  /* Variables for Phase 3 */
  float * AlphaChannel, * ChannelAlpha;
  /* Downsampled alpha for subsampled chroma channels (e.g. YUY2/YV16 half-width, YV12 quarter-res).
     Not needed for RGB or YV24 where all channels share the same resolution as the luma/alpha. */
  std::vector<float> ChannelAlphaVector;
  double  PixelValue, AlphaSum, AlphaWeightSum, AlphaWeight[MaxChannelCount];
  uint8_t * AlphaData, * DestPtr;
  int  AlphaPitch, AlphaSampleStep;


  /*** PHASE 0 :  Allocating memory ***/
  ChannelCount = srcFmt.channelCount - (destHasAlphaRow ? 0 : 1);
  /* Allocate memory for sum and square-sum vectors (depending on color space) */
  if (ChannelCount<=0)  return  Analyze_Result_GeneralError;  /* Unknown color space (a bug!) */
  try
  {
    // resize performs zero-init as well
    for (ChannelNo=0 ; ChannelNo<ChannelCount ; ChannelNo++)  Sum1[ChannelNo].resize(Width/srcFmt.pixelWidth[ChannelNo]*Height/srcFmt.pixelHeight[ChannelNo]);
    if (ComputeAlpha)  for (ChannelNo=0 ; ChannelNo<ChannelCount ; ChannelNo++)  Sum2[ChannelNo].resize(Width/srcFmt.pixelWidth[ChannelNo]*Height/srcFmt.pixelHeight[ChannelNo]);
  }
  catch (std::bad_alloc&)  { return  Analyze_Result_MemoryError; }
  /*** PHASE 1: Summing up (over time) all pixel values and their squares (Dear user: this will take some time!!) ***/
  // This ensures no individual accumulator cell overflows regardless of resolution,
  // each cell accumulates exactly FramesLeft values, bounded by INT32_MAX/UINT32_MAX limits.
  FramesLeft = FrameCount;
  if (ComputeAlpha)  if (FramesLeft>Analyze_Sum2Max/Sum2MaxDelta)  FramesLeft = static_cast<unsigned int>(Analyze_Sum2Max/Sum2MaxDelta);
  if (FramesLeft>static_cast<unsigned int>(Analyze_Sum1Max/Sum1MaxDelta))  FramesLeft = static_cast<unsigned int>(Analyze_Sum1Max/Sum1MaxDelta);
  FrameNo = 0;
  Result = Analyze_Result_OK;
  /** MAIN LOOP **/
  /* This loops over all frames of the source clip */
  while (FramesLeft)
  {
    /* Get source frame and data pointers via callback */
    if (!fetchSrcFrame(FrameNo, SrcData, FramePitch))
    {
      Result = Analyze_Result_SrcClipError;
      break;
    }
    /* Loop over all color channels */
    for (ChannelNo=0 ; ChannelNo<ChannelCount ; ChannelNo++)
    {
      Sum1Ptr = Sum1[ChannelNo].data();
      Sum2Ptr = ComputeAlpha ? Sum2[ChannelNo].data() : nullptr;
      SrcPtr = SrcData[ChannelNo];
      SrcSampleStep = srcFmt.sampleStep[ChannelNo];
      DividedWidth = Width/srcFmt.pixelWidth[ChannelNo];
      SrcPtrDelta = FramePitch[ChannelNo]-DividedWidth*SrcSampleStep;
      /* This is the critical loop: Here all color values of all source frames are added up. Fast code will be rewarded with fast computation. */
      y = Height/srcFmt.pixelHeight[ChannelNo];
      if (ComputeAlpha)
      {
        while (y--)
        {
          x = DividedWidth;
          while (x--)
          {
            *(Sum1Ptr++) += Value12 = (static_cast<signed int>(SrcPtr[0]))-(static_cast<signed int>(0x80));  /* Shift all values in the range [-128..127] to reduce squared values */
            *(Sum2Ptr++) += Value12*Value12;
            SrcPtr += SrcSampleStep;
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
            *(Sum1Ptr++) += (static_cast<signed int>(SrcPtr[0]))-(static_cast<signed int>(0x80));  /* Shift all values in the range [-128..127] for compatibility with the idea described above */
            SrcPtr += SrcSampleStep;
          }
          SrcPtr += SrcPtrDelta;
        }
      }
    }
    /* Release source frame */
    releaseSrcFrame(FrameNo);
    FramesLeft--;
    FrameNo++;
    if (!FramesLeft && (FrameNo!=FrameCount))
    {
      Sum1Max = 0;
      Sum2Max = 0;
      for (ChannelNo=0 ; ChannelNo<ChannelCount ; ChannelNo++)
      {
        Sum1Ptr = Sum1[ChannelNo].data();
        Sum2Ptr = ComputeAlpha ? Sum2[ChannelNo].data() : nullptr;
        for (k=0 ; k<Width/srcFmt.pixelWidth[ChannelNo]*Height/srcFmt.pixelHeight[ChannelNo] ; k++)
        {
          if (Sum1Max<((Sum1Ptr[k]<0)?(-Sum1Ptr[k]):(Sum1Ptr[k])))  Sum1Max = (Sum1Ptr[k]<0)?(-Sum1Ptr[k]):(Sum1Ptr[k]);
          if (Sum2Ptr)  if (Sum2Max<Sum2Ptr[k])  Sum2Max = Sum2Ptr[k];
        }
      }
      FramesLeft = FrameCount-FrameNo;
      if (ComputeAlpha)  if (FramesLeft>(Analyze_Sum2Max-Sum2Max)/Sum2MaxDelta)  FramesLeft = static_cast<unsigned int>((Analyze_Sum2Max-Sum2Max)/Sum2MaxDelta);
      if (FramesLeft>static_cast<unsigned int>((Analyze_Sum1Max-Sum1Max)/Sum1MaxDelta))  FramesLeft = static_cast<unsigned int>((Analyze_Sum1Max-Sum1Max)/Sum1MaxDelta);
    }
  }
  /* If (whyever) there is still an unreleased frame, release it now */
  releaseSrcFrame(FrameNo);
  /* If we have less than two frames, we return an error (maybe Sum1/2Max is too low..??) */
  if (ComputeAlpha)  if (FrameNo<2)  Result = Analyze_Result_GeneralError;
  if (FrameNo<1)  Result = Analyze_Result_GeneralError;
  /* If a (serious) error occured, we return */
  if (Result!=Analyze_Result_OK)  return  Result;
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
  ChannelNo = 0;
  while (ChannelNo<ChannelCount)
  {
    if (srcFmt.pixelWidth[ChannelNo]*srcFmt.pixelHeight[ChannelNo]>1)  break;
    ChannelNo++;
  }
  // ChannelNo is the first subsampled channel number and exited early, 
  // or reaches ChannelCount is no subsampled channel is present (e.g. RGB or YV24).
  try
  {
    if (ChannelNo<ChannelCount)  ChannelMaskVector.resize((Width/srcFmt.pixelWidth[ChannelNo])*(Height/srcFmt.pixelHeight[ChannelNo]));
    NMSum1.resize(Width*Height-MaskSize);
    if (ComputeAlpha)  NMSum2.resize(Width*Height-MaskSize);
  }
  catch (std::bad_alloc&)  { return  Analyze_Result_MemoryError; }
  /** Main loop in Phase 2: Each channel is processed completely **/
  ChannelMask = Mask;
  ChannelMaskSize = static_cast<int>(MaskSize);
  for (ChannelNo=0 ; ChannelNo<ChannelCount ; ChannelNo++)
  {
    const int  PW      = srcFmt.pixelWidth[ChannelNo];
    const int  PH      = srcFmt.pixelHeight[ChannelNo];
    const int  ChWidth = Width  / PW;
    const int  ChHeight= Height / PH;
    /* DV siting differs between U and V, so the downsampled mask is only reused for an identical siting */
    if (!ChannelNo || PW!=srcFmt.pixelWidth[ChannelNo-1] || PH!=srcFmt.pixelHeight[ChannelNo-1] || ChromaLoc==ChromaLoc_DV)
    {
      if (PW*PH==1)
      {
        // not subsampled, mask can be used as is
        ChannelMask = Mask;
        ChannelMaskSize = static_cast<int>(MaskSize);
      }
      else
      {
        /* A chroma sample is masked if any luma sample it is computed from (see DownsampleAlpha) is masked */
        std::vector<float>  MaskF(static_cast<size_t>(Width) * Height), ChMaskF(static_cast<size_t>(ChWidth) * ChHeight);
        for (k=0 ; k<Width*Height ; k++)  MaskF[k] = Mask[k] ? 1.0f : 0.0f;
        DownsampleAlpha(ChMaskF.data(), MaskF.data(), Width, Height, PW, PH, ChromaLoc, ChannelNo);
        ChannelMask = ChannelMaskVector.data();
        for (k=0 ; k<ChWidth*ChHeight ; k++)  ChannelMask[k] = (ChMaskF[k] > 0.0f) ? 1 : 0;
        /* Determine size of sub-sampled mask */
        ChannelMaskSize = 0;
        for (k=0 ; k<ChWidth*ChHeight ; k++)  ChannelMaskSize += ChannelMask[k] ? 1 : 0;
      }
    }
    Sum1Ptr = Sum1[ChannelNo].data();
    Sum2Ptr = ComputeAlpha ? Sum2[ChannelNo].data() : nullptr;
    k = 0;
    if (ComputeAlpha)
    {
      for (y=0 ; y<ChHeight ; y++)
      {
        for (x=0 ; x<ChWidth ; x++)
        {
          if (!ChannelMask[ChWidth*y+x])
          {
            NMSum1[k] = Sum1Ptr[ChWidth*y+x];
            NMSum2[k] = Sum2Ptr[ChWidth*y+x];
            k++;
          }
        }
      }
    }
    else
    {
      for (y=0 ; y<ChHeight ; y++)
      {
        for (x=0 ; x<ChWidth ; x++)
        {
          if (!ChannelMask[ChWidth*y+x])
          {
            NMSum1[k] = Sum1Ptr[ChWidth*y+x];
            k++;
          }
        }
      }
    }
    /* Assertion: If mask size has changed, something must be wrong !! */
    if (ChWidth*ChHeight != ChannelMaskSize+k)  return  Analyze_Result_GeneralError;
    // Always: k == total_channel_pixels - ChannelMaskSize
    // as the assertion above checks that.

    /* Sort only the first k elements (non-masked pixels for this channel, a subset of the full buffer).
       NMSum1: sorted by absolute value — reduces Kahan summation numerical error by adding small values first.
       NMSum2: sorted ascending — same reason. */
    const int  NMCount = k;  // number of non-masked pixels collected for this channel
    std::sort(NMSum1.begin(), NMSum1.begin()+NMCount,
              [](Analyze_Sum1Type a, Analyze_Sum1Type b){ return std::abs(a) < std::abs(b); });
    KahanSum = 0.0;
    KahanC = 0.0;
    for (k=0 ; k<NMCount ; k++)  KahanAdditionMakro(KahanSum,NMSum1[k],KahanC,KahanT,KahanY);
    ChannelNMSum1[ChannelNo] = KahanSum;
    if (ComputeAlpha)
    {
      std::sort(NMSum2.begin(), NMSum2.begin()+NMCount);  // ascending, unsigned values
      KahanSum = 0.0;
      KahanC = 0.0;
      for (k=0 ; k<NMCount ; k++)  KahanAdditionMakro(KahanSum,NMSum2[k],KahanC,KahanT,KahanY);
      ChannelNMSum2[ChannelNo] = KahanSum;
    }
    /* Compute averages and variances of non-masked pixels */
    k = NMCount;
    ChannelNMAverage[ChannelNo] = ChannelNMSum1[ChannelNo]/k/FrameCount;
    if (ComputeAlpha)
    {
      ChannelNMVariance[ChannelNo] = ChannelNMSum2[ChannelNo]-ChannelNMSum1[ChannelNo]*ChannelNMSum1[ChannelNo]/k/FrameCount;
      ChannelNMVariance[ChannelNo] /= static_cast<double>(k)*static_cast<double>(FrameCount)-1.0;
      if (!(ChannelNMVariance[ChannelNo]>0.0))  ChannelNMVariance[ChannelNo] = 0.0;
    }
  }
  /*** PHASE 3: Compute transparency values and color values for each pixel of the logo (each mask pixel) and store in destination frame ***/
  /** We start with the alpha channel - it is needed to compute the color channels **/
  std::vector<float> AlphaChannelVec;
  try  { AlphaChannelVec.resize(Width*Height); }
  catch (std::bad_alloc&)  { return  Analyze_Result_MemoryError; }
  AlphaChannel = AlphaChannelVec.data();
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
      if (DeviationWeight>2*FLT_EPSILON)  AlphaWeight[ChannelNo] *= std::pow(ChannelNMVariance[ChannelNo],0.5*DeviationWeight);
      if (SubsamplingWeight>FLT_EPSILON)  AlphaWeight[ChannelNo] /= std::pow(static_cast<double>(srcFmt.pixelWidth[ChannelNo]*srcFmt.pixelHeight[ChannelNo]),SubsamplingWeight);
      if (!(AlphaWeight[ChannelNo]>0.0))  AlphaWeight[ChannelNo] = 0.0;
      AlphaWeightSum += AlphaWeight[ChannelNo];
    }
    if (AlphaWeightSum>0.0)
    {
      for (ChannelNo=0 ; ChannelNo<ChannelCount ; ChannelNo++)  AlphaWeight[ChannelNo] /= AlphaWeightSum;
      /* Transparency estimate of one channel sample (k: index in the channel's own resolution) */
      auto  ChannelTransparency = [&](int  Ch,  int  kk) -> double
      {
        double  v = static_cast<double>(Sum2[Ch][kk])-static_cast<double>(Sum1[Ch][kk])*static_cast<double>(Sum1[Ch][kk])/FrameCount;
        if (!(v>0.0))  v = 0.0;
        v = std::sqrt(v/(FrameCount-1)/ChannelNMVariance[Ch]);
        if (!(v>0.0))  v = 0.0;
        if (v>1.0)  v = 1.0;
        return  v;
      };
      bool  HasSubsampled = false;
      double  FullWeightSum = 0.0;
      for (ChannelNo=0 ; ChannelNo<ChannelCount ; ChannelNo++)
      {
        if (srcFmt.pixelWidth[ChannelNo]*srcFmt.pixelHeight[ChannelNo]>1)  HasSubsampled = true;
        else  FullWeightSum += AlphaWeight[ChannelNo];
      }
      if (HasSubsampled && FullWeightSum>0.0)
      {
        /* A subsampled channel only knows the transparency of a whole block of luma pixels. Using that value
           for each pixel of the block turns sharp alpha edges into blocks. Instead the full resolution estimate
           is kept for the detail and the subsampled channel corrects its block mean:
             T(x) = Tfull(x) + Tsub(block) - Downsample(Tfull)(block)                                        */
        std::vector<float>  TFull(static_cast<size_t>(Width)*Height), TFullDown;
        for (k=0 ; k<Width*Height ; k++)
        {
          double  s = 0.0;
          for (ChannelNo=0 ; ChannelNo<ChannelCount ; ChannelNo++)
            if (srcFmt.pixelWidth[ChannelNo]*srcFmt.pixelHeight[ChannelNo]==1)  s += AlphaWeight[ChannelNo]*ChannelTransparency(ChannelNo,k);
          TFull[k] = static_cast<float>(s/FullWeightSum);
        }
        std::vector<double>  AlphaSumVec(static_cast<size_t>(Width)*Height);
        for (k=0 ; k<Width*Height ; k++)  AlphaSumVec[k] = FullWeightSum*TFull[k];
        for (ChannelNo=0 ; ChannelNo<ChannelCount ; ChannelNo++)
        {
          const int  PW = srcFmt.pixelWidth[ChannelNo], PH = srcFmt.pixelHeight[ChannelNo];
          if (PW*PH==1)  continue;
          const int  ChWidth = Width/PW;
          TFullDown.resize(static_cast<size_t>(ChWidth)*(Height/PH));
          DownsampleAlpha(TFullDown.data(), TFull.data(), Width, Height, PW, PH, ChromaLoc, ChannelNo);
          for (y=0 ; y<Height ; y++)
            for (x=0 ; x<Width ; x++)
            {
              const int  kc = ChWidth*(y/PH)+(x/PW);
              double  v = TFull[Width*y+x] + ChannelTransparency(ChannelNo,kc) - TFullDown[kc];
              v = std::min(std::max(v, 0.0), 1.0);
              AlphaSumVec[Width*y+x] += AlphaWeight[ChannelNo]*v;
            }
        }
        for (k=0 ; k<Width*Height ; k++)
        {
          AlphaSum = AlphaSumVec[k];
          if (AlphaSum>1.0)  AlphaSum = 1.0;
          if (!(AlphaSum<2.0))  AlphaSum = 1.0;
          AlphaChannel[k] = static_cast<float>(1.0-AlphaSum);  /* = Opacity */
        }
      }
      else
      for (y=0 ; y<Height ; y++)
      {
        for(x=0 ; x<Width ; x++)
        {
          AlphaSum = 0.0;
          for (ChannelNo=0 ; ChannelNo<ChannelCount ; ChannelNo++)
          {
            Sum1Ptr = Sum1[ChannelNo].data();
            Sum2Ptr = Sum2[ChannelNo].data();
            k = (Width/srcFmt.pixelWidth[ChannelNo])*(y/srcFmt.pixelHeight[ChannelNo])+(x/srcFmt.pixelWidth[ChannelNo]);
            PixelValue = static_cast<double>(Sum2Ptr[k])-static_cast<double>(Sum1Ptr[k])*static_cast<double>(Sum1Ptr[k])/FrameCount;
            if (!(PixelValue>0.0))  PixelValue = 0.0;
            PixelValue = std::sqrt(PixelValue/(FrameCount-1)/ChannelNMVariance[ChannelNo]);
            if (!(PixelValue>0.0))  PixelValue = 0.0;
            if (PixelValue>1.0)  PixelValue = 1.0;
            AlphaSum += AlphaWeight[ChannelNo]*PixelValue;
          }
          if (AlphaSum>1.0)  AlphaSum = 1.0;
          if (!(AlphaSum<2.0))  AlphaSum = 1.0;  /* Emergency stop if division by zero or other shit occured */
          AlphaChannel[Width*y+x] = static_cast<float>(1.0-AlphaSum);  /* = Opacity */
        }
      }
    }
    else
    {
      for (k=0 ; k<Width*Height ; k++)  AlphaChannel[k] = 0.0f;
    }
  }
  else
  {
    for (k=0 ; k<Width*Height ; k++)  AlphaChannel[k] = static_cast<float>(Mask[k])/static_cast<float>(0xFF);
  }
  /* Now Sum2 is not needed anymore */
  for (auto& v : Sum2)  { v.clear(); v.shrink_to_fit(); }
  /** The destination frame pointers are determined **/
  /* Now we're approaching color-space dependent code !! */
  /* Packed RGB (RGB24/RGB32) is stored bottom-up, so its first Height rows in memory are the lower
     half of the picture. Planar RGB (RGBP/RGBAP, VapourSynth RGB) is stored top-down like YUV. */
  const bool  PackedRGB = (srcFmt.colorFamily == ColorFamily::RGB) && (srcFmt.sampleStep[0] > 1);
  if (srcFmt.colorFamily != ColorFamily::RGB || (destHasAlphaRow && !PackedRGB))  /* YUV, Gray or planar RGB with alpha row */
  {
    AlphaData = destPlanes[0]+Height*destPitch[0];
    AlphaPitch = destPitch[0];
    AlphaSampleStep = srcFmt.sampleStep[0];
  }
  else
  {
    AlphaData = destPlanes[destHasAlphaRow ? 0 : 3];  /* For RGB24 we have to copy the alpha values to the green and blue channel later ! */
    if (destHasAlphaRow)  for (ChannelNo=0 ; ChannelNo<ChannelCount ; ChannelNo++)  destPlanes[ChannelNo] += Height*destPitch[ChannelNo];
    AlphaPitch = destPitch[0];
    AlphaSampleStep = srcFmt.sampleStep[0];
  }
  /** The alpha channel is stored in the destination frame **/
  for (y=0 ; y<Height ; y++)
  {
    for(x=0 ; x<Width ; x++)
    {
      const int iAlpha = static_cast<int>(AlphaChannel[Width * y + x] * 255.0f + 0.5f);
      AlphaData[y*AlphaPitch+x*AlphaSampleStep] = static_cast<uint8_t>(iAlpha);  /* 0=transparent 255=opaque */
    }
  }
  /** Now we compute the color channels **/
  ChannelNo = 0;
  while (ChannelNo<ChannelCount)
  {
    /* Only needed if at least one color channel is subsampled (YUY2, YV16, YV12/I420) */
    if (srcFmt.pixelWidth[ChannelNo]*srcFmt.pixelHeight[ChannelNo]>1)
    {
      try  { ChannelAlphaVector.resize((Width/srcFmt.pixelWidth[ChannelNo])*(Height/srcFmt.pixelHeight[ChannelNo])); }
      catch (std::bad_alloc&)  { return  Analyze_Result_MemoryError; }
      break;
    }
    ChannelNo++;
  }
  ChannelAlpha = AlphaChannel;
  for (ChannelNo=0 ; ChannelNo<ChannelCount ; ChannelNo++)
  {
    const int  PW      = srcFmt.pixelWidth[ChannelNo];
    const int  PH      = srcFmt.pixelHeight[ChannelNo];
    const int  ChWidth = Width  / PW;
    const int  ChHeight= Height / PH;
    // optimization: skip recomputing downsampled alpha if the previous channel had the same subsampling (and siting)
    if (!(ChannelNo>0 && (PW==srcFmt.pixelWidth[ChannelNo-1]) && (PH==srcFmt.pixelHeight[ChannelNo-1]) && ChromaLoc!=ChromaLoc_DV))
    {
      if (PW*PH==1)
      {
        // original, not subsampled alpha channel can be used as-is
        ChannelAlpha = AlphaChannel;
      }
      else
      {
        ChannelAlpha = ChannelAlphaVector.data();
        DownsampleAlpha(ChannelAlpha, AlphaChannel, Width, Height, PW, PH, ChromaLoc, ChannelNo);
      }
    }
    Sum1Ptr = Sum1[ChannelNo].data();
    DestPtr = destPlanes[ChannelNo];
    for (y=0 ; y<ChHeight ; y++)
    {
      for (x=0 ; x<ChWidth ; x++)
      {
        const int  ChIdx = ChWidth*y+x;
        PixelValue = ChannelAlpha[ChIdx];
        PixelValue = (PixelValue>0.0)?((static_cast<double>(Sum1Ptr[ChIdx])/static_cast<double>(FrameCount))-(1.0-PixelValue)*ChannelNMAverage[ChannelNo])/PixelValue:(static_cast<double>(Sum1Ptr[ChIdx])/static_cast<double>(FrameCount));
        int  iPixelValue;
        // NaN/Inf check
        if (!std::isfinite(PixelValue)) {
          iPixelValue = 0x80;
        }
        else {
          // safe to round and clamp now
          iPixelValue = static_cast<int>(PixelValue + 0.5);
          iPixelValue += 0x80; // chroma center
          iPixelValue = std::min(std::max(iPixelValue, 0x00), 0xFF);
        }
        DestPtr[destPitch[ChannelNo]*y+x*srcFmt.sampleStep[ChannelNo]] = static_cast<uint8_t>(iPixelValue);
      }
    }
  }
  if (destHasAlphaRow)
  {
    if (srcFmt.colorFamily != ColorFamily::RGB)  /* YUV or Gray: clear chroma of the alpha-row area */
    {
      for (ChannelNo=1 ; ChannelNo<ChannelCount ; ChannelNo++)
      {
        DestPtr = destPlanes[ChannelNo]+(Height/srcFmt.pixelHeight[ChannelNo])*destPitch[ChannelNo];
        for (y=0 ; y<Height/srcFmt.pixelHeight[ChannelNo] ; y++)
        {
          for (x=0 ; x<Width/srcFmt.pixelWidth[ChannelNo] ; x++)
          {
            DestPtr[destPitch[ChannelNo]*y+x*srcFmt.sampleStep[ChannelNo]] = 0x80;  /* No color I hope */
          }
        }
      }
    }
    if (srcFmt.colorFamily == ColorFamily::RGB && !PackedRGB)  /* planar RGB: R=G=B=alpha, A=0 in the alpha-row area */
    {
      for (ChannelNo=1 ; ChannelNo<srcFmt.channelCount ; ChannelNo++)
      {
        DestPtr = destPlanes[ChannelNo]+Height*destPitch[ChannelNo];
        for (y=0 ; y<Height ; y++)
          for (x=0 ; x<Width ; x++)
            DestPtr[destPitch[ChannelNo]*y+x] = (ChannelNo<3) ? AlphaData[y*AlphaPitch+x] : 0;
      }
    }
    if (PackedRGB)
    {
      for (y=0 ; y<Height ; y++)
      {
        for (x=0 ; x<Width ; x++)
        {
          AlphaData[y*AlphaPitch+x*AlphaSampleStep+2] = AlphaData[y*AlphaPitch+x*AlphaSampleStep+1] = AlphaData[y*AlphaPitch+x*AlphaSampleStep];
        }
      }
      if (srcFmt.colorFamily == ColorFamily::RGB && srcFmt.sampleStep[0] == 4)  /* packed RGB32 */
      {
        for (y=0 ; y<Height ; y++)
        {
          for (x=0 ; x<Width ; x++)
          {
            AlphaData[y*AlphaPitch+x*AlphaSampleStep+3] = 0;
          }
        }
      }
    }
  }
  return  static_cast<int32_t>(FrameCount);

  #undef  KahanAdditionMakro
}



