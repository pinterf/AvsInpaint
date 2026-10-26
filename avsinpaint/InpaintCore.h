#pragma once
/* InpaintCore.h — shared algorithm types, constants, and declarations.
   Included by both AVSInpaint.cpp (AviSynth wrapper) and VSSInpaint.cpp (VapourSynth 4 wrapper).
   Contains no API-specific types except AVS_Clip, which is forward-declared when avisynth/avisynth_c.h
   has not been included.                                                                          */

#include  <cstdint>
#include  <array>
#include  <functional>
#include  <vector>


/*****  Compile-time algorithm configuration  *****/

#define  BlurStandardDeviation  1  /* 1:blur-intensity=standard-deviation of Gauss-Kernel  0:blur-intensity=scale of autonomous heat equation u_t=tr(u_xx) */
#define  MaskThreshold  127  /* When the position of a logo (or something else) is given by a Y8-mask, the logo is supposed to be where  Sign(MT)*Mask>MT  holds */

#if  (MaskThreshold>0)
#define  MaskThresholdSign  (+1)
#else
#define  MaskThresholdSign  (-1)
#endif

constexpr double  InpaintMinRadiusXY  = 1.001; /* Will consider at least the 5-point-stencil */
constexpr int     InpaintMinPreBlurXYKernelSize  = 0;    /* It should hold  InpaintMinPreBlurXYKernelSize+InpaintMinPostBlurXYKernelSizeShould>=2  */
constexpr int     InpaintMinPostBlurXYKernelSize  = 2;
constexpr double  InpaintMinPreBlurXYStandardDeviation  = 0.0;
constexpr double  InpaintMinPostBlurXYStandardDeviation  = 0.1;

#define  InpaintFullMaskError  1  /* What to do if the whole frame should be inpainted?  0: Leave it unchanged,  1: Throw error on static logo,  2: Throw error even on dynamic logo */
#define  InpaintFailError  1  /* What if inpainting a certain pixel failed? 0: Leave it unchanged,  1: Throw error */
#define  InpaintChromaFailError  0  /* What if inpainting a certain subsampled chroma pixel failed? 0: Leave it unchanged (even if InpaintFailError==1),  1: Throw error */

constexpr int  MaxChannelCount  = 4;  /* Maximal number of color channels for all color spaces */

constexpr int  Analyze_Result_OK            =  0;
constexpr int  Analyze_Result_GeneralError  = -1;
constexpr int  Analyze_Result_SrcClipError  = -2;
constexpr int  Analyze_Result_DestFrameError = -3;
constexpr int  Analyze_Result_MemoryError   = -4;


/*****  Enumerations  *****/

enum class InpaintMaskResult : int
{
  NoMask       =  1,
  OK           =  0,
  GeneralError = -1,
  FrameError   = -2,
  MemoryError  = -3,
  FullMask     = -4
};

/* Color family — API-agnostic equivalent of AVS_CS_YUV / AVS_CS_RGB bits, and VS4's VSColorFamily */
enum class ColorFamily { Gray, YUV, RGB };


/* Chroma sample siting of subsampled YUV: the values of the _ChromaLocation frame property
   (ITU-T H.273, same in AviSynth+ and VapourSynth), plus the AviSynth "DV" special case
   (V co-sited top-left, U bottom-left). */
enum ChromaLocation
{
  ChromaLoc_Auto = -1,  /* not given: take _ChromaLocation of the source, or ChromaLoc_Default */
  ChromaLoc_Left = 0, ChromaLoc_Center = 1, ChromaLoc_TopLeft = 2, ChromaLoc_Top = 3,
  ChromaLoc_BottomLeft = 4, ChromaLoc_Bottom = 5,
  ChromaLoc_DV = 6,
  ChromaLoc_Invalid = -2
};
const int  ChromaLoc_Default = ChromaLoc_Left;  /* MPEG-2, the default of 4:2:0 and 4:2:2 (same as AviSynth+ and VapourSynth resizers) */


/*****  Structs  *****/

/* Physical layout of a video frame: all information derivable from pixel format alone, independent of any API */
struct FrameLayout
{
  int          channelCount;
  ColorFamily  colorFamily;
  int  sampleStep[MaxChannelCount];   /* bytes between adjacent samples within a row (1 for planar, 2+ for packed) */
  int  pixelWidth[MaxChannelCount];   /* horizontal subsampling factor (1 = full-res, 2 = half-width chroma, ...) */
  int  pixelHeight[MaxChannelCount];  /* vertical subsampling factor */
  FrameLayout() : channelCount(0), colorFamily(ColorFamily::Gray), sampleStep{}, pixelWidth{}, pixelHeight{} {}
};


/* Mask data computed from one mask frame */
struct InpaintMaskDataStruct
{
  int  OffsetX, OffsetY, Width, Height, MaskPixelCount;
  std::vector<unsigned int>  MaskPixelOrder;
  std::vector<uint8_t>       chiLuma, chiChroma;
  std::vector<float>         GchiLuma, GchiChroma;
  InpaintMaskDataStruct() : OffsetX(0), OffsetY(0), Width(0), Height(0), MaskPixelCount(0) {}
};


/* AVS_Clip is only stored as a pointer in InpaintDataStruct (MaskClip).
   A forward declaration suffices when avisynth/avisynth_c.h has not been included (e.g. in VSSInpaint.cpp).
   The VS4 wrapper always sets MaskClip = nullptr and manages its mask node separately.           */
#ifndef __AVISYNTH_C__
typedef struct AVS_Clip AVS_Clip;
#endif


/* All data needed by InpaintCore and InpaintPrepareMask — pixel-format params + pre-computed kernels */
struct InpaintDataStruct
{
  AVS_Clip * MaskClip;  /* nullptr means static mask or VS4 caller (which uses its own node handle) */
  int  MaskFrameCount, MaxSteps;
  /* Values given by the user (a bit processed) */
  double  PixelWidth, PixelHeight;  /* defined by pixel aspect ratio, product of both should be 1 !! */
  double  LumaWeight, ChromaWeight;  /* weights for structure tensor */
  double  LumaRadiusX, LumaRadiusY, ChromaRadiusX, ChromaRadiusY, Sharpness;  /* Different X/Y-radi will give an ellipse (due to PAR) */
  bool  ChromaTensor;
  /* Color space dependent values */
  int  PixelType, Width, Height, ChromaPixelWidth, ChromaPixelHeight, ChromaChannelCount, MaskPixelType, MaskChannelNo;
  FrameLayout  srcLayout, maskLayout;
  bool  FlipMask;
  bool  isYUV;             /* srcLayout.colorFamily == ColorFamily::YUV */
  bool  isChromaSubsampled; /* ChromaPixelWidth > 1 && ChromaPixelHeight > 1 */
  /* Blurring stuff */
  double * LumaPreBlurXKernel, * LumaPreBlurYKernel, * ChromaPreBlurXKernel, * ChromaPreBlurYKernel;  /* (Preblurred data will be buffered later) */
  double * Luma4LumaPostBlurXKernel, * Luma4LumaPostBlurYKernel;  /* Postblurring will also convert between different samplings */
  double * Chroma4ChromaPostBlurXKernel, * Chroma4ChromaPostBlurYKernel;
  double * Luma4ChromaPostBlurXKernel, * Luma4ChromaPostBlurYKernel;  /* If ChromaPixelW/H is even, this points to (the weight of) the pixel *before* the center line */
  std::vector<double*> Chroma4LumaPostBlurXKernels, Chroma4LumaPostBlurYKernels;  /* center pointers into KernelVectors below */
  std::vector<double> LumaPreBlurXKernelVector, LumaPreBlurYKernelVector, ChromaPreBlurXKernelVector, ChromaPreBlurYKernelVector;
  std::vector<double> Luma4LumaPostBlurXKernelVector, Luma4LumaPostBlurYKernelVector;
  std::vector<double> Chroma4ChromaPostBlurXKernelVector, Chroma4ChromaPostBlurYKernelVector;
  std::vector<double> Luma4ChromaPostBlurXKernelVector, Luma4ChromaPostBlurYKernelVector;
  std::vector<std::vector<double>> Chroma4LumaPostBlurXKernelVectors, Chroma4LumaPostBlurYKernelVectors;  /* Blurring Chroma to transport luma -> Different kernels for each luma pixel (inside one chroma pixel) */
  int  LumaPreBlurXKernelSize, LumaPreBlurYKernelSize, ChromaPreBlurXKernelSize, ChromaPreBlurYKernelSize;
  int  Luma4LumaPostBlurXKernelSize, Luma4LumaPostBlurYKernelSize;
  int  Chroma4ChromaPostBlurXKernelSize, Chroma4ChromaPostBlurYKernelSize;
  int  Luma4ChromaPostBlurXKernelSize, Luma4ChromaPostBlurYKernelSize;
  int  Chroma4LumaPostBlurXKernelSize, Chroma4LumaPostBlurYKernelSize;
  /* For static mask */
  InpaintMaskDataStruct * StaticMaskData;  /* nullptr means dynamic mask */

  InpaintDataStruct() :
    MaskClip(nullptr), MaskFrameCount(0), MaxSteps(0),
    PixelWidth(0.0), PixelHeight(0.0),
    LumaWeight(0.0), ChromaWeight(0.0),
    LumaRadiusX(0.0), LumaRadiusY(0.0), ChromaRadiusX(0.0), ChromaRadiusY(0.0), Sharpness(0.0),
    ChromaTensor(false),
    PixelType(0), Width(0), Height(0), ChromaPixelWidth(0), ChromaPixelHeight(0),
    ChromaChannelCount(0), MaskPixelType(0), MaskChannelNo(0),
    FlipMask(false),
    isYUV(false), isChromaSubsampled(false),
    LumaPreBlurXKernel(nullptr), LumaPreBlurYKernel(nullptr),
    ChromaPreBlurXKernel(nullptr), ChromaPreBlurYKernel(nullptr),
    Luma4LumaPostBlurXKernel(nullptr), Luma4LumaPostBlurYKernel(nullptr),
    Chroma4ChromaPostBlurXKernel(nullptr), Chroma4ChromaPostBlurYKernel(nullptr),
    Luma4ChromaPostBlurXKernel(nullptr), Luma4ChromaPostBlurYKernel(nullptr),
    LumaPreBlurXKernelSize(0), LumaPreBlurYKernelSize(0),
    ChromaPreBlurXKernelSize(0), ChromaPreBlurYKernelSize(0),
    Luma4LumaPostBlurXKernelSize(0), Luma4LumaPostBlurYKernelSize(0),
    Chroma4ChromaPostBlurXKernelSize(0), Chroma4ChromaPostBlurYKernelSize(0),
    Luma4ChromaPostBlurXKernelSize(0), Luma4ChromaPostBlurYKernelSize(0),
    Chroma4LumaPostBlurXKernelSize(0), Chroma4LumaPostBlurYKernelSize(0),
    StaticMaskData(nullptr)
  {}
};


/*****  Pure function declarations (implementations in AVSInpaint.cpp)  *****/

/* Format helper — AviSynth pixel_type based, only available in AviSynth builds */
#ifdef ENABLE_AVISYNTH
FrameLayout  FrameLayoutFromPixelType(int  PixelType);
#endif

/* Kernel builder (used during filter creation by both AVS and VS4 wrappers) */
double *  CreateGaussKernel(double  Kernel[],  int  Size,  double  Deviation,  double  Center);

/* Deblend tools — no API types; all frame data via plain pointers */

/* Downsampled alpha of the current frame, shared by the chroma channels with the same siting
   (U and V, except for DV). Create one per frame and pass it to every DeblendDynamicSubsampled call. */
struct  SubsampledAlphaCache
{
  std::vector<float>  Alpha;
  int  Key = -1;
};

void  DeblendDynamic(uint8_t  Data[],  const  uint8_t  LogoData[],  const  uint8_t  AlphaData[],  int  Pitch,  int  Stride,  int  LogoPitch,  int  LogoStride,  int  AlphaPitch,  int  AlphaSampleStep,  int  Width,  int  Height);
void  DeblendDynamicSubsampled(uint8_t  Data[],  const  uint8_t  LogoData[],  const  uint8_t  AlphaData[],  int  Pitch,  int  Stride,  int  LogoPitch,  int  LogoStride,  int  AlphaPitch,  int  AlphaSampleStep,  int  Width,  int  Height,  int  PixelWidth,  int  PixelHeight,  int  ChromaLoc,  int  ChannelNo,  SubsampledAlphaCache &  Cache);

/* Chroma placement name (case-insensitive) -> ChromaLocation. nullptr or "auto" gives ChromaLoc_Auto,
   unknown names ChromaLoc_Invalid. Names as in AviSynth+ ChromaInPlacement and VapourSynth chromaloc_s. */
int  ParseChromaPlacement(const char *  Name);
/* Resolves ChromaLoc_Auto: FrameProp is the _ChromaLocation frame property (-1 if absent) */
int  ResolveChromaLocation(int  ChromaLoc,  int  FrameProp);
/* Downsamples a full-resolution alpha (or mask) plane to the resolution of channel ChannelNo, which is
   subsampled by PixelWidth x PixelHeight. Each chroma sample takes the luma samples around its sited
   position: a box over the block where it is centered, a triangle ([1,2,1]/4 for factor 2) where it is
   co-sited with a luma sample. ChromaLoc_Center is the plain block average. */
void  DownsampleAlpha(float  Dest[],  const float  Src[],  int  Width,  int  Height,  int  PixelWidth,  int  PixelHeight,  int  ChromaLoc,  int  ChannelNo);
/* LogoData[i] = alpha * logo_pixel, premultiplied, uint16_t (range [0..255*255=65025], fits exactly).
   AlphaData[i] = 255 - alpha (the effective denominator in the deblend formula).
   Hot-loop formula: source = (255*blended - Logo) / (255-alpha), computed in double. */
void  DeblendStatic(uint8_t  Data[],  const  uint16_t  LogoData[],  const  uint8_t  AlphaData[],  int  Pitch,  int  Stride,  int  Width,  int  Height);

/* alphaPlane==nullptr means alpha is embedded in the last channel of logoPlanes.
   When alphaPlane is non-null, the caller must already have applied any packed-format
   byte offset (+3 for RGB32 A channel) and any row-flip (negative pitch for upside-down clips). */
int  DeblendStaticPrepare(uint16_t *  LogoDataChannels[],  uint8_t *  AlphaDataChannels[],
                           const FrameLayout &  logoFmt,  int  Width,  int  Height,
                           const uint8_t *  logoPlanes[],  int  logoPitch[],
                           const uint8_t *  alphaPlane,  int  alphaPitch,  int  alphaSampleStep,
                           int  ChromaLoc);

/* Core algorithm — no API types; all frame data via plain pointers */
InpaintMaskResult  InpaintPrepareMask(InpaintMaskDataStruct **  MaskDataPtr,  const FrameLayout &  maskLayout,  const uint8_t *  planes[],  int  pitch[],  const InpaintDataStruct *  InpaintData);

const char *  InpaintCore(const InpaintDataStruct *  InpaintData,  InpaintMaskDataStruct *  MaskData,  uint8_t *  FrameData[],  const int  FramePitch[]);

const char *  DistanceFunctionCore(uint8_t *  data,  int  pitch,  int  SampleStep,  int  width,  int  height,
                                   double  scale,  int  scaleSign,  double  pixelWidth,  double  pixelHeight);

int32_t  Analyze(const FrameLayout &  srcFmt,  int  Width,  int  Height,  unsigned int  FrameCount,
                 std::function<bool(unsigned int, const uint8_t*[], int[])>  fetchSrcFrame,
                 std::function<void(unsigned int)>                           releaseSrcFrame,
                 uint8_t *  destPlanes[],  int  destPitch[],  bool  destHasAlphaRow,
                 unsigned char  Mask[],  int32_t  MaskSize,  int  ComputeAlpha,
                 double  DeviationWeight,  double  SubsamplingWeight,  int  ChromaLoc);
