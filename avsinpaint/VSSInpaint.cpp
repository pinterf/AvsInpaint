// VSSInpaint.cpp  —  VapourSynth V4 wrapper for AvsInPaint
// Implements InpaintLogo and AnalyzeLogo using the pure-pointer core from AVSInpaint.cpp.
// DeblendLogo and DistanceFunction are not implemented for VapourSynth (they rely on
// AVS-specific frame memory management that has not yet been decoupled).

#include  "InpaintCore.h"
#include  "vs/VapourSynth4.h"

#include  <cstdlib>
#include  <cmath>
#include  <climits>
#include  <cstdint>
#include  <cstring>
#include  <algorithm>
#include  <new>
#include  <vector>


/*****  VS4-specific layout helper  *****/

/* Build a FrameLayout from a VSVideoFormat.
   VS4 has no packed formats; sampleStep is always 1.
   RGB planes are in R,G,B order (plane 0 = R), which differs from the AVS convention
   (plane 0 = B).  InpaintCore treats all RGB channels symmetrically so the order does
   not affect correctness, only which channel drives the structure tensor.               */
static FrameLayout  FrameLayoutFromVSFormat(const VSVideoFormat &  fmt)
{
  FrameLayout  L;
  switch (fmt.colorFamily)
  {
    case cfGray:  L.colorFamily = ColorFamily::Gray;  break;
    case cfYUV:   L.colorFamily = ColorFamily::YUV;   break;
    case cfRGB:   L.colorFamily = ColorFamily::RGB;   break;
    default:      return L;  /* channelCount stays 0 — unsupported */
  }
  L.channelCount = fmt.numPlanes;
  for (int i = 0 ; i < L.channelCount ; i++)  L.sampleStep[i] = 1;  /* always planar */
  /* luma / first plane: no subsampling */
  L.pixelWidth[0]  = 1;
  L.pixelHeight[0] = 1;
  /* chroma planes: log2 subsampling factors from VSVideoFormat */
  for (int i = 1 ; i < L.channelCount ; i++)
  {
    L.pixelWidth[i]  = 1 << fmt.subSamplingW;
    L.pixelHeight[i] = 1 << fmt.subSamplingH;
  }
  return L;
}


/*****  Shared InpaintDataStruct setup helper  *****/

/* Populates *InpaintData from user parameters.
   Returns nullptr on success, or a static error string on failure.
   Precondition: *InpaintData has been zero-constructed (InpaintDataStruct default ctor).  */
static const char *  SetupInpaintData(InpaintDataStruct *  InpaintData,
                                      const FrameLayout &  srcFmt,
                                      int  Width,  int  Height,
                                      bool  hasMaskClip,   /* false means alpha embedded in src (not supported for VS4) */
                                      const FrameLayout &  maskFmt,
                                      int  maskChannelNo,  /* which mask plane carries the mask data */
                                      bool  flipMask,
                                      int   maskFrameCount,
                                      double  Radius,  double  Sharpness,
                                      double  PreBlur, double  PostBlur,
                                      double  ChromaWeight,
                                      double  PreBlurSize, double  PostBlurSize,
                                      bool  ChromaTensor,
                                      double  PAR,
                                      int  MaxSteps)
{
  int  k;

  /** Color space parameters **/
  InpaintData->srcLayout = srcFmt;
  InpaintData->ChromaChannelCount = srcFmt.channelCount - 1 - (hasMaskClip ? 0 : 1);
  InpaintData->Width  = Width;
  InpaintData->Height = Height;
  if (InpaintData->ChromaChannelCount)
  {
    InpaintData->ChromaPixelWidth  = srcFmt.pixelWidth[1];
    InpaintData->ChromaPixelHeight = srcFmt.pixelHeight[1];
  }
  else
  {
    InpaintData->ChromaPixelWidth  = 0;
    InpaintData->ChromaPixelHeight = 0;
  }
  InpaintData->isYUV             = (srcFmt.colorFamily == ColorFamily::YUV);
  InpaintData->isChromaSubsampled = (InpaintData->ChromaPixelWidth > 1) && (InpaintData->ChromaPixelHeight > 1);
  InpaintData->maskLayout     = maskFmt;
  InpaintData->MaskChannelNo  = maskChannelNo;
  InpaintData->FlipMask       = flipMask;
  InpaintData->MaskFrameCount = (maskFrameCount > 1) ? maskFrameCount : 0;
  InpaintData->MaxSteps       = (MaxSteps >= 0) ? MaxSteps : -1;

  /** User-specified values **/
  InpaintData->PixelWidth  = std::sqrt(PAR);
  InpaintData->PixelHeight = 1.0 / InpaintData->PixelWidth;
  if (InpaintData->ChromaChannelCount)
  {
    InpaintData->LumaWeight   = 1.0 - ChromaWeight;
    InpaintData->ChromaWeight = ChromaWeight / InpaintData->ChromaChannelCount;
    InpaintData->ChromaRadiusX = std::max<double>(Radius / InpaintData->PixelWidth  / InpaintData->ChromaPixelWidth,  InpaintMinRadiusXY);
    InpaintData->ChromaRadiusY = std::max<double>(Radius / InpaintData->PixelHeight / InpaintData->ChromaPixelHeight, InpaintMinRadiusXY);
  }
  else
  {
    InpaintData->LumaWeight    = 1.0;
    InpaintData->ChromaWeight  = 0.0;
    InpaintData->ChromaRadiusX = 0.0;
    InpaintData->ChromaRadiusY = 0.0;
  }
  InpaintData->LumaRadiusX = std::max<double>(Radius / InpaintData->PixelWidth,  InpaintMinRadiusXY);
  InpaintData->LumaRadiusY = std::max<double>(Radius / InpaintData->PixelHeight, InpaintMinRadiusXY);
  InpaintData->Sharpness    = Sharpness;
  InpaintData->ChromaTensor = ChromaTensor && InpaintData->ChromaChannelCount && (srcFmt.colorFamily == ColorFamily::YUV);

  /** Set blurring kernel sizes and allocate kernel vectors **/
  if (InpaintData->LumaWeight != 0.0)
  {
    InpaintData->LumaPreBlurXKernelSize  = static_cast<int>(std::max<double>(std::floor(PreBlurSize  / InpaintData->PixelWidth),  InpaintMinPreBlurXYKernelSize));
    InpaintData->LumaPreBlurYKernelSize  = static_cast<int>(std::max<double>(std::floor(PreBlurSize  / InpaintData->PixelHeight), InpaintMinPreBlurXYKernelSize));
    InpaintData->Luma4LumaPostBlurXKernelSize = static_cast<int>(std::max<double>(std::floor(PostBlurSize / InpaintData->PixelWidth  + 0.5), InpaintMinPostBlurXYKernelSize));
    InpaintData->Luma4LumaPostBlurYKernelSize = static_cast<int>(std::max<double>(std::floor(PostBlurSize / InpaintData->PixelHeight + 0.5), InpaintMinPostBlurXYKernelSize));
    try
    {
      InpaintData->LumaPreBlurXKernelVector.resize(2 * InpaintData->LumaPreBlurXKernelSize + 1);
      InpaintData->LumaPreBlurYKernelVector.resize(2 * InpaintData->LumaPreBlurYKernelSize + 1);
      InpaintData->Luma4LumaPostBlurXKernelVector.resize(2 * InpaintData->Luma4LumaPostBlurXKernelSize + 1);
      InpaintData->Luma4LumaPostBlurYKernelVector.resize(2 * InpaintData->Luma4LumaPostBlurYKernelSize + 1);
    }
    catch (std::bad_alloc &)  { return  "InpaintLogo: Could not allocate memory"; }
  }
  if (InpaintData->ChromaChannelCount || (InpaintData->ChromaWeight != 0.0))
  {
    InpaintData->ChromaPreBlurXKernelSize = static_cast<int>(std::max<double>(std::floor(PreBlurSize  / InpaintData->PixelWidth  / InpaintData->ChromaPixelWidth),  InpaintMinPreBlurXYKernelSize));
    InpaintData->ChromaPreBlurYKernelSize = static_cast<int>(std::max<double>(std::floor(PreBlurSize  / InpaintData->PixelHeight / InpaintData->ChromaPixelHeight), InpaintMinPreBlurXYKernelSize));
    try
    {
      InpaintData->ChromaPreBlurXKernelVector.resize(2 * InpaintData->ChromaPreBlurXKernelSize + 1);
      InpaintData->ChromaPreBlurYKernelVector.resize(2 * InpaintData->ChromaPreBlurYKernelSize + 1);
    }
    catch (std::bad_alloc &)  { return  "InpaintLogo: Could not allocate memory"; }
  }
  if (InpaintData->ChromaTensor || (InpaintData->ChromaWeight != 0.0))
  {
    InpaintData->Chroma4ChromaPostBlurXKernelSize = static_cast<int>(std::max<double>(std::floor(PostBlurSize / InpaintData->PixelWidth  / InpaintData->ChromaPixelWidth  + 0.5), InpaintMinPostBlurXYKernelSize));
    InpaintData->Chroma4ChromaPostBlurYKernelSize = static_cast<int>(std::max<double>(std::floor(PostBlurSize / InpaintData->PixelHeight / InpaintData->ChromaPixelHeight + 0.5), InpaintMinPostBlurXYKernelSize));
    try
    {
      InpaintData->Chroma4ChromaPostBlurXKernelVector.resize(2 * InpaintData->Chroma4ChromaPostBlurXKernelSize + 1);
      InpaintData->Chroma4ChromaPostBlurYKernelVector.resize(2 * InpaintData->Chroma4ChromaPostBlurYKernelSize + 1);
    }
    catch (std::bad_alloc &)  { return  "InpaintLogo: Could not allocate memory"; }
  }
  if (InpaintData->ChromaTensor && (InpaintData->LumaWeight != 0.0))
  {
    InpaintData->Luma4ChromaPostBlurXKernelSize = static_cast<int>(std::max<double>(std::floor(PostBlurSize / InpaintData->PixelWidth  + (InpaintData->ChromaPixelWidth  % 2 ? 0.0 : 0.5)), InpaintMinPostBlurXYKernelSize));
    InpaintData->Luma4ChromaPostBlurYKernelSize = static_cast<int>(std::max<double>(std::floor(PostBlurSize / InpaintData->PixelHeight + (InpaintData->ChromaPixelHeight % 2 ? 0.0 : 0.5)), InpaintMinPostBlurXYKernelSize));
    try
    {
      InpaintData->Luma4ChromaPostBlurXKernelVector.resize(2 * InpaintData->Luma4ChromaPostBlurXKernelSize + 1);
      InpaintData->Luma4ChromaPostBlurYKernelVector.resize(2 * InpaintData->Luma4ChromaPostBlurYKernelSize + 1);
    }
    catch (std::bad_alloc &)  { return  "InpaintLogo: Could not allocate memory"; }
  }
  if (InpaintData->ChromaWeight != 0.0)
  {
    InpaintData->Chroma4LumaPostBlurXKernelSize = static_cast<int>(std::max<double>(std::floor(PostBlurSize / InpaintData->PixelWidth  / InpaintData->ChromaPixelWidth  + 0.5 - (InpaintData->ChromaPixelWidth  % 2 ? 0.0 : 0.5 / InpaintData->ChromaPixelWidth)),  InpaintMinPostBlurXYKernelSize));
    InpaintData->Chroma4LumaPostBlurYKernelSize = static_cast<int>(std::max<double>(std::floor(PostBlurSize / InpaintData->PixelHeight / InpaintData->ChromaPixelHeight + 0.5 - (InpaintData->ChromaPixelHeight % 2 ? 0.0 : 0.5 / InpaintData->ChromaPixelHeight)), InpaintMinPostBlurXYKernelSize));
    try
    {
      InpaintData->Chroma4LumaPostBlurXKernelVectors.resize(InpaintData->ChromaPixelWidth);
      InpaintData->Chroma4LumaPostBlurYKernelVectors.resize(InpaintData->ChromaPixelHeight);
      InpaintData->Chroma4LumaPostBlurXKernels.resize(InpaintData->ChromaPixelWidth,  nullptr);
      InpaintData->Chroma4LumaPostBlurYKernels.resize(InpaintData->ChromaPixelHeight, nullptr);
      for (k = 0 ; k < InpaintData->ChromaPixelWidth  ; k++)  InpaintData->Chroma4LumaPostBlurXKernelVectors[k].resize(2 * InpaintData->Chroma4LumaPostBlurXKernelSize + 1);
      for (k = 0 ; k < InpaintData->ChromaPixelHeight ; k++)  InpaintData->Chroma4LumaPostBlurYKernelVectors[k].resize(2 * InpaintData->Chroma4LumaPostBlurYKernelSize + 1);
    }
    catch (std::bad_alloc &)  { return  "InpaintLogo: Could not allocate memory"; }
  }
  if (InpaintData->ChromaPixelWidth * InpaintData->ChromaPixelHeight > 255)
    return  "InpaintLogo internal error: Chroma subsampling too strong";

  /** Compute blur kernels **/
  if (!InpaintData->LumaPreBlurXKernelVector.empty())  InpaintData->LumaPreBlurXKernel = CreateGaussKernel(InpaintData->LumaPreBlurXKernelVector.data(), InpaintData->LumaPreBlurXKernelSize, std::max<double>(PreBlur / InpaintData->PixelWidth,  InpaintMinPreBlurXYStandardDeviation), 0.0);
  if (!InpaintData->LumaPreBlurYKernelVector.empty())  InpaintData->LumaPreBlurYKernel = CreateGaussKernel(InpaintData->LumaPreBlurYKernelVector.data(), InpaintData->LumaPreBlurYKernelSize, std::max<double>(PreBlur / InpaintData->PixelHeight, InpaintMinPreBlurXYStandardDeviation), 0.0);
  if (!InpaintData->Luma4LumaPostBlurXKernelVector.empty())  InpaintData->Luma4LumaPostBlurXKernel = CreateGaussKernel(InpaintData->Luma4LumaPostBlurXKernelVector.data(), InpaintData->Luma4LumaPostBlurXKernelSize, std::max<double>(PostBlur / InpaintData->PixelWidth,  InpaintMinPostBlurXYStandardDeviation), -0.5);
  if (!InpaintData->Luma4LumaPostBlurYKernelVector.empty())  InpaintData->Luma4LumaPostBlurYKernel = CreateGaussKernel(InpaintData->Luma4LumaPostBlurYKernelVector.data(), InpaintData->Luma4LumaPostBlurYKernelSize, std::max<double>(PostBlur / InpaintData->PixelHeight, InpaintMinPostBlurXYStandardDeviation), -0.5);
  if (!InpaintData->ChromaPreBlurXKernelVector.empty())  InpaintData->ChromaPreBlurXKernel = CreateGaussKernel(InpaintData->ChromaPreBlurXKernelVector.data(), InpaintData->ChromaPreBlurXKernelSize, std::max<double>(PreBlur / InpaintData->PixelWidth  / InpaintData->ChromaPixelWidth,  InpaintMinPreBlurXYStandardDeviation), 0.0);
  if (!InpaintData->ChromaPreBlurYKernelVector.empty())  InpaintData->ChromaPreBlurYKernel = CreateGaussKernel(InpaintData->ChromaPreBlurYKernelVector.data(), InpaintData->ChromaPreBlurYKernelSize, std::max<double>(PreBlur / InpaintData->PixelHeight / InpaintData->ChromaPixelHeight, InpaintMinPreBlurXYStandardDeviation), 0.0);
  if (!InpaintData->Chroma4ChromaPostBlurXKernelVector.empty())  InpaintData->Chroma4ChromaPostBlurXKernel = CreateGaussKernel(InpaintData->Chroma4ChromaPostBlurXKernelVector.data(), InpaintData->Chroma4ChromaPostBlurXKernelSize, std::max<double>(PostBlur / InpaintData->PixelWidth  / InpaintData->ChromaPixelWidth,  InpaintMinPostBlurXYStandardDeviation), -0.5);
  if (!InpaintData->Chroma4ChromaPostBlurYKernelVector.empty())  InpaintData->Chroma4ChromaPostBlurYKernel = CreateGaussKernel(InpaintData->Chroma4ChromaPostBlurYKernelVector.data(), InpaintData->Chroma4ChromaPostBlurYKernelSize, std::max<double>(PostBlur / InpaintData->PixelHeight / InpaintData->ChromaPixelHeight, InpaintMinPostBlurXYStandardDeviation), -0.5);
  if (!InpaintData->Luma4ChromaPostBlurXKernelVector.empty())  InpaintData->Luma4ChromaPostBlurXKernel = CreateGaussKernel(InpaintData->Luma4ChromaPostBlurXKernelVector.data(), InpaintData->Luma4ChromaPostBlurXKernelSize, std::max<double>(PostBlur / InpaintData->PixelWidth,  InpaintMinPostBlurXYStandardDeviation), (InpaintData->ChromaPixelWidth  % 2) ? -0.5 : 0.0);
  if (!InpaintData->Luma4ChromaPostBlurYKernelVector.empty())  InpaintData->Luma4ChromaPostBlurYKernel = CreateGaussKernel(InpaintData->Luma4ChromaPostBlurYKernelVector.data(), InpaintData->Luma4ChromaPostBlurYKernelSize, std::max<double>(PostBlur / InpaintData->PixelHeight, InpaintMinPostBlurXYStandardDeviation), (InpaintData->ChromaPixelHeight % 2) ? -0.5 : 0.0);
  if (!InpaintData->Chroma4LumaPostBlurXKernelVectors.empty())
    for (k = 0 ; k < InpaintData->ChromaPixelWidth ; k++)
      InpaintData->Chroma4LumaPostBlurXKernels[k] = CreateGaussKernel(InpaintData->Chroma4LumaPostBlurXKernelVectors[k].data(), InpaintData->Chroma4LumaPostBlurXKernelSize, std::max<double>(PostBlur / InpaintData->PixelWidth / InpaintData->ChromaPixelWidth, InpaintMinPostBlurXYStandardDeviation), (2.0*k+1.0)/2.0/InpaintData->ChromaPixelWidth - (2*k+1 < InpaintData->ChromaPixelWidth ? 0.0 : 1.0));
  if (!InpaintData->Chroma4LumaPostBlurYKernelVectors.empty())
    for (k = 0 ; k < InpaintData->ChromaPixelHeight ; k++)
      InpaintData->Chroma4LumaPostBlurYKernels[k] = CreateGaussKernel(InpaintData->Chroma4LumaPostBlurYKernelVectors[k].data(), InpaintData->Chroma4LumaPostBlurYKernelSize, std::max<double>(PostBlur / InpaintData->PixelHeight / InpaintData->ChromaPixelHeight, InpaintMinPostBlurXYStandardDeviation), (2.0*k+1.0)/2.0/InpaintData->ChromaPixelHeight - (2*k+1 < InpaintData->ChromaPixelHeight ? 0.0 : 1.0));

  return  nullptr;  /* success */
}


/*****  VS4 plane pointer helpers  *****/

static bool  VsGetFramePlanesRO(const VSFrame *  Frame,  const FrameLayout &  L,  const uint8_t *  Planes[],  int  Pitch[],  const VSAPI *  vsapi)
{
  if (!Frame || !L.channelCount)  return  false;
  for (int i = 0 ; i < L.channelCount ; i++)
  {
    Planes[i] = vsapi->getReadPtr(Frame, i);
    Pitch[i]  = static_cast<int>(vsapi->getStride(Frame, i));
    if (!Planes[i])  return  false;
  }
  return  true;
}

static bool  VsGetFramePlanes(VSFrame *  Frame,  const FrameLayout &  L,  uint8_t *  Planes[],  int  Pitch[],  const VSAPI *  vsapi)
{
  if (!Frame || !L.channelCount)  return  false;
  for (int i = 0 ; i < L.channelCount ; i++)
  {
    Planes[i] = vsapi->getWritePtr(Frame, i);
    Pitch[i]  = static_cast<int>(vsapi->getStride(Frame, i));
    if (!Planes[i])  return  false;
  }
  return  true;
}


/******************************************************************
                      InpaintLogo (VS4)
 ******************************************************************/

struct VSInpaintData
{
  VSNode *          srcNode;
  VSNode *          maskNode;      /* non-null only if mask is dynamic (maskFrameCount > 1) */
  int               maskFrameCount;
  InpaintDataStruct inpaint;
};


static const VSFrame *  VS_CC  vsInpaintGetFrame(int  n,  int  activationReason,  void *  instanceData,  void **  frameData,  VSFrameContext *  frameCtx,  VSCore *  core,  const VSAPI *  vsapi)
{
  VSInpaintData * d = static_cast<VSInpaintData*>(instanceData);

  if (activationReason == arInitial)
  {
    vsapi->requestFrameFilter(n, d->srcNode, frameCtx);
    if (d->maskNode)
      vsapi->requestFrameFilter(n < d->maskFrameCount ? n : d->maskFrameCount - 1, d->maskNode, frameCtx);
    return  nullptr;
  }

  if (activationReason != arAllFramesReady)  return  nullptr;

  const VSFrame * srcFrame = vsapi->getFrameFilter(n, d->srcNode, frameCtx);
  InpaintMaskDataStruct * MaskData = d->inpaint.StaticMaskData;

  /* Dynamic mask: prepare per-frame mask data */
  if (!MaskData)
  {
    const VSFrame * maskFrame = vsapi->getFrameFilter(n < d->maskFrameCount ? n : d->maskFrameCount - 1, d->maskNode, frameCtx);
    const uint8_t * maskPlanes[MaxChannelCount] = {};
    int             maskPitch[MaxChannelCount]  = {};
    if (!VsGetFramePlanesRO(maskFrame, d->inpaint.maskLayout, maskPlanes, maskPitch, vsapi))
    {
      vsapi->freeFrame(maskFrame);
      vsapi->freeFrame(srcFrame);
      vsapi->setFilterError("InpaintLogo: Could not get mask frame planes", frameCtx);
      return  nullptr;
    }
    InpaintMaskResult res = InpaintPrepareMask(&MaskData, d->inpaint.maskLayout, maskPlanes, maskPitch, &d->inpaint);
    vsapi->freeFrame(maskFrame);
    if (res == InpaintMaskResult::FrameError || res == InpaintMaskResult::MemoryError || res == InpaintMaskResult::GeneralError)
    {
      vsapi->freeFrame(srcFrame);
      vsapi->setFilterError(res == InpaintMaskResult::MemoryError ? "InpaintLogo: Out of memory preparing mask" : "InpaintLogo: Mask preparation failed", frameCtx);
      return  nullptr;
    }
  }

  /* No mask data → nothing to do, return source frame unchanged */
  if (!MaskData)  return  srcFrame;

  /* Make a writable copy of the source frame */
  VSFrame * dstFrame = vsapi->copyFrame(srcFrame, core);
  vsapi->freeFrame(srcFrame);
  if (!dstFrame)
  {
    if (!d->inpaint.StaticMaskData)  { delete MaskData;  MaskData = nullptr; }
    vsapi->setFilterError("InpaintLogo: Could not copy frame", frameCtx);
    return  nullptr;
  }

  /* Get writable plane pointers */
  uint8_t * framePlanes[MaxChannelCount] = {};
  int        framePitch[MaxChannelCount]  = {};
  if (!VsGetFramePlanes(dstFrame, d->inpaint.srcLayout, framePlanes, framePitch, vsapi))
  {
    if (!d->inpaint.StaticMaskData)  { delete MaskData;  MaskData = nullptr; }
    vsapi->freeFrame(dstFrame);
    vsapi->setFilterError("InpaintLogo: Could not get frame planes", frameCtx);
    return  nullptr;
  }

  /* Run inpainting */
  const char * ErrorText = InpaintCore(&d->inpaint, MaskData, framePlanes, framePitch);
  if (!d->inpaint.StaticMaskData)  { delete MaskData;  MaskData = nullptr; }
  if (ErrorText)
  {
    vsapi->freeFrame(dstFrame);
    vsapi->setFilterError(ErrorText, frameCtx);
    return  nullptr;
  }
  return  dstFrame;
}


static void  VS_CC  vsInpaintFree(void *  instanceData,  VSCore *  /*core*/,  const VSAPI *  vsapi)
{
  VSInpaintData * d = static_cast<VSInpaintData*>(instanceData);
  if (d->inpaint.StaticMaskData)  { delete d->inpaint.StaticMaskData;  d->inpaint.StaticMaskData = nullptr; }
  if (d->maskNode)  vsapi->freeNode(d->maskNode);
  vsapi->freeNode(d->srcNode);
  delete d;
}


static void  VS_CC  vsInpaintCreate(const VSMap *  in,  VSMap *  out,  void *  /*userData*/,  VSCore *  core,  const VSAPI *  vsapi)
{
  int  err;
  const char * ErrorText = nullptr;

  /* --- Fetch clips --- */
  VSNode * srcNode  = vsapi->mapGetNode(in, "clip", 0, nullptr);
  VSNode * maskNode = vsapi->mapGetNode(in, "mask", 0, &err);  /* optional */
  if (err)  maskNode = nullptr;

  const VSVideoInfo * srcVI  = vsapi->getVideoInfo(srcNode);
  const VSVideoInfo * maskVI = maskNode ? vsapi->getVideoInfo(maskNode) : nullptr;

  /* --- Validate source format --- */
  if (!srcVI->format.colorFamily)
    ErrorText = "InpaintLogo: Source clip has variable format (not supported)";
  else if (srcVI->format.bitsPerSample != 8 || srcVI->format.sampleType != stInteger)
    ErrorText = "InpaintLogo: Only 8-bit integer formats are supported";
  else if (srcVI->format.colorFamily != cfGray && srcVI->format.colorFamily != cfYUV && srcVI->format.colorFamily != cfRGB)
    ErrorText = "InpaintLogo: Unsupported color family (Gray, YUV, RGB supported)";
  else if (!srcVI->width || !srcVI->height || !srcVI->numFrames)
    ErrorText = "InpaintLogo: Source clip has no video";
  else if (!maskNode)
    ErrorText = "InpaintLogo: Mask clip is required for VapourSynth (no embedded-alpha support)";

  /* --- Validate mask format --- */
  if (!ErrorText && maskNode)
  {
    if (!maskVI->format.colorFamily)
      ErrorText = "InpaintLogo: Mask clip has variable format (not supported)";
    else if (maskVI->format.bitsPerSample != 8 || maskVI->format.sampleType != stInteger)
      ErrorText = "InpaintLogo: Mask must be 8-bit integer";
    else if (maskVI->width != srcVI->width || maskVI->height != srcVI->height)
      ErrorText = "InpaintLogo: Source and mask clips have different dimensions";
  }

  if (ErrorText)
  {
    vsapi->mapSetError(out, ErrorText);
    vsapi->freeNode(srcNode);
    if (maskNode)  vsapi->freeNode(maskNode);
    return;
  }

  /* --- Read parameters --- */
  double  Radius       = vsapi->mapGetFloat(in, "radius",        0, &err);  if (err) Radius       = 2.0;
  double  Sharpness    = vsapi->mapGetFloat(in, "sharpness",     0, &err);  if (err) Sharpness    = 0.0;
  double  PreBlur      = vsapi->mapGetFloat(in, "preblur",       0, &err);  if (err) PreBlur      = 1.0;
  double  PostBlur     = vsapi->mapGetFloat(in, "postblur",      0, &err);  if (err) PostBlur     = 1.0;
  double  ChromaWeight = vsapi->mapGetFloat(in, "chromaweight",  0, &err);  if (err) ChromaWeight = -1.0;  /* -1 = auto */
  double  PreBlurSize  = vsapi->mapGetFloat(in, "preblursize",   0, &err);  if (err) PreBlurSize  = 1.5;
  double  PostBlurSize = vsapi->mapGetFloat(in, "postblursize",  0, &err);  if (err) PostBlurSize = 1.5;
  int     ChromaTensor = vsapi->mapGetIntSaturated(in, "chromatensor", 0, &err);  if (err) ChromaTensor = 0;
  double  PAR          = vsapi->mapGetFloat(in, "pixelaspect",   0, &err);  if (err) PAR          = 1.0;
  int     MaxSteps     = vsapi->mapGetIntSaturated(in, "steps",  0, &err);  if (err) MaxSteps     = -1;

  /* --- Validate parameter ranges --- */
  if (Radius <= 0.0)         ErrorText = "InpaintLogo: Radius must be positive";
  else if (Sharpness < 0.0)  ErrorText = "InpaintLogo: Sharpness must be non-negative";
  else if (PreBlur < 0.0)    ErrorText = "InpaintLogo: PreBlur must be non-negative";
  else if (PostBlur < 0.0)   ErrorText = "InpaintLogo: PostBlur must be non-negative";
  else if (PAR <= 0.0)       ErrorText = "InpaintLogo: PixelAspect must be positive";

  const FrameLayout srcFmt  = FrameLayoutFromVSFormat(srcVI->format);
  const FrameLayout maskFmt = maskNode ? FrameLayoutFromVSFormat(maskVI->format) : FrameLayout{};

  /* Compute default ChromaWeight if not specified */
  if (!ErrorText && ChromaWeight < 0.0)
  {
    int chromaChannels = srcFmt.channelCount - 1;  /* mask is always a separate clip for VS4 */
    ChromaWeight = (chromaChannels == 0) ? 0.0 : (srcFmt.colorFamily == ColorFamily::RGB ? 1.0 - 1.0 / chromaChannels : 0.0);
  }
  if (!ErrorText && (ChromaWeight < 0.0 || ChromaWeight > 1.0))
    ErrorText = "InpaintLogo: ChromaWeight must be in [0.0, 1.0]";

  if (ErrorText)
  {
    vsapi->mapSetError(out, ErrorText);
    vsapi->freeNode(srcNode);
    if (maskNode)  vsapi->freeNode(maskNode);
    return;
  }

  /* --- Allocate and set up filter data --- */
  VSInpaintData * d = new (std::nothrow) VSInpaintData;
  if (!d)
  {
    vsapi->mapSetError(out, "InpaintLogo: Could not allocate memory");
    vsapi->freeNode(srcNode);
    if (maskNode)  vsapi->freeNode(maskNode);
    return;
  }
  d->srcNode       = srcNode;
  d->maskFrameCount = maskVI ? maskVI->numFrames : 0;
  d->maskNode       = nullptr;  /* set below after static mask processing */

  ErrorText = SetupInpaintData(&d->inpaint, srcFmt,
    srcVI->width, srcVI->height,
    /*hasMaskClip=*/true, maskFmt, /*maskChannelNo=*/0, /*flipMask=*/false,
    d->maskFrameCount, Radius, Sharpness, PreBlur, PostBlur, ChromaWeight,
    PreBlurSize, PostBlurSize, ChromaTensor != 0, PAR, MaxSteps);

  /* --- Prepare static mask (mask clip with only 1 frame) --- */
  if (!ErrorText && maskNode && d->maskFrameCount <= 1)
  {
    char errMsg[256];
    const VSFrame * maskFrame = vsapi->getFrame(0, maskNode, errMsg, sizeof(errMsg));
    if (!maskFrame)
    {
      ErrorText = "InpaintLogo: Could not get mask frame";
    }
    else
    {
      const uint8_t * maskPlanes[MaxChannelCount] = {};
      int             maskPitch[MaxChannelCount]  = {};
      if (!VsGetFramePlanesRO(maskFrame, maskFmt, maskPlanes, maskPitch, vsapi))
        ErrorText = "InpaintLogo: Could not get mask frame planes";
      else
      {
        switch (InpaintPrepareMask(&d->inpaint.StaticMaskData, maskFmt, maskPlanes, maskPitch, &d->inpaint))
        {
          case InpaintMaskResult::FullMask:
#if ((InpaintFullMaskError) >= 1)
            ErrorText = "InpaintLogo: Mask is full";
#endif
            break;
          case InpaintMaskResult::NoMask:
            break;  /* StaticMaskData stays nullptr: pass-through filter */
          case InpaintMaskResult::MemoryError:
            ErrorText = "InpaintLogo: Mask preparation failed: Out of memory";
            break;
          default:
            ErrorText = "InpaintLogo: Mask preparation failed";
            break;
        }
      }
      vsapi->freeFrame(maskFrame);
    }
    /* Static mask prepared — release mask node (no longer needed per-frame) */
    vsapi->freeNode(maskNode);
    maskNode = nullptr;
  }
  else if (!ErrorText && maskNode)
  {
    /* Dynamic mask: keep node for per-frame requests */
    d->maskNode = maskNode;
    maskNode    = nullptr;
  }

  if (ErrorText)
  {
    vsapi->mapSetError(out, ErrorText);
    if (maskNode)  vsapi->freeNode(maskNode);
    vsapi->freeNode(d->srcNode);
    if (d->inpaint.StaticMaskData)  { delete d->inpaint.StaticMaskData;  d->inpaint.StaticMaskData = nullptr; }
    delete d;
    return;
  }

  /* --- Register filter --- */
  VSFilterDependency  deps[2];
  int numDeps = 1;
  deps[0] = { d->srcNode, rpStrictSpatial };
  if (d->maskNode)
  {
    deps[1]  = { d->maskNode, d->maskFrameCount > 1 ? rpStrictSpatial : rpNoFrameReuse };
    numDeps  = 2;
  }

  vsapi->createVideoFilter(out, "InpaintLogo", srcVI, vsInpaintGetFrame, vsInpaintFree,
    fmParallel, deps, numDeps, d, core);
}


/******************************************************************
                      AnalyzeLogo (VS4)
 ******************************************************************/

struct VSAnalyzeData
{
  const VSFrame * resultFrame;
  VSVideoInfo     vi;
};


static const VSFrame *  VS_CC  vsAnalyzeGetFrame(int  /*n*/,  int  activationReason,  void *  instanceData,  void **  /*frameData*/,  VSFrameContext *  frameCtx,  VSCore *  core,  const VSAPI *  vsapi)
{
  if (activationReason != arAllFramesReady)  return  nullptr;
  VSAnalyzeData * d = static_cast<VSAnalyzeData*>(instanceData);
  return  vsapi->copyFrame(d->resultFrame, core);
}


static void  VS_CC  vsAnalyzeFree(void *  instanceData,  VSCore *  /*core*/,  const VSAPI *  vsapi)
{
  VSAnalyzeData * d = static_cast<VSAnalyzeData*>(instanceData);
  vsapi->freeFrame(d->resultFrame);
  delete d;
}


static void  VS_CC  vsAnalyzeCreate(const VSMap *  in,  VSMap *  out,  void *  /*userData*/,  VSCore *  core,  const VSAPI *  vsapi)
{
  int  err;
  const char * ErrorText = nullptr;

  /* --- Fetch clips --- */
  VSNode * srcNode  = vsapi->mapGetNode(in, "clip", 0, nullptr);
  VSNode * maskNode = vsapi->mapGetNode(in, "mask", 0, nullptr);  /* required */

  const VSVideoInfo * srcVI  = vsapi->getVideoInfo(srcNode);
  const VSVideoInfo * maskVI = vsapi->getVideoInfo(maskNode);

  /* --- Validate --- */
  if (!srcVI->format.colorFamily)
    ErrorText = "AnalyzeLogo: Source clip has variable format (not supported)";
  else if (srcVI->format.bitsPerSample != 8 || srcVI->format.sampleType != stInteger)
    ErrorText = "AnalyzeLogo: Only 8-bit integer formats are supported";
  else if (!srcVI->width || !srcVI->height || !srcVI->numFrames)
    ErrorText = "AnalyzeLogo: Source clip has no video";
  else if (!maskVI->format.colorFamily)
    ErrorText = "AnalyzeLogo: Mask clip has variable format (not supported)";
  else if (maskVI->format.bitsPerSample != 8 || maskVI->format.sampleType != stInteger)
    ErrorText = "AnalyzeLogo: Mask must be 8-bit integer";
  else if (maskVI->width != srcVI->width || maskVI->height != srcVI->height)
    ErrorText = "AnalyzeLogo: Source and mask clips have different dimensions";

  /* --- Parameters --- */
  int    ComputeAlpha      = vsapi->mapGetIntSaturated(in, "computealpha",      0, &err);  if (err) ComputeAlpha      = 0;
  double DeviationWeight   = vsapi->mapGetFloat(in,       "deviationweight",    0, &err);  if (err) DeviationWeight   = 0.0;
  double SubsamplingWeight = vsapi->mapGetFloat(in,       "subsamplingweight",  0, &err);  if (err) SubsamplingWeight = 0.0;

  if (!ErrorText && ComputeAlpha && srcVI->numFrames < 2)
    ErrorText = "AnalyzeLogo: Source clip must have at least two frames for alpha computation";

  if (ErrorText)
  {
    vsapi->mapSetError(out, ErrorText);
    vsapi->freeNode(srcNode);
    vsapi->freeNode(maskNode);
    return;
  }

  /* --- Load mask frame and build mask array --- */
  char errMsg[256];
  const VSFrame * maskFrame = vsapi->getFrame(0, maskNode, errMsg, sizeof(errMsg));
  vsapi->freeNode(maskNode);  maskNode = nullptr;
  if (!maskFrame)
  {
    vsapi->mapSetError(out, "AnalyzeLogo: Could not get mask frame");
    vsapi->freeNode(srcNode);
    return;
  }

  const uint8_t * MaskDataPtr = vsapi->getReadPtr(maskFrame, 0);
  int             MaskPitch   = static_cast<int>(vsapi->getStride(maskFrame, 0));
  int  Width  = srcVI->width;
  int  Height = srcVI->height;
  int  x, y;

  std::vector<uint8_t> MaskVec;
  int32_t MaskSize = 0;
  try  { MaskVec.resize(Width * Height); }
  catch (std::bad_alloc &)
  {
    vsapi->freeFrame(maskFrame);
    vsapi->freeNode(srcNode);
    vsapi->mapSetError(out, "AnalyzeLogo: Could not allocate memory");
    return;
  }
  unsigned char * Mask = MaskVec.data();

  if (ComputeAlpha)
  {
    for (y = 0 ; y < Height ; y++)
      for (x = 0 ; x < Width ; x++)
        MaskSize += Mask[Width * y + x] = ((MaskThresholdSign) * MaskDataPtr[MaskPitch * y + x] > (MaskThresholdSign) * (MaskThreshold));
  }
  else
  {
    for (y = 0 ; y < Height ; y++)
      for (x = 0 ; x < Width ; x++)
      {
        Mask[Width * y + x] = MaskDataPtr[MaskPitch * y + x];
        MaskSize += Mask[Width * y + x] ? 1 : 0;
      }
  }
  vsapi->freeFrame(maskFrame);

  if (!MaskSize)         { vsapi->freeNode(srcNode);  vsapi->mapSetError(out, "AnalyzeLogo: Mask is empty");  return; }
  if (MaskSize == Width * Height)  { vsapi->freeNode(srcNode);  vsapi->mapSetError(out, "AnalyzeLogo: Mask is full");   return; }

  /* --- Allocate result frame --- */
  const FrameLayout srcFmt = FrameLayoutFromVSFormat(srcVI->format);
  VSVideoInfo dstVI = *srcVI;
  if (ComputeAlpha)  dstVI.height *= 2;  /* lower half = alpha channel */

  VSFrame * dstFrame = vsapi->newVideoFrame(&srcVI->format, dstVI.width, dstVI.height, nullptr, core);
  if (!dstFrame)
  {
    vsapi->freeNode(srcNode);
    vsapi->mapSetError(out, "AnalyzeLogo: Could not allocate result frame");
    return;
  }

  uint8_t * destPlanes[MaxChannelCount] = {};
  int        destPitch[MaxChannelCount]  = {};
  VsGetFramePlanes(dstFrame, srcFmt, destPlanes, destPitch, vsapi);

  /* --- Synchronously fetch each source frame and run Analyze --- */
  const VSFrame * currentSrcFrame = nullptr;
  auto fetchSrcFrame = [&](unsigned int n, const uint8_t * planes[], int pitch[]) -> bool {
    currentSrcFrame = vsapi->getFrame(static_cast<int>(n), srcNode, errMsg, sizeof(errMsg));
    if (!currentSrcFrame)  return  false;
    return  VsGetFramePlanesRO(currentSrcFrame, srcFmt, planes, pitch, vsapi);
  };
  auto releaseSrcFrame = [&](unsigned int /*n*/) {
    if (currentSrcFrame)  { vsapi->freeFrame(currentSrcFrame);  currentSrcFrame = nullptr; }
  };

  int32_t AnalyzeResult = Analyze(srcFmt, Width, Height,
    static_cast<unsigned int>(srcVI->numFrames),
    fetchSrcFrame, releaseSrcFrame,
    destPlanes, destPitch, ComputeAlpha != 0,
    Mask, MaskSize, ComputeAlpha, DeviationWeight, SubsamplingWeight);

  vsapi->freeNode(srcNode);

  if (AnalyzeResult == Analyze_Result_SrcClipError)  ErrorText = "AnalyzeLogo: Problem while reading source clip";
  if (AnalyzeResult == Analyze_Result_MemoryError)   ErrorText = "AnalyzeLogo: Could not allocate memory";
  if (AnalyzeResult == Analyze_Result_GeneralError)  ErrorText = "AnalyzeLogo: Internal error while analyzing source clip";
  if (ErrorText)
  {
    vsapi->freeFrame(dstFrame);
    vsapi->mapSetError(out, ErrorText);
    return;
  }

  /* --- Create filter that returns the pre-computed frame --- */
  VSAnalyzeData * d = new (std::nothrow) VSAnalyzeData;
  if (!d)
  {
    vsapi->freeFrame(dstFrame);
    vsapi->mapSetError(out, "AnalyzeLogo: Could not allocate memory");
    return;
  }
  d->resultFrame = dstFrame;
  d->vi          = dstVI;
  d->vi.numFrames = static_cast<int>(AnalyzeResult);

  vsapi->createVideoFilter(out, "AnalyzeLogo", &d->vi, vsAnalyzeGetFrame, vsAnalyzeFree,
    fmParallel, nullptr, 0, d, core);
}


/******************************************************************
                      Plugin entry point
 ******************************************************************/

VS_EXTERNAL_API(void)  VapourSynthPluginInit2(VSPlugin *  plugin,  const VSPLUGINAPI *  vspapi)
{
  vspapi->configPlugin("info.avisynth.avsinpaint", "avsinpaint",
    "Logo Inpainting for VapourSynth",
    VS_MAKE_VERSION(1, 3), VAPOURSYNTH_API_VERSION, 0, plugin);

  vspapi->registerFunction("InpaintLogo",
    "clip:vnode;mask:vnode;"
    "radius:float:opt;sharpness:float:opt;preblur:float:opt;postblur:float:opt;"
    "chromaweight:float:opt;preblursize:float:opt;postblursize:float:opt;"
    "chromatensor:int:opt;pixelaspect:float:opt;steps:int:opt;",
    "clip:vnode;",
    vsInpaintCreate, nullptr, plugin);

  vspapi->registerFunction("AnalyzeLogo",
    "clip:vnode;mask:vnode;"
    "computealpha:int:opt;deviationweight:float:opt;subsamplingweight:float:opt;",
    "clip:vnode;",
    vsAnalyzeCreate, nullptr, plugin);
}
