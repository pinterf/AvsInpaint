"""
Demo script for AvsInpaint - VapourSynth V4
VapourSynth port of Demo_YUV.avs, the YUV (YUV420P8) version of Demo.vpy:
the clip with the logo is converted to YUV420P8 (Rec.601, limited range)
before the logo is analyzed and removed. The masks are GRAY8 as in Demo.vpy.
Each YUV clip is converted back to RGB24 only for display.

WHAT THIS SCRIPT DOES
---------------------
The same as Demo.avs: it builds a test clip with a semi-transparent text logo,
lets AnalyzeLogo estimate the logo from a roughly drawn mask, removes the
transparent parts with DeblendLogo and the opaque parts with InpaintLogo, and
shows the whole workflow as one labelled multi-panel view
(OutputVerbosity = 2):

  column 1  clean test clip / logo (colour part) / logo (alpha mask)
  column 2  analyze mask / deblend mask / all masks (alternating)
  column 3  clip with logo / estimated logo colour / estimated logo alpha
  column 4  deblended clip / repair mask / repaired clip
  column 5  empty / final clip / empty

The workflow, step by step:

  1. A colourful pattern moves through an 80x80 window, so every pixel sees
     different content over the frames. The analysis needs that: the alpha of
     a pixel can only be estimated if the content underneath it changes.
  2. The words "fancy logo" are blended onto it. The logo colour is a
     horizontal black-to-white ramp, its alpha is the text, faded out towards
     the top and the bottom - so the logo has opaque and transparent parts.
  3. A rough mask - the logo area, grown by AnalyzeHalo pixels - is given to
     AnalyzeLogo, which estimates the logo's colour map and alpha map from the
     temporal variation of the frames.
  4. DeblendLogo undoes the alpha blending of the transparent parts.
  5. The (nearly) opaque parts cannot be deblended - the background
     information is lost there - so they are inpainted by InpaintLogo.
  6. The result is blended back over the clip using the soft deblend mask.

DIFFERENCES FROM THE ORIGINAL Demo.avs
--------------------------------------
- Text: VapourSynth R70 has no Subtitle filter, only core.text.Text with a
  built-in 8x16 bitmap font, no colour parameter and no anti-aliasing.
  - The panel captions are rendered as a glyph mask and composited as white
    text with a 1 pixel black halo, like the Subtitle look of the original.
    The font size is fixed, so long captions are wrapped onto two lines.
  - The "fancy logo" text and the version string of the background pattern
    are rendered at 4x size and scaled down, which gives them anti-aliased
    edges similar to Subtitle (the logo alpha would be binary otherwise).
    The font is a different one, so the logo shape is not identical.
- Alpha: VapourSynth has no alpha-carrying format and no ShowAlpha/Mask, so
  the logo colour and the logo alpha are separate clips throughout, and the
  masks are GRAY8 clips (the plugin always reads plane 0 of a mask).
- Layer: the original blends with AviSynth Layer, whose default op "add" is a
  plain alpha blend (equivalent to Overlay mode="blend", not an addition), so
  core.std.MaskedMerge is the exact equivalent. VapourSynth R70 has no Layer.
- Levels: the threshold steps of the original (Levels with a 1 count wide
  input range) are done with core.std.Binarize, with the same thresholds.
- The original moves the pattern with Overlay + FrameEvaluate (OL_X_Offset,
  OL_Y_Offset); here core.std.FrameEval crops the same window per frame.
- The original stores the AnalyzeLogo result in an image file and reuses it
  on the next run. AnalyzeLogo is fast enough on this small clip, so the
  analysis is simply redone every time the script is loaded.

SETUP
-----
- AVSInpaint.dll is taken from the VapourSynth plugin autoload folder if it is
  installed there; otherwise set DLL_PATH below.
- Open in VapourSynth Editor (or vspipe); the output is the stacked view.
"""

import math
import os
import textwrap

import vapoursynth as vs
core = vs.core

# Use the autoloaded plugin if present; otherwise load it from DLL_PATH
# (default: the x64 Release build output of this repository).
DLL_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                        r"..\x64\Release\AvsInPaint.dll")
if not hasattr(core, "avsinpaint"):
    core.std.LoadPlugin(DLL_PATH)

# ---------------------------------------------------------------------------
# Parameters (the names and values of the original Demo.avs)
# ---------------------------------------------------------------------------
# Set some variables for the output clip
OutputSize = 192  # several clips are stacked together - width and height of each
OutputVerbosity = 2  # -1, 0, 1, 2 allowed

# Set the properties of the test clip here
Size = 80  # width and height of the test clip
AnalyzeHalo = 2  # additional size of the region (around the real logo) where we
                 # tell the Analyze function to find the logo.
                 # In a real application, you would draw this region by hand.

# Some parameters similar to the DeLogo filter
DeblendFalloff = Size / 8.0
AlphaToRepair = 20
RepairRadius = 2
# ...and some inpainting parameters
InpaintRadius = Size / 12.0  # similar to "Depth"
InpaintSharpness = 25.0
InpaintPreBlur = Size / 50.0
InpaintPostBlur = Size / 30.0

# AviSynth colors_rgb.avsi values
Color_Red, Color_Yellow, Color_Green = [255, 0, 0], [255, 255, 0], [0, 128, 0]
Color_Cyan, Color_Blue, Color_Magenta = [0, 255, 255], [0, 0, 255], [255, 0, 255]
Color_White, Color_Black = [255, 255, 255], [0, 0, 0]

TEXT_MARGIN = 16  # core.text.Text keeps this distance from the frame edges


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------
def avs_round(v):
    """AviSynth Round: halves are rounded up."""
    return int(math.floor(v + 0.5))


def blank_rgb(w, h, color, length=1):
    return core.std.BlankClip(width=w, height=h, format=vs.RGB24, length=length, color=color)


def blank_gray(w, h, value, length=1):
    return core.std.BlankClip(width=w, height=h, format=vs.GRAY8, length=length, color=value)


def gray_to_rgb(gray):
    """GRAY8 -> RGB24 with R=G=B (the original's ConvertToRGB24 of a mask)."""
    return core.std.ShufflePlanes(gray, planes=[0, 0, 0], colorfamily=vs.RGB)


def text_mask(text, height, supersample=4, bold=0):
    """Anti-aliased GRAY8 mask (0..255) of a single line of text, `height` px tall.

    core.text.Text has no anti-aliasing, so the text is drawn `supersample`
    times larger and scaled down. `bold` grows the glyphs (in large pixels),
    like the Halo_Color=Text_Color trick of the original.
    """
    w, h = len(text) * 8 * supersample, 16 * supersample
    canvas = blank_gray(w + 2 * TEXT_MARGIN, max(h + 2 * TEXT_MARGIN, 64), 0)
    glyphs = core.text.Text(canvas, text, alignment=7, scale=supersample)
    glyphs = core.std.CropAbs(glyphs, width=w, height=h, left=TEXT_MARGIN, top=TEXT_MARGIN)
    mask = core.std.Binarize(glyphs, threshold=128)  # drop the 16-valued glyph cells
    for _ in range(bold):
        mask = core.std.Maximum(mask)
    return core.resize.Bilinear(mask, width=avs_round(w * height / h), height=height)


def place(mask, w, h, x, y):
    """Put a GRAY8 mask at (x, y) on a black w x h frame."""
    return core.std.AddBorders(mask, left=x, top=y,
                               right=w - x - mask.width, bottom=h - y - mask.height)


# ---------------------------------------------------------------------------
# Creates a funny colourful clip (CreateExampleClip of the original)
# ---------------------------------------------------------------------------
def create_example_clip(S):
    wy, wx, ax, vx = 0.23, 0.3, 40.0, 0.1 * S
    cR, cY, cG, cC = (blank_rgb(1, 1, c) for c in (Color_Red, Color_Yellow, Color_Green, Color_Cyan))
    cB, cM, cW, cK = (blank_rgb(1, 1, c) for c in (Color_Blue, Color_Magenta, Color_White, Color_Black))
    grid = core.std.StackVertical([
        core.std.StackHorizontal([cR, cW, cG, cK, cB, cW, cR]),
        core.std.StackHorizontal([cK, cY, cW, cC, cK, cM, cW]),
        core.std.StackHorizontal([cR, cW, cB, cK, cG, cW, cR]),
        core.std.StackHorizontal([cW, cM, cK, cC, cW, cY, cK]),
    ])
    band = core.std.StackHorizontal([grid, core.std.FlipVertical(grid)])
    band = core.resize.Bilinear(band, width=16 * 32, height=32)

    # The version string as a white outline (the original adds the black halo
    # of a white-on-white Subtitle, inverted, onto the band).
    version = text_mask(f"VapourSynth R{core.core_version.release_major}",
                        height=math.floor(27.8 * band.width / 512))
    version = place(version, band.width, band.height, 8, 2)
    outline = core.std.Expr([core.std.Maximum(version), version], "x y -")
    band = core.std.MaskedMerge(band, blank_rgb(band.width, band.height, Color_White),
                                outline, first_plane=True)

    band = core.resize.Bilinear(band, width=64 * S, height=2 * S)

    # A window of S x S pixels moves along the band (the original uses Overlay
    # with per-frame OL_X_Offset / OL_Y_Offset).
    length = avs_round((band.width - band.height // 2 - ax) / vx)
    band = core.std.Loop(band, length)

    def window(n):
        left = avs_round(vx * n + ax * math.sin(wx * n))
        top = band.height // 4 - avs_round(0.5 * S * math.cos(wy * n))
        return core.std.CropAbs(band, width=S, height=S, left=left, top=top)

    base = core.std.CropAbs(band, width=S, height=S)
    return core.std.FrameEval(base, window)


# ---------------------------------------------------------------------------
# Creates a logo with opaque and transparent regions (CreateExampleLogo)
#   Returns (colour, alpha): an RGB24 clip and a GRAY8 clip, one frame each.
# ---------------------------------------------------------------------------
def create_example_logo(S):
    # alpha: the text "fancy logo", faded out towards the top and the bottom
    fade = core.std.StackVertical([blank_gray(2, 1, 0), blank_gray(2, 2, 255), blank_gray(2, 1, 0)])
    fade = core.resize.Bilinear(fade, width=S, height=S)
    fancy = text_mask("fancy", height=avs_round(2.7 * S / 8.0), bold=2)
    logo = text_mask("logo", height=avs_round(3.0 * S / 8.0), bold=2)
    text = core.std.Expr([place(fancy, S, S, avs_round(9.0 * S / 80.0), avs_round(8.0 * S / 80.0)),
                          place(logo, S, S, avs_round(10.0 * S / 80.0), avs_round(36.0 * S / 80.0))],
                         "x y max")
    alpha = core.std.Expr([fade, text], "x y * 255 /")

    # colour: a horizontal black-to-white ramp
    ramp = core.std.StackHorizontal([blank_gray(1, 2, 0), blank_gray(1, 2, 255)])
    colour = gray_to_rgb(core.resize.Bilinear(ramp, width=S, height=S))
    return colour, alpha


# ---------------------------------------------------------------------------
# Prepares a clip for output (LabelClip): scale to the panel size, write
# the captions at the top and at the bottom.
# ---------------------------------------------------------------------------
def label_clip(clip, top, bot):
    clip = core.resize.Point(clip, width=OutputSize, height=OutputSize)
    if not top and not bot:
        return clip

    # Glyphs at x=2 and 4 px from the top / bottom edge; the 8 px wide glyphs
    # allow 23 characters per line.
    chars = (OutputSize - 2) // 8
    canvas = blank_gray(OutputSize + 2 * TEXT_MARGIN, OutputSize + 2 * TEXT_MARGIN - 8, 0)
    if top:
        canvas = core.text.Text(canvas, "\n".join(textwrap.wrap(top, chars)), alignment=7)
    if bot:
        canvas = core.text.Text(canvas, "\n".join(textwrap.wrap(bot, chars)), alignment=1)
    canvas = core.std.CropAbs(canvas, width=OutputSize, height=OutputSize,
                              left=TEXT_MARGIN - 2, top=TEXT_MARGIN - 4)
    glyphs = core.std.Binarize(canvas, threshold=128)
    halo = core.std.Maximum(glyphs)

    clip = core.std.MaskedMerge(clip, blank_rgb(OutputSize, OutputSize, Color_Black), halo, first_plane=True)
    return core.std.MaskedMerge(clip, blank_rgb(OutputSize, OutputSize, Color_White), glyphs, first_plane=True)


# ---------------------------------------------------------------------------
# Prepare example clip
# ---------------------------------------------------------------------------
ReferenceClip = create_example_clip(Size)
ReferenceLogoColor, ReferenceLogoAlpha = create_example_logo(Size)

# The logo area (alpha > 0), grown by AnalyzeHalo pixels: the distance
# function falls off by 10 per pixel, so values >= 256 - AnalyzeHalo*10 remain.
Mask4Analyze = core.std.Binarize(ReferenceLogoAlpha, threshold=1)
Mask4Analyze = core.avsinpaint.DistanceFunction(Mask4Analyze, scale=10.0)
Mask4Analyze = core.std.Binarize(Mask4Analyze, threshold=256 - AnalyzeHalo * 10)

Clip = core.std.MaskedMerge(ReferenceClip, ReferenceLogoColor, ReferenceLogoAlpha, first_plane=True)
Clip = core.resize.Bicubic(Clip, format=vs.YUV420P8, matrix_s="170m")

# Status here:
#   Clip = YUV (YUV420P8) video clip with an annoying logo inside
#   Mask4Analyze = a b/w (GRAY8) mask of where we (could have) located the logo approximately


################################################################
## Most of this file is uninteresting stuff needed to prepare ##
## the demo clips or compose a nice output clip out of the    ##
## results. The following lines are more interesting, as the  ##
## "AnalyzeLogo" function is invoked here.                    ##
################################################################

# Analyze the logo; the colour and the alpha mask are returned. The frame count
# of the result is the number of frames processed.
# (You can save some CPU time by analyzing only parts of the clip,
#  e.g. core.std.SelectEvery(Clip, 8, 0).)
Analyzed = core.avsinpaint.AnalyzeLogo(Clip, Mask4Analyze, computealpha=1)
AnalyzeFrameCount = Analyzed.num_frames
Analyzed = core.std.Trim(Analyzed, length=1)  # all frames are equal - we need only one
LogoColor = core.std.Crop(Analyzed, bottom=Size)  # the colour map is in the top half
LogoAlpha = core.std.ShufflePlanes(core.std.Crop(Analyzed, top=Size),  # the alpha in the luma of the bottom half
                                   planes=0, colorfamily=vs.GRAY)

# Prepare Deblend mask (will be applied at the very end)
DeblendMask = core.avsinpaint.DistanceFunction(Mask4Analyze, scale=255.0 / DeblendFalloff)

# Prepare repair mask (marks pixels that cannot be deblended):
# alpha >= 255 - AlphaToRepair, grown by RepairRadius pixels
RepairMask = core.std.Binarize(LogoAlpha, threshold=255 - AlphaToRepair)
if RepairRadius > 0.1:
    RepairMask = core.avsinpaint.DistanceFunction(RepairMask, scale=127.0 / RepairRadius)
RepairMask = core.std.Binarize(RepairMask, threshold=128)


################################################################
## These lines are also interesting as both, the transparent  ##
## parts of the logo (with "DeblendLogo") and the opaque      ##
## parts (with InpaintLogo) are removed.                      ##
################################################################
# Let's go...
DeblendedClip = core.avsinpaint.DeblendLogo(Clip, LogoColor, LogoAlpha)  # transparent parts removed
RepairedClip = core.avsinpaint.InpaintLogo(DeblendedClip, RepairMask,
                                           radius=InpaintRadius, sharpness=InpaintSharpness,
                                           preblur=InpaintPreBlur, postblur=InpaintPostBlur)
# This step is done by Karel Suhajda's DeLogo to remove some dirt created by wrongly
# estimated alpha masks. It is not needed for this example as our clip is not embedded
# in some bigger movie!! It is only left to resemble the DeLogo behaviour more precisely.
FinalClip = core.std.MaskedMerge(Clip, RepairedClip, DeblendMask, first_plane=True)


# ---------------------------------------------------------------------------
# Prepare output
# ---------------------------------------------------------------------------
Length = Clip.num_frames
# The YUV clips are converted to RGB24 for display
Clip, LogoColor, DeblendedClip, RepairedClip, FinalClip = (
    core.resize.Bicubic(c, format=vs.RGB24, matrix_in_s="170m")
    for c in (Clip, LogoColor, DeblendedClip, RepairedClip, FinalClip))
Mask4AnalyzeV = gray_to_rgb(Mask4Analyze)
ReferenceLogoAlphaV = gray_to_rgb(ReferenceLogoAlpha)
LogoAlphaV = gray_to_rgb(LogoAlpha)
DeblendMaskV = gray_to_rgb(DeblendMask)
RepairMaskV = gray_to_rgb(RepairMask)

# The masks one after the other, each for one frame, over the clip length
AllMasks = core.std.Interleave([label_clip(Mask4AnalyzeV, "", "Analyze Mask"),
                                label_clip(ReferenceLogoAlphaV, "", "Logo Alpha Mask"),
                                label_clip(LogoAlphaV, "", "Estm. Alpha Mask"),
                                label_clip(RepairMaskV, "", "Repair Mask"),
                                label_clip(DeblendMaskV, "", "Deblend Mask")])
AllMasks = core.std.Loop(AllMasks, Length // 5)
AllMasks = label_clip(AllMasks, "All Masks (alternating)", "")

Analyzed_note = f"as determined by Analyze ({AnalyzeFrameCount} frames used)"
NotReal = "You wouldn't have this in reality!"

ClipP = label_clip(Clip, "Clip with Logo", "We want to remove the logo")
ReferenceClipP = label_clip(ReferenceClip, f"Clean test clip ({ReferenceClip.width}x{ReferenceClip.height})", NotReal)
ReferenceLogoColorP = label_clip(ReferenceLogoColor, "Logo (color part)", NotReal)
ReferenceLogoAlphaP = label_clip(ReferenceLogoAlphaV, "Logo (alpha mask)", NotReal)
Mask4AnalyzeP = label_clip(Mask4AnalyzeV, "Analyze Mask", "You would draw this by hand")
LogoColorP = label_clip(LogoColor, "Estimated Logo (color part)", Analyzed_note)
LogoAlphaP = label_clip(LogoAlphaV, "Estimated Logo (alpha mask)", Analyzed_note)
DeblendMaskP = label_clip(DeblendMaskV, "Deblend Mask", "(applied at the very end)")
RepairMaskP = label_clip(RepairMaskV, "Repair Mask", "where InpaintLogo is applied")
DeblendedClipP = label_clip(DeblendedClip, "Deblended Clip", "transparent parts of logo removed")
RepairedClipP = label_clip(RepairedClip, "Repaired Clip", "opaque parts of logo inpainted")
FinalClipP = label_clip(FinalClip, "Final Clip", "after the Deblend Mask was applied")
NoClip = blank_rgb(OutputSize, OutputSize, Color_Black)

# Stacking repeats the last frame of the shorter (single frame) clips
Output = RepairedClipP
if OutputVerbosity >= 0:
    Output = FinalClipP
if OutputVerbosity >= 1:
    Output = core.std.StackHorizontal([ClipP, LogoColorP, LogoAlphaP, DeblendedClipP, RepairedClipP])
if OutputVerbosity >= 2:
    Output = core.std.StackHorizontal([
        core.std.StackVertical([ReferenceClipP, ReferenceLogoColorP, ReferenceLogoAlphaP]),
        core.std.StackVertical([Mask4AnalyzeP, DeblendMaskP, AllMasks]),
        core.std.StackVertical([ClipP, LogoColorP, LogoAlphaP]),
        core.std.StackVertical([DeblendedClipP, RepairMaskP, RepairedClipP]),
        core.std.StackVertical([NoClip, FinalClipP, NoClip]),
    ])

Output.set_output(0)
