"""
Demo script for AvsInpaint — VapourSynth V4
Demonstrates InpaintLogo on a synthetic YUV444P8 clip.
AviSynth twin: Demo2.avs.

SETUP
-----
- AVSInpaint.dll is taken from the VapourSynth plugin autoload folder if it is
  installed there; otherwise set DLL_PATH below.
- Open in VapourSynth Editor (or vspipe). Output is a single clip: result /
  before / mask stacked vertically.

WHAT THIS SCRIPT DOES
---------------------
- Builds a colour-bar background (8 standard SMPTE bars, YUV444P8 limited range).
- Pastes a bright solid rectangle at the upper-left corner to simulate an opaque
  TV-station bug (Y=210, U=60, V=210 — pinkish-purple; clearly visible on all bars).
- Creates a GRAY8 mask that is exactly the bounding box of that rectangle.
- Runs InpaintLogo to reconstruct the covered pixels from the surrounding content.

NOTES
-----
- The logo sits entirely within the first (white) colour bar, so the inpainter
  reconstructs a clean white region — easy, predictable, good for a first test.
- The mask is the exact bounding box of the logo. For a real workflow you would
  inflate it slightly with DistanceFunction to catch any blending fringe at the
  edges. See Demo2_DistanceFunction.vpy for how that works.
- This demo uses a fully opaque logo, so only InpaintLogo is needed. If the logo
  is semi-transparent (blended onto the image), use DeblendLogo first, then
  InpaintLogo for any opaque remainder. See Demo2_Deblend.vpy for that workflow.
- VapourSynth has no embedded-alpha format, so the mask clip is always a separate
  argument — unlike the AviSynth RGB32/RGBAP shorthand.
- Only 8-bit integer formats are supported by this plugin under VapourSynth V4.
"""

import os

import vapoursynth as vs
core = vs.core

# Use the autoloaded plugin if present; otherwise load it from DLL_PATH
# (default: the x64 Release build output of this repository).
DLL_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                        r"..\x64\Release\AvsInPaint.dll")
if not hasattr(core, "avsinpaint"):
    core.std.LoadPlugin(DLL_PATH)

# ---------------------------------------------------------------------------
# Geometry
# ---------------------------------------------------------------------------
W, H = 320, 200

LOGO_X, LOGO_Y = 20, 20      # upper-left corner of the logo
LOGO_W, LOGO_H = W-2*LOGO_X, 60  # logo size
LENGTH         = 100          # clip length in frames

# ---------------------------------------------------------------------------
# Background: horizontal colour bars (YUV444P8, limited range)
#   Y=16..235  U/V=16..240  neutral chroma = 128
# ---------------------------------------------------------------------------
BAR_COLORS = [
    [235, 128, 128],   # white
    [219,  16, 138],   # yellow
    [188, 154,  16],   # cyan
    [173,  42,  26],   # green
    [ 78, 214, 230],   # magenta
    [ 63, 102, 240],   # red
    [ 32, 240, 118],   # blue
    [ 16, 128, 128],   # black
]

bar_w = W // len(BAR_COLORS)
bars  = [core.std.BlankClip(width=bar_w, height=H, format=vs.YUV444P8,
                             length=LENGTH, color=c)
         for c in BAR_COLORS]
bg = core.std.StackHorizontal(bars)

# ---------------------------------------------------------------------------
# Logo: a bright solid rectangle (simulates an opaque TV-station bug)
# ---------------------------------------------------------------------------
logo_patch = core.std.BlankClip(width=LOGO_W, height=LOGO_H, format=vs.YUV444P8,
                                 length=LENGTH, color=[210, 60, 210])

logo_full = core.std.AddBorders(
    logo_patch,
    left=LOGO_X,         right=W - LOGO_X - LOGO_W,
    top=LOGO_Y,          bottom=H - LOGO_Y - LOGO_H,
    color=[16, 128, 128])   # fill colour outside the patch is irrelevant

# Mask: GRAY8, white (255) where the logo sits, black (0) everywhere else
mask_patch = core.std.BlankClip(width=LOGO_W, height=LOGO_H, format=vs.GRAY8,
                                 length=LENGTH, color=255)
mask = core.std.AddBorders(
    mask_patch,
    left=LOGO_X,         right=W - LOGO_X - LOGO_W,
    top=LOGO_Y,          bottom=H - LOGO_Y - LOGO_H,
    color=0)

# Composite: paste the logo patch over the background
clip_with_logo = core.std.MaskedMerge(bg, logo_full, mask)

# ---------------------------------------------------------------------------
# InpaintLogo
# ---------------------------------------------------------------------------
result = core.avsinpaint.InpaintLogo(
    clip_with_logo, mask,
    radius=5.0, sharpness=25.0,
    preblur=1.0, postblur=2.0,
)

# ---------------------------------------------------------------------------
# Output: stacked vertically (the GRAY8 mask is converted to YUV444P8)
# ---------------------------------------------------------------------------
def label(clip, text):
    return core.text.Text(clip, text, alignment=8)

core.std.StackVertical([
    label(result, "After InpaintLogo"),
    label(clip_with_logo, "Before (with logo)"),
    label(core.resize.Point(mask, format=vs.YUV444P8), "Mask"),
]).set_output(0)
