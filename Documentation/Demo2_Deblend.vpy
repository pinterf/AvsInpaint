"""
Demo script for DeblendLogo — VapourSynth V4
AviSynth twin: Demo2_Deblend.avs.

WHAT THIS SCRIPT DOES
---------------------
- Builds a colour-bar background (8 standard SMPTE bars, YUV444P8 limited range).
- Alpha-blends a semi-transparent logo rectangle onto it, then runs
  DeblendLogo to mathematically recover the original background.
- Row 3 of the output is the ground-truth clean background so you can compare
  directly.

HOW DeblendLogo WORKS
---------------------
DeblendLogo is the mathematical inverse of alpha-compositing.  Given:
  - the composited frame  (clip_with_logo)
  - the logo colour image  (logo)
  - the per-pixel alpha    (alpha, GRAY8 where 0 = transparent, 255 = opaque)
it solves for the original background pixel:
      bg = (255 * blended - alpha * logo) / (255 - alpha)

Consequence: pixels where alpha = 255 (fully opaque) make the denominator zero
and cannot be recovered — DeblendLogo leaves those pixels undefined.
Keep LOGO_ALPHA below 255 (180–220 is a realistic TV-station range).

WHEN TO USE DeblendLogo vs InpaintLogo
---------------------------------------
  DeblendLogo — semi-transparent logo (alpha < 255 everywhere in the region).
                The original background shines through; algebra can undo the blend.

  InpaintLogo — fully opaque logo (alpha = 255).
                No information from the background survives; the pixel must be
                reconstructed from its neighbours by interpolation.

  Real logos often combine both: semi-transparent body (→ DeblendLogo) with an
  opaque core or hard outline (→ InpaintLogo on the remaining pixels).
  See Demo2.vpy for the InpaintLogo step.

NOTES
-----
- The mask clip is always a separate argument: VapourSynth has no embedded-alpha
  format, unlike the AviSynth RGB32/RGBAP shorthand.
- The alpha clip may be any 8-bit color family; plane 0 is used, so GRAY8 is
  the natural choice.

SETUP
-----
- AVSInpaint.dll is taken from the VapourSynth plugin autoload folder if it is
  installed there; otherwise set DLL_PATH below.
- Open in VapourSynth Editor (or vspipe). Output is a single clip with all
  stages stacked vertically.
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

LOGO_X, LOGO_Y = 20, 20
LOGO_W, LOGO_H = W - 2 * LOGO_X, 60
LENGTH         = 100

# Alpha opacity of the logo: 0 = invisible, 255 = fully opaque.
# DeblendLogo cannot recover pixels where alpha = 255.
LOGO_ALPHA = 180

# ---------------------------------------------------------------------------
# Background: colour bars (YUV444P8, limited range)
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
# Logo colour patch (the pure logo image before blending)
# ---------------------------------------------------------------------------
logo_patch = core.std.BlankClip(width=LOGO_W, height=LOGO_H, format=vs.YUV444P8,
                                length=LENGTH, color=[210, 60, 210])
logo = core.std.AddBorders(
    logo_patch,
    left=LOGO_X,  right=W - LOGO_X - LOGO_W,
    top=LOGO_Y,   bottom=H - LOGO_Y - LOGO_H,
    color=[16, 128, 128])   # outside the patch: irrelevant

# ---------------------------------------------------------------------------
# Alpha mask: uniform opacity within the logo rectangle (GRAY8)
# ---------------------------------------------------------------------------
alpha_patch = core.std.BlankClip(width=LOGO_W, height=LOGO_H, format=vs.GRAY8,
                                  length=LENGTH, color=LOGO_ALPHA)
alpha = core.std.AddBorders(
    alpha_patch,
    left=LOGO_X,  right=W - LOGO_X - LOGO_W,
    top=LOGO_Y,   bottom=H - LOGO_Y - LOGO_H,
    color=0)

# ---------------------------------------------------------------------------
# Composite: blend logo onto background
#   MaskedMerge(clipa, clipb, mask) = clipa*(1-mask/255) + clipb*(mask/255)
#   so MaskedMerge(bg, logo, alpha) gives the alpha-blended result.
# ---------------------------------------------------------------------------
clip_with_logo = core.std.MaskedMerge(bg, logo, alpha)

# ---------------------------------------------------------------------------
# DeblendLogo: recover the background
#   Signature: DeblendLogo(clip, logo, alpha)
# ---------------------------------------------------------------------------
result = core.avsinpaint.DeblendLogo(clip_with_logo, logo, alpha)

# ---------------------------------------------------------------------------
# Output: stacked vertically
#   Row 1 — deblended result   (should match row 3 closely)
#   Row 2 — clip with logo     (input to DeblendLogo)
#   Row 3 — clean background   (ground truth — not available in a real scenario)
#   Row 4 — logo colour patch  (what was blended in)
#   Row 5 — alpha mask
# ---------------------------------------------------------------------------
def label(clip, text):
    return core.text.Text(clip, text, alignment=8)

core.std.StackVertical([
    label(result, "After DeblendLogo"),
    label(clip_with_logo, "Before (blended)"),
    label(bg, "Ground truth (clean bg)"),
    label(logo, "Logo colour patch"),
    label(core.resize.Point(alpha, format=vs.YUV444P8), "Alpha mask"),
]).set_output(0)
