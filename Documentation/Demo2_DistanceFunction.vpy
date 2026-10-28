"""
Demo script for DistanceFunction — VapourSynth V4
AviSynth twin: Demo2_DistanceFunction.avs.

WHAT THIS SCRIPT DOES
---------------------
- Creates a GRAY8 binary mask (white rectangle on black background).
- Demonstrates three DistanceFunction modes:
  - Outward gradient: fades to black moving away from the rectangle edge
  - Inward  gradient: fades to black moving toward the rectangle centre
  - Binary threshold: scale=0, just binarises at 127 (sanity check: the input is already binary, so it equals the original mask)

WHAT DistanceFunction DOES
--------------------------
For each pixel, DistanceFunction computes its distance to the boundary of the
"significant area" (pixels > 127) and maps that distance to a grey value.

  Positive scale:  the significant area (> 127) is turned white; every pixel
                   outside it is assigned  max(0, 255 - distance * scale).
                   Result: a gradient that fades to black moving away from
                   the shape edge.  Useful for building soft feathered masks.

  Negative scale:  the image is inverted before and after the computation,
                   so the gradient runs inward from the edge toward the shape
                   centre.  Useful for eroding or shrinking a mask.

  scale = 0:       binarises the clip (threshold at 127), no gradient.

PRACTICAL USE IN A DELOGO WORKFLOW
-----------------------------------
  # 1. Start from a hard binary mask (e.g. the exact logo bounding box):
  rough_mask = ...   # GRAY8, white where logo is

  # 2. Inflate outward by ~4 px to catch blending fringe:
  inflated = core.avsinpaint.DistanceFunction(rough_mask, scale=255.0/4)
  inflated = core.std.Binarize(inflated, threshold=1)   # back to binary

  # 3. Pass the inflated mask to InpaintLogo.
  #
  # Alternatively pass the soft (gradient) mask directly — InpaintLogo uses
  # the > 127 threshold internally, so the gradient edge won't affect which
  # pixels are inpainted, but a soft mask is handy for blending the result
  # back with MaskedMerge.

NOTES
-----
- Only 8-bit integer formats are supported by this plugin under VapourSynth V4.
- DistanceFunction works on Gray and YUV clips; RGB is rejected, because
  VapourSynth has no RGBA format and therefore no alpha channel to work on.

SETUP
-----
- AVSInpaint.dll is taken from the VapourSynth plugin autoload folder if it is
  installed there; otherwise set DLL_PATH below.
- Open in VapourSynth Editor (or vspipe). Output is a single clip with all
  variants stacked vertically.
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
W, H  = 320, 180
LENGTH = 1          # a single frame is enough for a mask demo

RECT_X, RECT_Y = 80, 45
RECT_W, RECT_H = 160, 90

# ---------------------------------------------------------------------------
# Binary mask: white rectangle on black background (GRAY8)
# ---------------------------------------------------------------------------
rect     = core.std.BlankClip(width=RECT_W, height=RECT_H, format=vs.GRAY8,
                               length=LENGTH, color=255)
mask_bin = core.std.AddBorders(rect,
    left=RECT_X,    right=W - RECT_X - RECT_W,
    top=RECT_Y,     bottom=H - RECT_Y - RECT_H,
    color=0)

# ---------------------------------------------------------------------------
# DistanceFunction variants
# ---------------------------------------------------------------------------

# Outward gradient: the value reaches 0 at a distance of 16 px outside the
# rectangle — scale = 255/16
dist_outward = core.avsinpaint.DistanceFunction(mask_bin, scale=255.0 / 16)

# Inward gradient: the value reaches 0 at a distance of 16 px inside the
# rectangle — scale = -255/16
dist_inward  = core.avsinpaint.DistanceFunction(mask_bin, scale=-(255.0 / 16))

# Binary threshold only (scale=0). The input is already 0/255, so the result is identical to mask_bin
dist_binary  = core.avsinpaint.DistanceFunction(mask_bin, scale=0.0)

# ---------------------------------------------------------------------------
# Output: stacked vertically (GRAY8 -> YUV444P8, as in the AviSynth twin)
#   Row 1 — outward gradient  (inflate / feather outward)
#   Row 2 — inward  gradient  (erode  / feather inward)
#   Row 3 — binary threshold  (scale=0, sanity check: same as row 4)
#   Row 4 — original binary mask
# ---------------------------------------------------------------------------
def label(clip, text):
    return core.text.Text(core.resize.Point(clip, format=vs.YUV444P8), text, alignment=8)

core.std.StackVertical([
    label(dist_outward, "Outward gradient (scale=255/16)"),
    label(dist_inward, "Inward gradient (scale=-255/16)"),
    label(dist_binary, "Binary (scale=0): same as below"),
    label(mask_bin, "Original binary mask"),
]).set_output(0)
