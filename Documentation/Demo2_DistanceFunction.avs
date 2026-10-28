# Demo2_DistanceFunction.avs — DistanceFunction for AviSynth+
# VapourSynth twin: Demo2_DistanceFunction.vpy.
#
# SETUP
# -----
# Place AVSInpaint.dll in the AviSynth+ plugins folder; it is autoloaded.
#
# WHAT THIS SCRIPT DOES
# ---------------------
# Creates a Y8 binary mask (white rectangle on black background) and
# demonstrates three DistanceFunction modes stacked vertically:
#   - Outward gradient: fades to black moving away from the rectangle edge
#   - Inward  gradient: fades to black moving toward the rectangle centre
#   - Binary threshold: scale=0, just binarises at 127 (sanity check: the input is already binary, so it equals the original mask)
#
# WHAT DistanceFunction DOES
# --------------------------
# For each pixel, DistanceFunction computes its distance to the boundary of the
# "significant area" (pixels > 127) and maps that distance to a grey value.
#
#   Positive scale:  the significant area (> 127) is turned white; every pixel
#                    outside it is assigned  max(0, 255 - distance * scale).
#                    Result: a gradient that fades to black moving away from
#                    the shape edge.  Useful for building soft feathered masks.
#
#   Negative scale:  the image is inverted before and after the computation,
#                    so the gradient runs inward from the edge toward the shape
#                    centre.  Useful for eroding or shrinking a mask.
#
#   scale = 0:       binarises the clip (threshold at 127), no gradient.
#
# PRACTICAL USE IN A DELOGO WORKFLOW
# -----------------------------------
#   # 1. Start from a hard binary mask (e.g. the exact logo bounding box):
#   rough_mask = ...   # Y8, white where logo is
#
#   # 2. Inflate outward by ~4 px to catch blending fringe:
#   inflated = rough_mask.DistanceFunction(scale=255.0/4)
#   inflated = inflated.Binarize(1)   # back to binary
#
#   # 3. Pass the inflated mask to InpaintLogo.
#
#   Alternatively pass the soft (gradient) mask directly — InpaintLogo uses
#   the > 127 threshold internally, so the gradient edge won't affect which
#   pixels are inpainted, but a soft mask is handy for blending the result
#   back with Overlay(..., mask=...).

# ---------------------------------------------------------------------------
# Geometry
# ---------------------------------------------------------------------------
W      = 320
H      = 180
LENGTH = 1        # single frame is enough for a mask demo

RECT_X = 80
RECT_Y = 45
RECT_W = 160
RECT_H = 90

# ---------------------------------------------------------------------------
# Binary mask: white rectangle on black background (Y8)
# Mask levels matter: masks are full range (0 and 255). Use Color_yuv (raw
# Y value), not Color: Color is RGB and is converted to limited-range luma,
# so white would become 235 and black 16.
# ---------------------------------------------------------------------------
rect     = BlankClip(Width=RECT_W, Height=RECT_H, Length=LENGTH, Pixel_Type="Y8", Color_yuv=$FF0000)
mask_bin = rect.AddBorders(RECT_X, RECT_Y, W-RECT_X-RECT_W, H-RECT_Y-RECT_H, color_yuv=$000000)

# ---------------------------------------------------------------------------
# DistanceFunction variants
# ---------------------------------------------------------------------------

# Outward gradient: value reaches 0 at distance 16 px outside the rectangle
dist_outward = mask_bin.DistanceFunction(scale=255.0/16)

# Inward gradient: value reaches 0 at distance 16 px inside the rectangle
dist_inward  = mask_bin.DistanceFunction(scale=-(255.0/16))

# Binary threshold only (scale=0). The input is already 0/255, so the result is identical to mask_bin
dist_binary  = mask_bin.DistanceFunction(scale=0.0)

# ---------------------------------------------------------------------------
# Output: stacked vertically (convert Y8 → YV24 for uniform stacking)
# ---------------------------------------------------------------------------
StackVertical( \
    dist_outward.ConvertToYV24().Subtitle("Outward gradient (scale=255/16)", align=8, size=14), \
    dist_inward.ConvertToYV24().Subtitle("Inward gradient (scale=-255/16)",  align=8, size=14), \
    dist_binary.ConvertToYV24().Subtitle("Binary (scale=0): same as below",   align=8, size=14), \
    mask_bin.ConvertToYV24().Subtitle("Original binary mask",                 align=8, size=14) \
)
