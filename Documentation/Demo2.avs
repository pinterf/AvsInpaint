# Demo2.avs — InpaintLogo for AviSynth+
# VapourSynth twin: Demo2.vpy.
#
# SETUP
# -----
# Place AVSInpaint.dll in the AviSynth+ plugins folder; it is autoloaded.
#
# WHAT THIS SCRIPT DOES
# ---------------------
# Builds a colour-bar background (8 standard SMPTE bars, YV24 = YUV444P8
# 8-bit limited range).  Pastes a bright solid rectangle at the upper edge
# to simulate an opaque TV-station bug, then runs InpaintLogo to reconstruct
# the covered pixels.
# Output is a single clip: result / before / mask stacked vertically.
#
# COLOUR FORMAT NOTES
# -------------------
# Pixel_Type "YV24" is AviSynth's name for 8-bit 4:4:4 YUV (= YUV444P8).
# Color_YUV is a packed integer $YYUUVV (each component 8-bit hex).
# Limited range: Y=16..235, U/V=16..240, neutral chroma = 128 ($80).
#   $EB8080  Y=235 U=128 V=128  white
#   $108080  Y= 16 U=128 V=128  black  (used as neutral border fill)
# Logo colour: Y=210 U=60 V=210  $D23CD2  (pinkish-purple, visible on all bars)
#
# NOTES
# -----
# - The logo sits entirely within the first (white) bar, so the inpainter
#   reconstructs a clean white region — predictable and easy to verify.
# - The mask is the exact bounding box of the logo.  For a real workflow
#   inflate it slightly with DistanceFunction to catch blending fringe;
#   see Demo2_DistanceFunction.avs.
# - For semi-transparent logos use DeblendLogo first; see Demo2_Deblend.avs.
# - When using chroma-subsampled formats (YV12 / YUV420, YV16 / YUV422):
#   the chroma planes are inpainted at reduced resolution (half in one or
#   both directions).  For YV24/YUV444 and YV16/YUV422 the inpainting in
#   this demo recovers the exact bar colours; for YV12 the chroma result
#   may differ by a few counts in the inpainted region.  This is inherent
#   to 4:2:0 chroma subsampling — not a regression or plugin bug — and
#   is consistent with the original 2008 plugin behaviour.

# ---------------------------------------------------------------------------
# Geometry
# ---------------------------------------------------------------------------
W      = 320
H      = 200
LOGO_X = 20
LOGO_Y = 20
LOGO_W = W - 2*LOGO_X   # 280  — spans full inner width for visibility
LOGO_H = 60
LENGTH = 100

# ---------------------------------------------------------------------------
# Background: horizontal colour bars (YV24, limited range)
# ---------------------------------------------------------------------------
bar_w   = W / 8   # 40 px per bar; 8 bars × 40 = 320

bar_white   = BlankClip(Width=bar_w, Height=H, Length=LENGTH, Pixel_Type="YV24", Color_YUV=$EB8080)
bar_yellow  = BlankClip(Width=bar_w, Height=H, Length=LENGTH, Pixel_Type="YV24", Color_YUV=$DB108A)
bar_cyan    = BlankClip(Width=bar_w, Height=H, Length=LENGTH, Pixel_Type="YV24", Color_YUV=$BC9A10)
bar_green   = BlankClip(Width=bar_w, Height=H, Length=LENGTH, Pixel_Type="YV24", Color_YUV=$AD2A1A)
bar_magenta = BlankClip(Width=bar_w, Height=H, Length=LENGTH, Pixel_Type="YV24", Color_YUV=$4ED6E6)
bar_red     = BlankClip(Width=bar_w, Height=H, Length=LENGTH, Pixel_Type="YV24", Color_YUV=$3F66F0)
bar_blue    = BlankClip(Width=bar_w, Height=H, Length=LENGTH, Pixel_Type="YV24", Color_YUV=$20F076)
bar_black   = BlankClip(Width=bar_w, Height=H, Length=LENGTH, Pixel_Type="YV24", Color_YUV=$108080)

bg = StackHorizontal(bar_white, bar_yellow, bar_cyan, bar_green, \
                     bar_magenta, bar_red, bar_blue, bar_black)

# ---------------------------------------------------------------------------
# Logo: bright solid rectangle — AddBorders(left, top, right, bottom, ...)
# ---------------------------------------------------------------------------
logo_patch = BlankClip(Width=LOGO_W, Height=LOGO_H, Length=LENGTH, Pixel_Type="YV24", Color_YUV=$D23CD2)
logo_full  = logo_patch.AddBorders(LOGO_X, LOGO_Y, W-LOGO_X-LOGO_W, H-LOGO_Y-LOGO_H, color_yuv=$108080)

# Mask: Y8, white (255) over the logo, black (0) everywhere else
# Mask levels matter: masks are full range. Use Color_yuv (raw Y value), not
# Color: Color is RGB and is converted to limited-range luma (255 -> 235, 0 -> 16).
mask_patch = BlankClip(Width=LOGO_W, Height=LOGO_H, Length=LENGTH, Pixel_Type="Y8", Color_yuv=$FF0000)
mask       = mask_patch.AddBorders(LOGO_X, LOGO_Y, W-LOGO_X-LOGO_W, H-LOGO_Y-LOGO_H, Color_yuv=$000000)

# Composite: paste logo over background
# Overlay blends YV24 natively (no RGB round trip) and accepts a Y8 mask;
# it is the AviSynth equivalent of VapourSynth's std.MaskedMerge.
clip_with_logo = Overlay(bg, logo_full, mask=mask)

# ---------------------------------------------------------------------------
# InpaintLogo
# ---------------------------------------------------------------------------
result = clip_with_logo.InpaintLogo(Mask=mask, Radius=5.0, Sharpness=25.0, \
                                    PreBlur=1.0, PostBlur=2.0)

# ---------------------------------------------------------------------------
# Output: stacked vertically
# ---------------------------------------------------------------------------
StackVertical( \
    result.Subtitle("After InpaintLogo", align=8, size=14), \
    clip_with_logo.Subtitle("Before (with logo)", align=8, size=14), \
    mask.ConvertToYV24().Subtitle("Mask", align=8, size=14) \
)
