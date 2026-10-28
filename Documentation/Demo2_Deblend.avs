# Demo2_Deblend.avs — DeblendLogo for AviSynth+
# VapourSynth twin: Demo2_Deblend.vpy.
#
# SETUP
# -----
# Place AVSInpaint.dll in the AviSynth+ plugins folder; it is autoloaded.
#
# WHAT THIS SCRIPT DOES
# ---------------------
# Builds a colour-bar background (8 standard SMPTE bars, YV24 = YUV444P8).
# Alpha-blends a semi-transparent logo rectangle onto it, then runs
# DeblendLogo to mathematically recover the original background.
# Output is a single clip: result / before / ground-truth / logo / alpha mask
# stacked vertically.
#
# HOW DeblendLogo WORKS
# ---------------------
# DeblendLogo is the mathematical inverse of alpha-compositing.  Given:
#   - the composited frame  (clip_with_logo)
#   - the logo colour image  (logo)
#   - the per-pixel alpha    (alpha, Y8 where 0 = transparent, 255 = opaque)
# it solves for the original background pixel:
#       bg = (255 * blended - alpha * logo) / (255 - alpha)
#
# Consequence: pixels where alpha = 255 (fully opaque) make the denominator
# zero and cannot be recovered.  Keep LOGO_ALPHA below 255 (180-220 is a
# realistic TV-station range).
#
# WHEN TO USE DeblendLogo vs InpaintLogo
# ---------------------------------------
#   DeblendLogo  — semi-transparent logo (alpha < 255 everywhere in region).
#                  The original background shines through; algebra undoes it.
#
#   InpaintLogo  — fully opaque logo (alpha = 255).
#                  No information from the background survives; pixels must be
#                  reconstructed from neighbours by interpolation.
#
#   Real logos often combine both: semi-transparent body (DeblendLogo) with
#   an opaque core or hard outline (InpaintLogo on the remaining pixels).
#   See Demo2.avs for the InpaintLogo step.
#
# COLOUR FORMAT NOTES
# -------------------
# Color_YUV is a packed integer $YYUUVV (each component 8-bit hex).
# Limited range: Y=16..235, U/V=16..240, neutral chroma = 128 ($80).
# Logo colour: Y=210 U=60 V=210  $D23CD2  (pinkish-purple, visible on all bars)

# ---------------------------------------------------------------------------
# Geometry
# ---------------------------------------------------------------------------
W      = 320
H      = 200
LOGO_X = 20
LOGO_Y = 20
LOGO_W = W - 2*LOGO_X   # 280
LOGO_H = 60
LENGTH = 100

# Alpha opacity: 0 = invisible, 255 = fully opaque.
# DeblendLogo cannot recover pixels where alpha = 255.
LOGO_ALPHA = 180

# ---------------------------------------------------------------------------
# Background: horizontal colour bars (YV24, limited range)
# ---------------------------------------------------------------------------
bar_w   = W / 8

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
# Logo colour patch (the pure logo image before blending)
# ---------------------------------------------------------------------------
logo_patch = BlankClip(Width=LOGO_W, Height=LOGO_H, Length=LENGTH, Pixel_Type="YV24", Color_YUV=$D23CD2)
logo       = logo_patch.AddBorders(LOGO_X, LOGO_Y, W-LOGO_X-LOGO_W, H-LOGO_Y-LOGO_H, color_yuv=$108080)

# ---------------------------------------------------------------------------
# Alpha mask: uniform opacity within the logo rectangle (Y8)
# DeblendLogo reads plane 0 of the Alpha clip, so Y8 is the natural choice.
# Mask levels matter: alpha is full range, 0 = transparent, 255 = opaque.
# Use Color_yuv (raw Y value), not Color: Color is RGB and is converted to
# limited-range luma, so Color=0 would give Y=16 (a 6% alpha everywhere).
# ---------------------------------------------------------------------------
alpha_patch = BlankClip(Width=LOGO_W, Height=LOGO_H, Length=LENGTH, Pixel_Type="Y8", Color_yuv=LOGO_ALPHA*65536)
alpha       = alpha_patch.AddBorders(LOGO_X, LOGO_Y, W-LOGO_X-LOGO_W, H-LOGO_Y-LOGO_H, color_yuv=$000000)

# ---------------------------------------------------------------------------
# Composite: alpha-blend logo onto background
#   Overlay(clipa, clipb, mask=m) = clipa*(1-m/255) + clipb*(m/255)
#   (same as VapourSynth's std.MaskedMerge; YV24 is blended natively)
# ---------------------------------------------------------------------------
clip_with_logo = Overlay(bg, logo, mask=alpha)

# ---------------------------------------------------------------------------
# DeblendLogo: recover the background
# Signature: DeblendLogo(clip, logo, Alpha=alpha)
# ---------------------------------------------------------------------------
result = clip_with_logo.DeblendLogo(logo, Alpha=alpha)

# ---------------------------------------------------------------------------
# Output: stacked vertically
#   Row 1 — deblended result   (should match row 3 closely)
#   Row 2 — clip with logo     (input to DeblendLogo)
#   Row 3 — clean background   (ground truth — not available in real workflow)
#   Row 4 — logo colour patch  (what was blended in)
#   Row 5 — alpha mask
# ---------------------------------------------------------------------------
StackVertical( \
    result.Subtitle("After DeblendLogo",          align=8, size=14), \
    clip_with_logo.Subtitle("Before (blended)",   align=8, size=14), \
    bg.Subtitle("Ground truth (clean bg)",         align=8, size=14), \
    logo.Subtitle("Logo colour patch",             align=8, size=14), \
    alpha.ConvertToYV24().Subtitle("Alpha mask",   align=8, size=14) \
)
