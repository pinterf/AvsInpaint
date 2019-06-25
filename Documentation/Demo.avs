# This file is part of
#        Logo Inpainting for AviSynth by Wolfgang Boiger
# Copyright (C)  2007, 2008  Wolfgang Boiger, Berlin
# 
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# version 2, as published by the Free Software Foundation in
# June 1991 (see 'GNUGPLv2.txt').
# 
# There is no guaranty that the rights of third parties
# (including software patents) are not infringed upon by the
# execution or distribution of this program.
# Referring to section 8 of the General Public License, any use
# of this program (including all activities mentioned in
# section 0 and including execution) is restricted to countries
# where any of these activities do not infringe upon any such
# rights of third parties. It lies with the user to verify the
# compliance of his or her use (especially concerning but not
# limited to distribution) of this program with the rights of
# any third parties in the respective countries or regions.
# 
# This program is distributed in the hope that it will be
# useful, but WITHOUT ANY WARRANTY; without even the implied
# warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
# PURPOSE.  See the GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public
# License along with this program; if not, write to the
#   Free Software Foundation, Inc.,
#   51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

# This file provides some demos for this plugin. It is intended
# to show what the plugin can do and demonstrate the effects of
# the different functions, not to teach a beginner how to invoke
# the plugin.



# Set some variables for the output clip
OutputSize = 192  # Several clips will be stacked toghether - this is the width and height of each of those
FontSize = 13  # Each clip will have some description on it
OutputVerbosity = 2  # -1, 0, 1, 2 allowed

# Set the properties of the test clip here
Size = 80  # Width and height of test clip
AnalyzeHalo = 2  # Additional size of the region (around the real logo)
                 # where we tell the Analyze function to find the logo
                 # In real application, you would draw this region by hand

# Here you can set some parameters similar to the DeLogo filter
DeblendFalloff = Size/8.0
AlphaToRepair = 20
RepairRadius = 2
# ...and some inpainting parameters
InpaintRadius = Size/12.0  # Similar to "Depth"
InpaintSharpness = 25.0
InpaintPreBlur = Size/50.0
InpaintPostBlur = Size/30.0



# Load the Logo Inpainting plugin
LoadCPlugin("AVSInpaint.dll")



# Creates a funny colorfull clip
# Beware: This function uses the "OL_?_Offset" variables
Function  CreateExampleClip(int S)
{
  wy = 0.23
  wx = 0.3
  ax = 40.0
  vx = 0.1*S
  BlankClip(Width=1,Height=1,Length=1,Pixel_Type="RGB32")
  KillAudio
  cR=last.BlankClip(Color=Color_Red)
  cY=last.BlankClip(Color=Color_Yellow)
  cG=last.BlankClip(Color=Color_Green)
  cC=last.BlankClip(Color=Color_Cyan)
  cB=last.BlankClip(Color=Color_Blue)
  cM=last.BlankClip(Color=Color_Magenta)
  cW=last.BlankClip(Color=Color_White)
  cK=last.BlankClip(Color=Color_Black)
  StackVertical(
  \ StackHorizontal(cR,cW,cG,cK,cB,cW,cR),
  \ StackHorizontal(cK,cY,cW,cC,cK,cM,cW),
  \ StackHorizontal(cR,cW,cB,cK,cG,cW,cR),
  \ StackHorizontal(cW,cM,cK,cC,cW,cY,cK)
  \ )
  StackHorizontal(FlipVertical)
  BilinearResize(16*32,32) 
  Layer(Invert.Mask(last.BlankClip(Color=Color_White).Subtitle(VersionString,Size=floor(27.8*Width/512),Text_Color=Color_White).Invert),"add",256)
  BilinearResize(64*S,2*S)
  # A colorfull version clip
  Loop(Width)
  last.BlankClip(Width=Height/2,Height=Height/2).Overlay(last,0,-Height/4,PC_Range=true)
  FrameEvaluate("OL_X_Offset=-Round("+String(1.0*vx)+"*current_frame+"+String(1.0*ax)+"*Sin("+String(1.0*wx)+"*current_frame))")
  FrameEvaluate("OL_Y_Offset=Round("+String(0.5*Height)+"*Cos("+String(1.0*wy)+"*current_frame))")
  Trim(0,-Round((FrameCount-Width-ax)/vx))
}

# Creates a logo with opaque and transparent regions
Function CreateExampleLogo(int S)
{
  BlankClip(Width=2,Height=2,Length=1,Pixel_Type="RGB32")
  KillAudio
  StackVertical(
  \ last.BlankClip(Height=1,Color=Color_Black),
  \ last.BlankClip(Height=2,Color=Color_White),
  \ last.BlankClip(Height=1,Color=Color_Black)
  \ )
  BilinearResize(S,S)
  last.Mask(last.BlankClip.Subtitle("fancy",Text_Color=Color_White,Halo_Color=Color_White,size=Round(2.7*S/8.0),x=Round(9.0*S/80.0),y=Round(8.0*S/80.0)).Subtitle("logo",Text_Color=Color_White,Halo_Color=Color_White,size=Round(3.0*S/8.0),x=Round(10.0*S/80.0),y=Round(36.0*S/80.0)))
  last.BlankClip.Layer(last,"add")
  StackHorizontal(last.BlankClip(Width=1),last.BlankClip(Width=1,Color=Color_White)).BilinearResize(Width,Height).Mask(last)
}

global FontSize = FontSize
global OutputSize = OutputSize
# Prepares clip for output
Function LabelClip(clip Clip, string Top, string Bot)
{
  Clip
  PointResize(OutputSize,OutputSize)
  Subtitle(Top,size=FontSize,x=0,y=FontSize/2)
  Subtitle(Bot,size=FontSize,x=0,y=Height-3*FontSize/2)
}



# Prepare example clip
ReferenceClip = CreateExampleClip(Size)
CreateExampleLogo(Size)
Mask4Analyze = ShowAlpha.Levels(0,1,1,0,255).ConvertToYV12(Matrix="PC.601").DistanceFunction(10).Levels(255-AnalyzeHalo*10,1,255-AnalyzeHalo*10+1,0,255,false).Greyscale
ReferenceLogo = last
ReferenceClip.Layer(last)
Clip = ConvertToRGB24


# Status here:
#   Clip = RGB video clip with anoying logo inside
#   Mask4Analyze = A b/w (YV12) mask of where we (could have) located the logo approximately


################################################################
## Most of this file is uninteresting stuff needed to prepare ##
## the demo clips or compose a nice output clip out of the    ##
## results. The following lines are more interesting, as the  ##
## "AnalyzeLogo" function is invoked here.                    ##
################################################################

AnalyzeFrameCount = -1
try{                                                             # Analyze takes some time, so we only do it once and store the result in a file
  ImageSource("DemoAnalyzeResult%06d.ebmp",0,0)
                                                                 # This line checks if the logo size has changed
  (Interleave(AssumeFPS(Clip.FrameRate),Clip.Trim(0,-2).AnalyzeLogo(Mask4Analyze)).FrameCount>-10)?last:last
}catch(dummy){
  Clip
  #SelectEvery(8,0)                                              # You can save some CPU time by analyzing only parts of the clip
  last.AnalyzeLogo(Mask4Analyze)                                 # We analyze the logo, the logo and the alpha mask are returned
  AnalyzeFrameCount = FrameCount                                 # Analyze returns the number of frames processed as frame count of the resulting clip
  Trim(0,-1)                                                     # All frames are equal - so we need only one frame
  ImageWriter("DemoAnalyzeResult",0,1,"ebmp")                    # Frame is written to hard disk (see above)
}
AssumeFPS(Clip.FrameRate)
LogoColor = Crop(0,0,0,Height/2)                                 # The color map of the logo is in the top half of the Analyze result
LogoAlpha = Crop(0,Height/2,0,0).ConvertToYV12(Matrix="PC.601")  # The alpha channel is in the bottom half


# Prepare Deblend mask (will be applied at the very end)
Mask4Analyze
DistanceFunction(255.0/DeblendFalloff)
DeblendMask = last

# Prepare repair mask (marks pixels that cannot be deblended)
LogoAlpha
Invert
Levels(AlphaToRepair,1,AlphaToRepair+1,255,0,false)
(RepairRadius>0.1)?DistanceFunction(127.0/RepairRadius):last
Levels(127,1,128,0,255,false)
Greyscale
RepairMask = last


################################################################
## These lines are also interesting as both, the transparent  ##
## parts of the logo (with "DeblendLogo") and the opaque      ##
## parts (with InpaintLogo) are removed.                      ##
################################################################
# Let's go...
Clip
last.DeblendLogo(LogoColor,LogoAlpha)  # The transparent parts of the logo are removed
DeblendedClip = last
last.InpaintLogo(RepairMask,Radius=InpaintRadius,Sharpness=InpaintSharpness,PreBlur=InpaintPreBlur,PostBlur=InpaintPostBlur)
RepairedClip = last
# This step is done by Karel Suhajda's DeLogo to remove some dirt created by wrong estimated alpha masks.
# It is not needed for this example as our clip is not embedded in some bigger movie!!
# I only left it to resemble the DeLogo behaviour more precisely
Clip.ConvertToRGB32.Layer(last.ConvertToRGB32.Mask(DeblendMask.ConvertToRGB32(Matrix="PC.601")))
ConvertToRGB24
FinalClip = last



# Prepare output
ReferenceClip = ReferenceClip.ConvertToRGB24
ReferenceLogoColor = ReferenceLogo.ConvertToRGB24
ReferenceLogoAlpha = ReferenceLogo.ShowAlpha.ConvertToRGB24
Mask4Analyze = Mask4Analyze.ConvertToRGB24(Matrix="PC.601")
Clip = Clip.ConvertToRGB24
LogoAlpha = LogoAlpha.ConvertToRGB24(Matrix="PC.601")
DeblendMask = DeblendMask.ConvertToRGB24(Matrix="PC.601")
RepairMask = RepairMask.ConvertToRGB24(Matrix="PC.601")

AllMasks           = LabelClip(Interleave(Mask4Analyze.LabelClip("","Analyze Mask"),ReferenceLogoAlpha.LabelClip("","Logo Alpha Mask"),LogoAlpha.LabelClip("","Estm. Alpha Mask"),RepairMask.LabelClip("","Repair Mask"),DeblendMask.LabelClip("","Deblend Mask")).Loop(Clip.FrameCount/5).AssumeFPS(Clip.FrameRate),"All Masks (alternating)","")
Clip               = LabelClip(Clip,"Clip with Logo","We want to remove the logo")
ReferenceClip      = LabelClip(ReferenceClip,"Clean test clip ("+String(ReferenceClip.Width)+"x"+String(ReferenceClip.Height)+")","You wouldn't have this in reality!")
ReferenceLogoColor = LabelClip(ReferenceLogoColor,"Logo (color part)","You wouldn't have this in reality!")
ReferenceLogoAlpha = LabelClip(ReferenceLogoAlpha,"Logo (alpha mask)","You wouldn't have this in reality!")
Mask4Analyze       = LabelClip(Mask4Analyze,"Analyze Mask","You would draw this by hand")
LogoColor          = LabelClip(LogoColor,"Estimated Logo (color part)","as determined by Analyze ("+((AnalyzeFrameCount>0)?(String(AnalyzeFrameCount)+" frames used)"):"fetched from file)"))
LogoAlpha          = LabelClip(LogoAlpha,"Estimated Logo (alpha mask)","as determined by Analyze ("+((AnalyzeFrameCount>0)?(String(AnalyzeFrameCount)+" frames used)"):"fetched from file)"))
DeblendMask        = LabelClip(DeblendMask,"Deblend Mask","(applied at the very end)")
RepairMask         = LabelClip(RepairMask,"Repair Mask","where InpaintLogo is applied")
DeblendedClip      = LabelClip(DeblendedClip,"Deblended Clip","transparent parts of logo removed")
RepairedClip       = LabelClip(RepairedClip,"Repaired Clip","opaque parts of logo inpainted")
FinalClip          = LabelClip(FinalClip,"Final Clip","after the Deblend Mask was applied")
NoClip = Clip.BlankClip

RepairedClip
(OutputVerbosity<0)?last:FinalClip
(OutputVerbosity<1)?last:StackHorizontal(Clip,LogoColor,LogoAlpha,DeblendedClip,RepairedClip)
(OutputVerbosity<2)?last:StackHorizontal(StackVertical(ReferenceClip,ReferenceLogoColor,ReferenceLogoAlpha),\
                                         StackVertical(Mask4Analyze,DeblendMask,AllMasks),\
                                         StackVertical(Clip,LogoColor,LogoAlpha),\
                                         StackVertical(DeblendedClip,RepairMask,RepairedClip),\
                                         StackVertical(NoClip,FinalClip,NoClip))

# Add copyright notice
CB = Trim(0,-1).Loop(Round(FrameRate))
(OutputVerbosity<2)?last:CB.Levels(0,1,255,0,47).ScriptClip("""InpaintLogo(Mask=ConvertToYV12,Steps=-9)""").Trim(0,-1).Subtitle("Logo Inpainting copyright notice (with error notice, which may be ignored)",align=2).Loop(5*CB.FrameCount).Dissolve(CB,CB.FrameCount-1)+last
