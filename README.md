# AvsInpaint
AvsInpaint plugin for Avisynth

Copyright (C)  2007, 2008  Wolfgang Boiger, Berlin
Some fixes by pinterf (C)2019-2021

v1.3 (20211025) by pinterf
- fix: InpaintDelogo: stuck for large (e.g. 1920x1080) frame sizes: Fast Marching process did not converge.
- Add YV16, YV24, Y8 color spaces

v1.2 (20190705) by pinterf
- Fix broken compatibility with classic Avisynth 2.6 (remove underscore from dll export)

v1.1 (20190624) by pinterf
- Fix crash in AVSInpaint-2008.02.23, when using mode "Deblend" or "Both"
  (double frame release, revealed when using Avisynth+)
- Add version resource
- Visual Studio 2019 solution
- x64 version
- (no new colorspaces)

Links:
https://forum.doom9.org/showthread.php?t=133682
https://forum.doom9.org/showthread.php?p=1877879
https://forum.doom9.org/showthread.php?t=176860
http://avisynth.nl/index.php/InpaintFunc
https://github.com/pinterf/AvsInpaint
