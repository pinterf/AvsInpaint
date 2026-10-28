# AvsInpaint
AvsInpaint plugin for Avisynth and VapourSynth

Copyright (C)  2007, 2008  Wolfgang Boiger, Berlin

Additional work and VapourSynth port by Ferenc Pintér (C)2019-2026

## Requirements

The plugin has two independent front ends. Each one needs only its own host — the two
requirements are alternatives, not a combined list.

### AviSynth front end

- Works with **AviSynth+** and with classic **AviSynth 2.6** (use `LoadCPlugin` there).
  The AviSynth+ planar RGB functions (`avs_is_planar_rgb` / `avs_is_planar_rgba`) are
  optional, so RGBP/RGBAP are available on AviSynth+ only; frame properties (used by
  `ChromaPlacement="auto"`) are optional as well. No newer interface version is needed.
- All API calls used are from the base AviSynth+ C API; none require V8 or later.
- `avisynth_c_plugin_init2` is exported and recognised by AviSynth+ ≥ 3.7.4 (V11) but is
  optional — older hosts fall back to `avisynth_c_plugin_init` automatically.
- AviSynth is loaded dynamically; no import library is needed at build time.
- The AviSynth C API headers are bundled in `avsinpaint/avisynth/`; no separate header
  installation is needed.

### VapourSynth front end

- Requires **VapourSynth V4** at runtime.
- The VS4 headers are bundled in `avsinpaint/vs/` (from VapourSynth r73); no external SDK
  is required to build.

## Build

### Windows — Visual Studio

Open `AvsInPaint.slnx` (Visual Studio 2026) — or, with Visual Studio 2022, the legacy
`AvsInPaint.sln` — and build.
From the command line:

```
devenv AvsInPaint.slnx /Build "Release|x64"     rem Visual Studio 2026
devenv AvsInPaint.sln  /Build "Release|x64"     rem Visual Studio 2022
```

Available configurations: `Debug|Win32`, `Debug|x64`, `Release|Win32`, `Release|x64`,
`RelWithDebInfo|Win32`, `RelWithDebInfo|x64`, `ReleaseClangCL|x64`, `DebugClangCL|x64`,
`ReleaseV143|Win32`, `ReleaseV143|x64`

The project uses the Visual Studio 2026 toolset (v145). `ReleaseV143` is the same as
`Release` but built with the Visual Studio 2022 toolset (v143, MSVC 19.44), for Windows 7
compatibility. With Visual Studio 2026 it needs the "MSVC v143 build tools" component.

Dual-interface configurations (the default) build **both** the AviSynth and the
VapourSynth front end into one DLL — the DLL exports `avisynth_c_plugin_init`,
`avisynth_c_plugin_init2` and `VapourSynthPluginInit2`.

VapourSynth-only configurations (`DebugVSOnly|Win32`, `DebugVSOnly|x64`,
`ReleaseVSOnly|Win32`, `ReleaseVSOnly|x64`) build the VapourSynth front end only:
the AviSynth wrapper and the AviSynth runtime loader are compiled out
(`ENABLE_AVISYNTH` is not defined), so the resulting DLL exports just
`VapourSynthPluginInit2` and does not touch AviSynth at all.

AviSynth+ is loaded dynamically at runtime; no AviSynth import library is needed at build time.

### Windows (MinGW/MSYS2) — CMake

AviSynth+ is loaded dynamically at runtime; no import library is needed at build time.

```sh
cmake -B build -S .
cmake --build build
```

Output: `build/avsinpaint/AvsInPaint.dll`

VapourSynth-only build (no AviSynth headers or runtime required):

```sh
cmake -B build -S . -DENABLE_AVISYNTH=OFF
cmake --build build
```

### Linux — CMake

AviSynth+ is loaded dynamically at runtime; no development package is needed to build.
For AviSynth use, **AviSynth+** (or classic AviSynth 2.6) must be installed on the target system.

```sh
cmake -B build -S .
cmake --build build -j$(nproc)
sudo cmake --install build
```

Output: `build/avsinpaint/libavsinpaint.so`

VapourSynth-only build — no AviSynth headers or runtime required; installs to
`<prefix>/lib/vapoursynth/` instead of `<prefix>/lib/avisynth/`:

```sh
cmake -B build -S . -DENABLE_AVISYNTH=OFF
cmake --build build -j$(nproc)
sudo cmake --install build
```

Install destination: `<prefix>/lib/avisynth/` (follows `GNUInstallDirs`)

### ARM64 (aarch64)

The plugin contains **no SIMD and no architecture specific code** — the pixel loops are
plain C++ and the only platform-dependent code is the AviSynth runtime loader
(`LoadLibrary`/`GetProcAddress` on Windows, `dlopen`/`dlsym` on POSIX). The same
sources therefore build for x86, x64 and ARM64 unchanged.

**Windows on ARM64** — cross-compiled on x64 Windows with
[llvm-mingw](https://github.com/mstorsjo/llvm-mingw) (UCRT flavour), CMake and Ninja,
mirroring the mvtools setup:

```bat
set LLVM_MINGW_ROOT=C:\avsplus_build_deps\llvm-mingw-20261020-ucrt-x86_64
build_arm64_mingw.bat
```

Toolchain file: `cmake/toolchains/llvm-mingw-aarch64.cmake` (libc++ and libunwind are
linked statically, so the DLL depends only on system DLLs and the UCRT).
Output: `out/ARM64/llvm-mingw/AvsInPaint.dll` — exports `avisynth_c_plugin_init`,
`avisynth_c_plugin_init2` and `VapourSynthPluginInit2`.

**Linux on ARM64** — cross-compiled on x86-64 with the Debian/Ubuntu cross toolchain
(`apt-get install g++-aarch64-linux-gnu`):

```sh
./build_aarch64_linux.sh
```

Toolchain file: `cmake/toolchains/aarch64-linux-gnu.cmake`.
Output: `Build/aarch64/avsinpaint/libavsinpaint.so`.

## Demo scripts

The `Documentation` folder contains demo scripts for both hosts. Each AviSynth `.avs` script
has a VapourSynth `.vpy` twin of the same name:

- `Demo` — the original 2008 analyze / deblend / inpaint workflow (RGB24).
- `Demo_YUV` — the same workflow with a YUV (YV12 / YUV420P8) clip.
- `Demo2` — `InpaintLogo`.
- `Demo2_Deblend` — `DeblendLogo`.
- `Demo2_DistanceFunction` — `DistanceFunction`.

The `.vpy` demos use the autoloaded plugin and fall back to the repo's `x64\Release` build.
See the "Demo scripts" section of `Documentation/AVSInpaint.htm` for details.

## Version change list

### v1.4 by pinterf

Developed with AI assistance: most of the work was done with Claude, with help from
other AI tools as well, plus my own coding and ideas and tests.

#### v1.4 in brief

- **VapourSynth V4 support — one DLL, both interfaces.** The plugin is now a dual-interface
  plugin: the same DLL exports the AviSynth entry points (`avisynth_c_plugin_init`,
  `avisynth_c_plugin_init2`) and the VS4 entry point (`VapourSynthPluginInit2`). All four
  filters — `InpaintLogo`, `AnalyzeLogo`, `DeblendLogo`, `DistanceFunction` — are available
  under both hosts. A VapourSynth-only build is possible too (`ENABLE_AVISYNTH=OFF` in CMake,
  `ReleaseVSOnly` / `DebugVSOnly` in Visual Studio): that DLL never touches AviSynth at all.
- **AviSynth is loaded at runtime, not linked.** The static link against `AviSynth.lib` /
  `libAviSynth.so` is gone; the AviSynth C API is loaded with `LoadLibrary`/`dlopen` at
  startup. The plugin DLL has no link-time dependency on AviSynth and can be installed in
  environments where AviSynth is not present.
- **Ported to C++17.** The C source became C++ (`AVSInpaint.cpp`), together with the
  refactoring that came with it: RAII instead of manual `malloc`/`free`, `static_cast`,
  `nullptr`, `enum class`, `inline constexpr`, and the algorithm core split out of the
  AviSynth wrapper into plain functions shared by both front ends. Details are in the
  v1.4 notes below.
- **Bug fixes** — several real ones, listed right below.
- **Much better quality on subsampled YUV (YV12, YV16, YUY2).** Deblending a logo
  (`AnalyzeLogo` + `DeblendLogo`) on 4:2:0/4:2:2 used to leave clearly visible logo/letter
  outlines; they are gone now, and the result is close to YV24/RGB. On the demo the YV12 luma
  error drops from 3.68 (max 99) to 2.42 (max 19), with YV24 at 2.34 (max 19); the alpha
  error drops from 10.8 to 4.9, with YV24 at 4.2. Two changes do this:
  - *Sharper alpha estimate:* `AnalyzeLogo` no longer spreads each chroma block's
    transparency over all of its luma pixels; chroma only corrects the block average of the
    full-resolution estimate.
  - *Chroma siting:* new `ChromaPlacement` argument for `AnalyzeLogo` and `DeblendLogo`
    (`chromaplacement` in VapourSynth). The full-resolution alpha is downsampled at the
    real chroma sample positions (left/MPEG-2, center, top_left, top, bottom_left, bottom, DV)
    instead of a plain block average. The default, `"auto"`, reads `_ChromaLocation` and falls
    back to `"left"`; `"center"` uses the plain block average, as before.

  Results for subsampled YUV therefore change (improve) for every placement, including
  `"center"`. RGB, 4:4:4 and greyscale results are unchanged.
- **Performance** — the hot paths were reworked (compile-time dispatch on the sample step,
  division-free chroma accumulation, `float` instead of `double` in the deblend math);
  roughly +30% fps in the inpainting path, +15% in `Analyze`.

#### Bug fixes in v1.4
- `Deblend`: the premultiplied logo data was kept in a `DoubleByte` struct whose base-255
  split lost up to ±255 in the deblend numerator — now exact `uint16_t` arithmetic over the
  full 0..255×255 range
- `Deblend_Static_GetFrame`: the per-channel loop was bounded by `MaxChannelCount` instead
  of the actual `ChannelCount`, so it touched channels that do not exist
- `CreateGaussKernel`: the C `abs()` was applied to a `double` and silently truncated it to
  an integer — now `std::abs()`
- `Analyze`: the mask and alpha channel vectors were allocated with a hardcoded
  `Width*Height/2` size (a YV16 worst-case approximation) instead of the exact
  per-channel size, which over- or under-allocated for every other subsampling
- The YUV test `(PixelType && AVS_CS_YUV)` used `&&` instead of `&`, so it was always true
  and the YUV-specific path was taken for every colorspace
- `Inpaint_Create`: the colorspace check of `GetFramePointers` was missing the `!ErrorText`
  guard and could overwrite an error message set earlier
- Planar RGB/RGBA: the vertical flip logic assumed a bottom-up RGB layout, but planar RGB
  is top-down — the condition is now `avs_is_rgb() && !avs_is_planar()`

#### AviSynth runtime loader and entry points
- AviSynth is no longer linked at build time; `AvsLoader.cpp` / `AvsLoader.h` load it at runtime via `LoadLibrary`/`dlopen` — the plugin DLL has no link-time dependency on `AviSynth.lib` / `libAviSynth.so` and can be installed in VapourSynth-only environments without AviSynth present
- Both `avisynth_c_plugin_init` and `avisynth_c_plugin_init2` are exported; older hosts call `init`, AviSynth+ ≥ 3.7.4 (V11) calls `init2` to signal that 64-bit data types (`int64`/`double`) are passed natively in `AVS_Value`; sets `g_is_v11_64bit_data_capable_host = true`, reserved for future use when the plugin uses 64-bit parameters

#### VapourSynth V4 support
- Single DLL exports both AviSynth and VapourSynth entry points; `VapourSynthPluginInit2` is the VS4 entry point
- VS4 headers bundled in `avsinpaint/vs/` (from VapourSynth r73); no external SDK required to build
- `InpaintLogo`, `AnalyzeLogo`, `DeblendLogo`, and `DistanceFunction` filters all available for VapourSynth V4
- VS4 `InpaintLogo` always requires a separate mask clip (no embedded-alpha shorthand)
- `InpaintCore.h` introduced as shared header: algorithm constants, `FrameLayout`, `InpaintDataStruct`, `InpaintMaskDataStruct`, and declarations for the API-agnostic core functions
- `DistanceFunctionCore` extracted as pure function (no AVS types); VS4 wrapper added
- `DeblendStaticPrepare` decoupled from AVS API (plain pointer interface); VS4 `DeblendLogo` wrapper added

#### Internal refactor (dual-API groundwork, no functional change for AviSynth)
- `FrameLayout` struct introduced: captures physical video format (`channelCount`, `colorFamily`, `sampleStep[]`, `pixelWidth[]`, `pixelHeight[]`) independently of any API; `FrameLayoutFromPixelType()` fills it from AVS pixel-type constants
- `AvsGetFramePlanes` / `AvsGetFramePlanesRO`: thin AVS helpers concentrating all `avs_get_pitch_p` / `avs_get_read_ptr_p` / `avs_get_write_ptr_p` calls
- `InpaintPrepareMask` decoupled from `AVS_VideoFrame*`; accepts `FrameLayout` + plain pointer arrays; `AvsInpaintPrepareMask` is the thin AVS wrapper
- `InpaintCore` extracted as a pure function (no AVS types); `Inpaint_GetFrame` is the thin AVS wrapper
- `Analyze` decoupled from AVS clips; frame fetching/releasing passed as `std::function` callbacks; `Analyze_Create` provides AVS-specific lambdas
- `stride[]` field renamed to `sampleStep[]` (`stride` is VapourSynth's term for bytes-per-row; `sampleStep` is bytes between adjacent samples within a row)

#### Performance and correctness improvements
- `AddPreBlurValue` refactored: ~+30% fps improvement in the inpainting hot path
- `InpaintPixel` optimized; `GetChannelTensor` refactored
- `DeblendStatic` / `DeblendDynamic`: arithmetic converted from `double` to `float`; reciprocal-multiply replaces per-pixel division
- `DoubleByte` struct replaced with `uint16_t` for static logo premultiplied data — fixes latent approximation error (base-255 split introduced up to ±255 error in the deblend numerator); range is now exact [0..255×255=65025]
- Rounding convention: `std::round` / `std::roundf` replaced with `(int)(x+0.5f)` throughout (valid for non-negative pixel data; compiles to FADD+CVTTSS2SI on x86)
- Bug fix in `Deblend_Static_GetFrame`: per-channel loop used `MaxChannelCount` as bound instead of `ChannelCount`
- `InpaintPrepareMask` hot-path refactored: `ChromaWidth`/`ChromaHeight` and `RowStep` hoisted outside inner loops; redundant `std::fill` calls eliminated (superseded by zero-initializing `resize()`); chiChroma accumulation rewritten from a flat indexed loop (with per-pixel integer division for chroma coordinates) to a 4-level `cy`/`ly`/`x`/`lx` structure that advances the luma pointer in raster order with no divisions; common subsampling factors (1×1 for RGB/4:4:4, 2×1 for 4:2:2/YUY2, 2×2 for 4:2:0) specialized via generic lambda + `std::integral_constant` with `if constexpr` dispatch, giving the compiler compile-time CPW/CPH; `chromaRowPtr` hoisted before the `ly` loop; sum accumulator added in the `lx` reduction to avoid a memory read-modify-write on every step
- `InpaintCoreImpl<LSS,CSS>`, `DistanceFunctionCoreImpl<SS>`, and `InpaintPrepareMaskImpl<SS>` templated on SampleStep; dispatch wrappers (`InpaintCore`, `DistanceFunctionCore`, `InpaintPrepareMask`) resolve the runtime stride to a compile-time constant — `<1,1>` for all planar formats (every VapourSynth frame, and AVS+ planar YUV/RGBP/Y8), `<2,4>` for YUY2, `<3,3>` for RGB24, `<4,4>` for RGB32; for `SS=1` the compiler eliminates every `x * SampleStep` multiply in the inner pixel loops, simplifies the row-advance to `pitch - width`, and can auto-vectorize the byte-scan loops

#### Build and SDK layout
- Visual Studio 2026 `.slnx` solution file added alongside the legacy `.sln`
- `RelWithDebInfo` build configurations added for Win32 and x64 (release optimizations + full PDB, no WholeProgramOptimization)
- `DebugClangCL` and `ReleaseClangCL` configurations added for x64 (`PlatformToolset=ClangCL`); library path via `AdditionalLibraryDirectories` for lld-link compatibility
- AviSynth C API headers moved to `avisynth/` subfolder, mirroring the `vs/` layout for VapourSynth headers; `AVSInpaint.cpp` includes `"AvsLoader.h"`, which pulls in `"avisynth/avisynth_c.h"`
- The `avs/` subdirectory (with `config.h`, `capi.h`, `types.h`, etc.) lives under `avisynth/avs/` and is unmodified SDK content, in sync with AviSynth+ upstream master
- MSVC x64 CMake builds no longer use `AvsInPaint.def`: the `@4`-decorated aliases are a Win32-only (stdcall) artifact, on x64 the exports come from the `dllexport` attributes of the entry points
- VapourSynth-only build fixed and made usable from both build systems: the API-agnostic core lives in `AVSInpaint.cpp` together with the AviSynth wrapper, so the file is now always compiled and the AviSynth-specific parts are guarded by `#ifdef ENABLE_AVISYNTH` (same for `AvsLoader.cpp` / `AvsLoader.h`). CMake: `ENABLE_AVISYNTH=OFF`; Visual Studio: the new `DebugVSOnly` / `ReleaseVSOnly` configurations

#### AviSynth C API header update
- `avisynth_c.h` synced with AviSynth+ upstream master (interface V14): 64-bit integer value type `'l'`, `avisynth_c_plugin_init2` optional entry point, `avs_prop_*` / `avs_set_to_*` / `avs_val_*` API calls, function values (`avs_invoke2`/`avs_invoke3`), standalone property maps
- The dynamic loader is no longer part of `avisynth_c.h` (upstream moved it into a separate `avisynth_c_loader.h`); this project uses its own `AvsLoader.cpp` instead

#### Earlier C→C++17 conversion
- Convert AVSInpaint.c to C++17 (AVSInpaint.cpp)
- C includes replaced with C++ equivalents (`<cstdlib>`, `<cmath>`, `<climits>`, `<cfloat>`, `<cstring>`, `<algorithm>`)
- `typedef struct` → C++ `struct`
- Compile-time constants converted from `#define` to `inline constexpr` where applicable
- `InpaintMask_Result_*` error codes replaced with `enum class InpaintMaskResult : int`
- `int ChromaTensor`, `int FlipMask` in `InpaintDataStruct` and `int FlipAlpha` in `DeblendDataStruct` changed to `bool`
- `FastMarching`, `FMMUpdateCloseValue`, `GetChannelTensor` return `bool`; `InpaintPrepareMask` returns `InpaintMaskResult`
- All `register` keywords removed (deprecated in C++17)
- C-style casts replaced with `static_cast<>()`
- Null pointer literals changed from `0` to `nullptr`
- All math and memory functions qualified with `std::` namespace
- Local `#define` macros inside function bodies replaced with `const` local variables
- Bug fix: `abs()` on `double` in `CreateGaussKernel` replaced with `std::abs()` (C `abs()` was silently truncating to integer)
- Type cleanup: `signed long int` / `unsigned long int` replaced with `int32_t` / `uint32_t` throughout (`long` is 32-bit on Windows but 64-bit on Linux LP64; original author used `long` as "at least 32 bits" — now explicit)
- `Analyze_Sum1Max` / `Analyze_Sum2Max` replaced from `LONG_MAX`/`ULONG_MAX` to `INT32_MAX`/`UINT32_MAX`; `std::qsort` with C comparator callbacks replaced by `std::sort` with lambdas (`SortCompareSum1`, `SortCompareSum2` deleted)
- `const char * ErrorText` (was `char *`): string literals are `const char*` in C++; MSVC correctly rejected the implicit conversion
- `<cstdint>`, `<array>`, `<vector>` added to includes
- `std::malloc`/`std::free` replaced with `std::vector` (RAII) for `Sum1`, `Sum2`, `NMSum1`, `NMSum2`, `ChannelMaskVector`, `ChannelAlphaVector` in `Analyze()`: eliminates 8+ scattered cleanup paths, zero-init loops, and nullptr guard patterns
- `std::malloc`/`std::free` replaced with `std::vector` (RAII) for temporary frame buffers (`chiLuma`, `GchiLuma`, `GchiuLuma`, `chiChroma`, `GchiChroma`, `GchiuChromas`) in `Inpaint_GetFrame()`
- All remaining `std::malloc`/`std::realloc`/`std::free` pairs replaced with `std::vector` (RAII): kernel buffers in `InpaintDataStruct`, static logo/alpha buffers in `DeblendDataStruct`, mask buffers in `InpaintMaskDataStruct` (`chiLuma`, `chiChroma`, `MaskPixelOrder`, `GchiLuma`, `GchiChroma`) and local `DistFcn` in `InpaintPrepareMask`; affected structs converted to classes with constructors; all manual cleanup paths eliminated
- Add planar RGB (`RGBP`) and planar RGBA (`RGBAP`) 8-bit support to `InpaintLogo`, `Deblend`, and `Analyze`: `GetFramePointers` extended with `AVS_CS_RGBP`/`AVS_CS_RGBAP` cases (planes B/G/R/A at channel indices 0/1/2/3, stride=1, no subsampling); planar RGB is top-down so flip logic corrected to use `avs_is_rgb() && !avs_is_planar()` instead of `avs_is_rgb()` throughout
- `ChannelMaskVector` and `ChannelAlphaVector` allocation sizes corrected from hardcoded `Width*Height/2` (worst-case YV16 approximation) to exact `(Width/PixelWidth)*(Height/PixelHeight)` derived from actual channel subsampling
- Local helper variables `PW`, `PH`, `ChWidth`, `ChHeight`, `ChIdx`, `ChRowStart`, `NMCount` introduced in channel loops to eliminate repeated subexpression evaluation
- Performance improvement: ~+15% speedup observed (attributed to inlined `std::sort` comparators vs. `qsort` function pointer indirection, and reduced memory allocation overhead)
- `BYTE` replaced with `uint8_t` throughout `AVSInpaint.cpp` (`BYTE` is a Windows/AviSynth typedef; `uint8_t` is standard and required for future VapourSynth portability)
- `isYUV` and `isChromaSubsampled` bool fields added to `InpaintDataStruct`, set once at init from `PixelType` / `ChromaPixelWidth/Height`; `AVS_CS_YUV` bitmask test and chroma-subsampling check now appear exactly once, fixing a pre-existing `&&` vs `&` bug (`(PixelType&&AVS_CS_YUV)` always evaluated true)
- Validation chains in all four `_Create` functions (`Inpaint_Create`, `Deblend_Create`, `Analyze_Create`, `DistanceFunction_Create`) converted from repeated `if (!(ErrorText || cond))` / `if (!ErrorText && cond)` guards to `else if` chains; parameter-extraction assignments left flat (unconditional); bug fixed in `Inpaint_Create` where `GetFramePointers` colorspace check lacked `!ErrorText` guard

### v1.3 (20211025) by pinterf
- fix: InpaintDelogo: stuck for large (e.g. 1920x1080) frame sizes: Fast Marching process did not converge.
- Add YV16, YV24, Y8 color spaces

### v1.2 (20190705) by pinterf
- Fix broken compatibility with classic Avisynth 2.6 (remove underscore from dll export)

### v1.1 (20190624) by pinterf
- Fix crash in AVSInpaint-2008.02.23, when using mode "Deblend" or "Both"
  (double frame release, revealed when using Avisynth+)
- Add version resource
- Visual Studio 2019 solution
- x64 version
- (no new colorspaces)

## Links

- https://forum.doom9.org/showthread.php?t=133682
- https://forum.doom9.org/showthread.php?p=1877879
- https://forum.doom9.org/showthread.php?t=176860
- http://avisynth.nl/index.php/InpaintFunc
- https://github.com/pinterf/AvsInpaint

## Donate
If you find this software useful and would like to support ongoing development:

[![Buy Me a Coffee](https://img.shields.io/badge/Buy_Me_A_Coffee-FFDD00?style=flat-square&logo=buy-me-a-coffee&logoColor=black)](https://buymeacoffee.com/pinterf)
[![PayPal](https://img.shields.io/badge/PayPal-00457C?style=flat-square&logo=paypal&logoColor=white)](https://paypal.me/pinterf)

Donations go to **Ferenc Pintér**, the active maintainer of this plugin since 2019, and
cover the tooling and testing costs of maintaining it.
