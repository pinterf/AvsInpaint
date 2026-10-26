#pragma once
// AvsLoader.h — Minimal cross-platform runtime loader for the AviSynth C API.
// Include this file instead of "avisynth/avisynth_c.h" in AviSynth wrapper code.
//
// The whole file is AviSynth-specific: it is compiled out entirely when the
// target is built without AviSynth support (ENABLE_AVISYNTH not defined).

#ifdef ENABLE_AVISYNTH

// AVSC_NO_DECLSPEC turns every AVSC_API(ret, name) into
//   typedef ret (AVSC_CC *name_func);
// giving us the function-pointer typedefs without any import-library dependency.
// avisynth_c.h also compiles its built-in AVS_Library / avs_load_library() block
// under #ifdef AVSC_NO_DECLSPEC (Windows only), which needs HMODULE / GetProcAddress.
// Include <windows.h> first so that block compiles; we simply never call avs_load_library.
#define AVSC_NO_DECLSPEC
#include <cstdlib>   // malloc/free used by avs_load_library() in avisynth_c.h
#ifdef _WIN32
#  ifndef WIN32_LEAN_AND_MEAN
#    define WIN32_LEAN_AND_MEAN
#  endif
#  ifndef NOMINMAX
#    define NOMINMAX
#  endif
#  include <windows.h>
#endif
#include "avisynth/avisynth_c.h"

// ---------------------------------------------------------------------------
// Global function pointers — intentionally given the same names as the C API.
// An alternative design (used by x264/x265) stores pointers in a struct and
// calls them as h->func.avs_xxx(...).  That requires every call site to change.
// AVSInpaint.cpp has 100+ scattered AviSynth call sites, so we keep the same
// names here: AVSC_NO_DECLSPEC suppresses the dllimport declarations and
// replaces them with _func typedefs, our globals shadow those names, and
// AVSInpaint.cpp compiles unchanged.
// ---------------------------------------------------------------------------
extern avs_add_function_func        avs_add_function;
extern avs_new_c_filter_func        avs_new_c_filter;
extern avs_take_clip_func           avs_take_clip;
extern avs_get_video_info_func      avs_get_video_info;

extern avs_is_yv12_func             avs_is_yv12;
extern avs_is_yv16_func             avs_is_yv16;
extern avs_is_yv24_func             avs_is_yv24;
extern avs_is_y8_func               avs_is_y8;
// AviSynth+ only; on classic AviSynth 2.6 these are stubs that always return 0.
extern avs_is_planar_rgb_func       avs_is_planar_rgb;
extern avs_is_planar_rgba_func      avs_is_planar_rgba;

extern avs_get_frame_func           avs_get_frame;
extern avs_release_video_frame_func avs_release_video_frame;
extern avs_release_clip_func        avs_release_clip;
extern avs_copy_value_func          avs_copy_value;
extern avs_make_writable_func       avs_make_writable;
extern avs_set_cache_hints_func     avs_set_cache_hints;
extern avs_new_video_frame_a_func   avs_new_video_frame_a;
extern avs_copy_video_frame_func    avs_copy_video_frame;

extern avs_get_pitch_p_func         avs_get_pitch_p;
extern avs_get_read_ptr_p_func      avs_get_read_ptr_p;
extern avs_get_write_ptr_p_func     avs_get_write_ptr_p;
extern avs_set_to_clip_func         avs_set_to_clip;

// Optional (AviSynth+ with frame properties, interface V9+); nullptr on older hosts.
extern avs_get_frame_props_ro_func  avs_get_frame_props_ro;
extern avs_prop_get_int_func        avs_prop_get_int;

// ---------------------------------------------------------------------------
// Set to true when avisynth_c_plugin_init2 is called (AviSynth+ >= 3.7.4):
// the host understands int64/double in AVS_Value natively.
// ---------------------------------------------------------------------------
extern bool g_is_v11_64bit_data_capable_host;

// ---------------------------------------------------------------------------
// Loader — call once from avisynth_c_plugin_init before any API use.
// Returns true on success; false if the library is absent or incomplete.
// Idempotent: safe to call more than once.
// ---------------------------------------------------------------------------
bool avs_loader_init();

// ---------------------------------------------------------------------------
// Inline helpers suppressed by AVSC_NO_DECLSPEC, re-provided here so
// call sites in AVSInpaint.cpp require no changes.
// ---------------------------------------------------------------------------
AVSC_INLINE AVS_Value avs_new_value_clip(AVS_Clip* v0)
    { AVS_Value v; avs_set_to_clip(&v, v0); return v; }

AVSC_INLINE AVS_VideoFrame* avs_new_video_frame(AVS_ScriptEnvironment* env,
                                                  const AVS_VideoInfo*   vi)
    { return avs_new_video_frame_a(env, vi, AVS_FRAME_ALIGN); }

AVSC_INLINE int avs_is_same_colorspace(const AVS_VideoInfo* x,
                                         const AVS_VideoInfo* y)
    { return x->pixel_type == y->pixel_type || (avs_is_yv12(x) && avs_is_yv12(y)); }

#endif  // ENABLE_AVISYNTH
