// The loader is only needed for the AviSynth wrapper: compiled out entirely
// when the target is built without AviSynth support (ENABLE_AVISYNTH not defined).
#ifdef ENABLE_AVISYNTH

#include "AvsLoader.h"

// ---------------------------------------------------------------------------
// Platform-specific library handle and symbol-lookup helpers
// ---------------------------------------------------------------------------
#ifdef _WIN32
#  include <windows.h>
   static HMODULE s_hLib = nullptr;
   static bool lib_open()
       { s_hLib = LoadLibraryA("avisynth"); return s_hLib != nullptr; }
   static FARPROC lib_sym(const char* name)
       { return GetProcAddress(s_hLib, name); }
#else
#  include <dlfcn.h>
   static void* s_hLib = nullptr;
   static void* lib_sym(const char* name) { return dlsym(s_hLib, name); }
#  ifdef __APPLE__
   static bool lib_open()
       { s_hLib = dlopen("libavisynth.dylib", RTLD_NOW | RTLD_LOCAL); return s_hLib != nullptr; }
#  else
   static bool lib_open()
       { s_hLib = dlopen("libavisynth.so",    RTLD_NOW | RTLD_LOCAL); return s_hLib != nullptr; }
#  endif
#endif

// ---------------------------------------------------------------------------
// Global function pointer definitions (initialised to nullptr)
// ---------------------------------------------------------------------------
avs_add_function_func        avs_add_function        = nullptr;
avs_new_c_filter_func        avs_new_c_filter        = nullptr;
avs_take_clip_func           avs_take_clip           = nullptr;
avs_get_video_info_func      avs_get_video_info      = nullptr;
avs_is_yv12_func             avs_is_yv12             = nullptr;
avs_is_yv16_func             avs_is_yv16             = nullptr;
avs_is_yv24_func             avs_is_yv24             = nullptr;
avs_is_y8_func               avs_is_y8               = nullptr;
avs_is_planar_rgb_func       avs_is_planar_rgb       = nullptr;
avs_is_planar_rgba_func      avs_is_planar_rgba      = nullptr;
avs_get_frame_func           avs_get_frame           = nullptr;
avs_release_video_frame_func avs_release_video_frame = nullptr;
avs_release_clip_func        avs_release_clip        = nullptr;
avs_copy_value_func          avs_copy_value          = nullptr;
avs_make_writable_func       avs_make_writable       = nullptr;
avs_set_cache_hints_func     avs_set_cache_hints     = nullptr;
avs_new_video_frame_a_func   avs_new_video_frame_a   = nullptr;
avs_copy_video_frame_func    avs_copy_video_frame    = nullptr;
avs_get_pitch_p_func         avs_get_pitch_p         = nullptr;
avs_get_read_ptr_p_func      avs_get_read_ptr_p      = nullptr;
avs_get_write_ptr_p_func     avs_get_write_ptr_p     = nullptr;
avs_set_to_clip_func         avs_set_to_clip         = nullptr;
avs_get_frame_props_ro_func  avs_get_frame_props_ro  = nullptr;
avs_prop_get_int_func        avs_prop_get_int        = nullptr;

bool g_is_v11_64bit_data_capable_host = false;

// ---------------------------------------------------------------------------
// avs_loader_init
// ---------------------------------------------------------------------------
bool avs_loader_init()
{
    if (avs_add_function)  return true;   // already loaded — idempotent guard
    if (!lib_open())       return false;

#define LOAD(fn)  fn = (fn##_func)lib_sym(#fn);  if (!fn)  return false
    LOAD(avs_add_function);
    LOAD(avs_new_c_filter);
    LOAD(avs_take_clip);
    LOAD(avs_get_video_info);
    LOAD(avs_is_yv12);
    LOAD(avs_is_yv16);
    LOAD(avs_is_yv24);
    LOAD(avs_is_y8);
    LOAD(avs_is_planar_rgb);
    LOAD(avs_is_planar_rgba);
    LOAD(avs_get_frame);
    LOAD(avs_release_video_frame);
    LOAD(avs_release_clip);
    LOAD(avs_copy_value);
    LOAD(avs_make_writable);
    LOAD(avs_set_cache_hints);
    LOAD(avs_new_video_frame_a);
    LOAD(avs_copy_video_frame);
    LOAD(avs_get_pitch_p);
    LOAD(avs_get_read_ptr_p);
    LOAD(avs_get_write_ptr_p);
    LOAD(avs_set_to_clip);
#undef LOAD
    avs_get_frame_props_ro = (avs_get_frame_props_ro_func)lib_sym("avs_get_frame_props_ro");
    avs_prop_get_int       = (avs_prop_get_int_func)lib_sym("avs_prop_get_int");
    if (!avs_get_frame_props_ro || !avs_prop_get_int)  avs_get_frame_props_ro = nullptr, avs_prop_get_int = nullptr;
    return true;
    // s_hLib is intentionally not closed: releasing the library handle while
    // any code still references its symbols causes undefined behaviour, and
    // the OS reclaims all handles anyway at process exit.
}

#endif  // ENABLE_AVISYNTH
