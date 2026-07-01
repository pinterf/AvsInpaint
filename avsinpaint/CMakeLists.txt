CMAKE_MINIMUM_REQUIRED( VERSION 3.8.2 )

set(PluginName "AvsInPaint")

if (NOT WIN32)
  string(TOLOWER "${PluginName}" PluginName)
endif()

set(ProjectName "${PluginName}")
project(${ProjectName} LANGUAGES CXX)

option(ENABLE_AVISYNTH "Build AviSynth interface (requires bundled avisynth/ headers)" ON)

set(AvsInPaint_Sources
  VSSInpaint.cpp
)

if(ENABLE_AVISYNTH)
  list(APPEND AvsInPaint_Sources
    AVSInpaint.cpp
    AvsLoader.cpp
  )
endif()

# MSVC: include the module-definition file (export name aliases) and version resource.
# DEF is only needed for AviSynth — it aliases the Win32 stdcall-decorated entry points.
# VapourSynthPluginInit2 is __cdecl and needs no alias.
# MinGW: the DEF is NOT used — --enable-stdcall-fixup (set in root CMakeLists) handles
# stdcall decoration stripping automatically, and the DEF's _avisynth_c_plugin_init@4
# internal symbol doesn't exist in 64-bit MinGW builds.
if(MSVC)
  if(ENABLE_AVISYNTH)
    list(APPEND AvsInPaint_Sources AvsInPaint.def)
  endif()
  list(APPEND AvsInPaint_Sources AvsInPaint.rc)
endif()

add_library(${PluginName} SHARED ${AvsInPaint_Sources})

set_target_properties(${PluginName} PROPERTIES "OUTPUT_NAME" "${PluginName}")
if(MINGW)
  set_target_properties(${PluginName} PROPERTIES PREFIX "")
  set_target_properties(${PluginName} PROPERTIES IMPORT_PREFIX "")
endif()

# Automatically group source files according to directory structure
foreach(FILE ${AvsInPaint_Sources})
  get_filename_component(PARENT_DIR "${FILE}" PATH)

  string(REGEX REPLACE "(\\./)" "" GROUP "${PARENT_DIR}")
  string(REPLACE "/" "\\" GROUP "${GROUP}")

  if ("${FILE}" MATCHES ".*\\.cpp")
    set(GROUP "Source Files\\${GROUP}")
  elseif("${FILE}" MATCHES ".*\\.h")
    set(GROUP "Header Files\\${GROUP}")
  endif()

  source_group("${GROUP}" FILES "${FILE}")
endforeach()

# Headers in the source dir are included via relative paths (e.g. "avisynth/avisynth_c.h")
target_include_directories(${ProjectName} PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

# AviSynth is loaded at runtime via LoadLibrary/dlopen — no import library needed.
# Windows: LoadLibrary/GetProcAddress are part of the Win32 API, no extra link target.
# Linux/macOS: dlopen/dlsym require libdl (linked below via the "dl" target).
if(MSVC OR MINGW)
  target_link_libraries(${ProjectName} "uuid" "winmm" "vfw32" "msacm32" "gdi32" "user32" "advapi32" "ole32" "imagehlp")
else()
  target_link_libraries(${ProjectName} "dl")
endif()

include(GNUInstallDirs)

if(ENABLE_AVISYNTH)
  set(AVSINPAINT_INSTALL_DIR "${CMAKE_INSTALL_LIBDIR}/avisynth")
else()
  set(AVSINPAINT_INSTALL_DIR "${CMAKE_INSTALL_LIBDIR}/vapoursynth")
endif()

INSTALL(TARGETS ${ProjectName}
        LIBRARY DESTINATION "${AVSINPAINT_INSTALL_DIR}")
