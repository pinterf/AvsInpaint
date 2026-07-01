CMAKE_MINIMUM_REQUIRED( VERSION 3.8.2 )

set(PluginName "AvsInPaint")

if (NOT WIN32)
  string(TOLOWER "${PluginName}" PluginName)
endif()

set(ProjectName "${PluginName}")
project(${ProjectName} LANGUAGES CXX)

set(AvsInPaint_Sources
  AVSInpaint.cpp
  AvsLoader.cpp
  VSSInpaint.cpp
)

# MSVC: include the module-definition file (export name aliases) and version resource.
# MinGW: the DEF is NOT used — --enable-stdcall-fixup (set in root CMakeLists) handles
# stdcall decoration stripping automatically, and the DEF's _avisynth_c_plugin_init@4
# internal symbol doesn't exist in 64-bit MinGW builds.
if(MSVC)
  list(APPEND AvsInPaint_Sources AvsInPaint.def)
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

INSTALL(TARGETS ${ProjectName}
        LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}/avisynth")
