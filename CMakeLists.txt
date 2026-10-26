# Tested generators:
# Command line makefile generator
# "MinGW Makefiles": MSYS2/Mingw64 GCC build
# IDE project file generators
# "Visual Studio 17 2022" optional platform generator Win32 and x64
# "Visual Studio 17 2022" optional platform generator Win32 and x64. optional toolset "clangcl"
# "Visual Studio 18 2026" optional platform generator Win32 and x64
# "Visual Studio 18 2026" optional platform generator Win32 and x64. optional toolset "clangcl"

CMAKE_MINIMUM_REQUIRED( VERSION 3.8.2 )

project("AvsInPaint" LANGUAGES CXX)
  include(GNUInstallDirs)

# Avoid uselessly linking to unused libraries
set(CMAKE_STANDARD_LIBRARIES "" CACHE STRING "" FORCE)
set(CMAKE_C_STANDARD_LIBRARIES "" CACHE STRING "" FORCE)
set(CMAKE_CXX_STANDARD_LIBRARIES "" CACHE STRING "" FORCE)

# We require C++17 or higher.
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED TRUE)
set(CMAKE_CXX_EXTENSIONS FALSE)

if(CMAKE_CONFIGURATION_TYPES)
  set(CMAKE_CONFIGURATION_TYPES Debug Release RelWithDebInfo)
  set(CMAKE_CONFIGURATION_TYPES "${CMAKE_CONFIGURATION_TYPES}" CACHE STRING "Reset the configurations to what we need" FORCE)
endif()

IF( MSVC )  # Check for Visual Studio

  #1910-1919 = VS 15.0 (v141 toolset) Visual Studio 2017
  #1920-1929 = VS 16.0 (v142 toolset) Visual Studio 2019
  #1930-1939 = VS 17.0 (v143 toolset) Visual Studio 2022

  IF( MSVC_VERSION VERSION_LESS 1910 )
    MESSAGE(FATAL_ERROR "Visual C++ 2017 or newer required.")
  ENDIF()

  IF(MSVC_IDE)
    message("MSVC_IDE support found, reported CMAKE_GENERATOR_TOOLSET is: ${CMAKE_GENERATOR_TOOLSET}")
    string( TOLOWER "${CMAKE_GENERATOR_TOOLSET}" cmake_gentoolset_lower)

    IF(cmake_gentoolset_lower STREQUAL "llvm" OR cmake_gentoolset_lower STREQUAL "clangcl")
      if(${CMAKE_CXX_COMPILER_ID} STREQUAL "Clang")
        message("LLVM toolset was specified via -T. Compiler ID is: ${CMAKE_CXX_COMPILER_ID}; CMAKE_CXX_COMPILER_VERSION is: ${CMAKE_CXX_COMPILER_VERSION}")
        if( CMAKE_CXX_COMPILER_VERSION VERSION_LESS 7.0.1 )
          MESSAGE(FATAL_ERROR "Clang 7.0.1 or newer required")
        endif()
      endif()
      set(CLANG_IN_VS "1")
    ENDIF()
  ENDIF()

  if(CMAKE_CXX_COMPILER_ID STREQUAL "Clang" AND CMAKE_CXX_COMPILER_FRONTEND_VARIANT STREQUAL "MSVC")
    set(CLANG_IN_VS "1")
  endif()

  IF(CLANG_IN_VS STREQUAL "1")
    STRING( REPLACE "/EHsc" "/EHa" CMAKE_C_FLAGS "${CMAKE_C_FLAGS}")
    STRING( REPLACE "/EHsc" "/EHa" CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wno-inconsistent-missing-override")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-inconsistent-missing-override")
  ELSE()
    STRING( REPLACE "/EHsc" "/EHa" CMAKE_C_FLAGS "${CMAKE_C_FLAGS}")
    STRING( REPLACE "/EHsc" "/EHa" CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
    if (NOT (MSVC_VERSION LESS 1930))
      set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /fp:contract")
      set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} /fp:contract")
    endif()
  ENDIF()

  # Prevent VC++ from complaining about not using MS-specific functions
  add_definitions("/D _CRT_SECURE_NO_WARNINGS /D _SECURE_SCL=0")

  # Enable CRT heap debugging - only effective in debug builds
  add_definitions("/D _CRTDBG_MAP_ALLOC")

  IF(CLANG_IN_VS STREQUAL "1")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-unused-function")
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wno-unused-function")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-reorder")
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wno-reorder")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-unused-value")
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wno-unused-value")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-gcc-compat")
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wno-gcc-compat")
  ENDIF()

  if (NOT (MSVC_VERSION LESS 1910))
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /permissive-")
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} /permissive-")
  endif()

ELSE()

  if(WIN32)
    # Append: the toolchain file may have set flags of its own (e.g. "-static" for
    # llvm-mingw, which links libc++ and libunwind statically).
    # --enable-stdcall-fixup strips the stdcall decoration of the 32-bit MinGW
    # exports; it is a no-op for x64/ARM64 (no stdcall there).
    SET( CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -Wl,--enable-stdcall-fixup" )
  elseif(APPLE)
    SET( CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -Wl,-undefined,error" )
  elseif(CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
    SET( CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -Wl,--no-undefined" )
  endif()

ENDIF()

add_subdirectory("avsinpaint")

# uninstall target
configure_file(
    "${CMAKE_CURRENT_SOURCE_DIR}/cmake_uninstall.cmake.in"
    "${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake"
    IMMEDIATE @ONLY)

add_custom_target(uninstall
    COMMAND ${CMAKE_COMMAND} -P ${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake)
