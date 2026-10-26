# ---------------------------------------------------------------------------
#  CMake toolchain: Linux on aarch64 (ARM64), cross-compiled on x86-64
#
#  Uses the Debian/Ubuntu aarch64 cross toolchain (g++-aarch64-linux-gnu).
#  The plugin has no architecture specific code (no SIMD, no x86 intrinsics),
#  so no extra flags are needed for ARM64.
#
#  Usage:
#    cmake -S . -B build/aarch64 -DCMAKE_BUILD_TYPE=Release ^
#          -DCMAKE_TOOLCHAIN_FILE=cmake/toolchains/aarch64-linux-gnu.cmake
# ---------------------------------------------------------------------------

set(CMAKE_SYSTEM_NAME Linux)
set(CMAKE_SYSTEM_PROCESSOR aarch64)

find_program(CMAKE_C_COMPILER   NAMES aarch64-linux-gnu-gcc   REQUIRED)
find_program(CMAKE_CXX_COMPILER NAMES aarch64-linux-gnu-g++   REQUIRED)

set(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_PACKAGE ONLY)
