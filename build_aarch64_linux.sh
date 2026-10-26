#!/bin/sh
# Linux on ARM64 (aarch64) build of AvsInPaint, cross-compiled on x86-64.
#
# Uses the Debian/Ubuntu aarch64 cross toolchain (g++-aarch64-linux-gnu).
# The plugin has no SIMD and no architecture specific code, so the same
# sources build for x86-64 and aarch64 unchanged.
#
#   apt-get install g++-aarch64-linux-gnu
#   ./build_aarch64_linux.sh
#
# Output: Build/aarch64/avsinpaint/libavsinpaint.so
set -e

BUILD_DIR="${BUILD_DIR:-$(dirname "$0")/Build/aarch64}"
SRC_DIR="$(dirname "$0")"

rm -rf "$BUILD_DIR"
cmake -S "$SRC_DIR" -B "$BUILD_DIR" -DCMAKE_BUILD_TYPE=Release \
  -DCMAKE_TOOLCHAIN_FILE="$SRC_DIR/cmake/toolchains/aarch64-linux-gnu.cmake"
cmake --build "$BUILD_DIR" --parallel

echo "Done: $BUILD_DIR/avsinpaint/libavsinpaint.so"
file "$BUILD_DIR/avsinpaint/libavsinpaint.so"
