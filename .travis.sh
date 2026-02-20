#!/bin/sh

set -e
set -x

CTEST_OUTPUT_ON_FAILURE=ON
CMAKE_CXX_STANDARD_REQUIRED=ON
export CTEST_OUTPUT_ON_FAILURE CMAKE_CXX_STANDARD_REQUIRED

if [ "${CC}" = "clang" -a "${TRAVIS_OS_NAME}" != "osx" ]; then
  SCAN_BUILD_CMD="scan-build --use-cc=${CC} --use-c++=${CXX}"
fi

cmake -E make_directory build
${SCAN_BUILD_CMD} cmake -E chdir build cmake \
  -G Ninja \
  ${CMAKE_CXX_COMPILER:+"-DCMAKE_CXX_COMPILER=${CMAKE_CXX_COMPILER}"} \
  ..

${SCAN_BUILD_CMD:+${SCAN_BUILD_CMD} --status-bugs} cmake --build build
cmake --build build --target test
