#!/bin/sh

set -e
set -x

CTEST_OUTPUT_ON_FAILURE=ON
CMAKE_CXX_STANDARD_REQUIRED=ON
export CTEST_OUTPUT_ON_FAILURE CMAKE_CXX_STANDARD_REQUIRED

if [ "${CC}" = "gcc" ] && which gcc-4.7 > /dev/null 2>&1; then
  CC=gcc-4.7
fi
if [ "${CXX}" = "g++" ] && which g++-4.7 > /dev/null 2>&1; then
  CXX=g++-4.7
fi

if [ "${CC}" = "clang" -a "${TRAVIS_OS_NAME}" != "osx" ]; then
  SCAN_BUILD_CMD="scan-build --use-cc=${CC} --use-c++=${CXX}"
fi

cmake -E make_directory build
${SCAN_BUILD_CMD} cmake -E chdir build cmake -G Ninja ..

if [ "x${CPPCHECK:-}" = "xyes" ]; then
  cppcheck --template "{file}:{line}: {severity} ({id}): {message}" \
    --enable=style --force --std=c++11 -j 8 \
    -U__FUNCSIG__ -U_MSC_VER \
    -U__GNUC__ -U__clang__ -U__GLIBCXX__ -U_LIBCPP_VERSION \
    -Iinclude -Ibuild/include/awesome \
    src 2> build/cppcheck.txt
  if [ -s build/cppcheck.txt ]; then
    cat < build/cppcheck.txt >&2
    exit 1
  fi
else
  ${SCAN_BUILD_CMD:+${SCAN_BUILD_CMD} --status-bugs} cmake --build build
  cmake --build build --target test
fi
