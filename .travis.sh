#!/bin/sh

set -e
set -x

if [ "${ANALYZE}" -a "${CC}" = "clang" ]; then
  SCAN_BUILD_CMD="scan-build --use-cc=${CC} --use-c++=${CXX}"
else
  SCAN_BUILD_CMD=""
fi

cmake -E make_directory build
${SCAN_BUILD_CMD} cmake -E chdir build cmake -G Ninja ..

if [ "${ANALYZE}" -a "${CC}" != "clang" ]; then
  cppcheck --template "{file}:{line}: {severity} ({id}): {message}" \
    --enable=style --force --std=c++11 -j 8 \
    -U__FUNCSIG__ -U_MSC_VER \
    -U__GNUC__ -U__clang__ \
    -Iinclude -Ibuild/include/awesome \
    src 2> build/cppcheck.txt
  if [ -s build/cppcheck.txt ]; then
    cat < build/cppcheck.txt >&2
    exit 1
  fi
else
  ${SCAN_BUILD_CMD:+${SCAN_BUILD_CMD} --status-bugs} cmake --build build
fi
if [ ! "${ANALYZE}" ]; then
  cmake --build build --target test
fi
