#!/bin/sh

set -e
set -x

cmake -E make_directory build
if [ "${ANALYZE}" ]; then
  if [ "${CC}" = "clang" ]; then
    scan-build --use-cc="${CC}" --use-c++="${CXX}" cmake -E chdir build cmake -G Ninja ..
    scan-build --use-cc="${CC}" --use-c++="${CXX}" --status-bugs cmake --build build
  else
    cppcheck --template "{file}:{line}: {severity} ({id}): {message}" \
      --enable=style --force --std=c++11 -j 8 \
      -U__FUNCSIG__ -U_MSC_VER \
      -U__GNUC__ -U__clang__ \
      -Iinclude \
      src 2> build/cppcheck.txt
    if [ -s build/cppcheck.txt ]; then
      cat < build/cppcheck.txt >&2
      exit 1
    fi
  fi
else
  cmake -E chdir build cmake -G Ninja ..
  cmake --build build
  cmake --build build --target test
fi
