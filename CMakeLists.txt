#
# Copyright (C) 2016-2018 Giel van Schijndel
#
# This file is part of AwesomeAssert.
#
# AwesomeAssert is free software: you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# as published by the Free Software Foundation, either version 3 of
# the License, or (at your option) any later version.
#
# AwesomeAssert is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the
# GNU Lesser General Public License along with AwesomeAssert.
# If not, see <http://www.gnu.org/licenses/>.
#

cmake_minimum_required(VERSION 3.5)

project(AwesomeAssert CXX)

find_program(CLANG_TIDY_EXE "clang-tidy")
message(STATUS "CLANG_TIDY_EXE=${CLANG_TIDY_EXE}")
if(CLANG_TIDY_EXE)
  set(CMAKE_CXX_CLANG_TIDY ${CLANG_TIDY_EXE} --config-file=${CMAKE_SOURCE_DIR}/.clang-tidy)
endif()

if(UNIX AND (
       (CMAKE_CXX_COMPILER_ID MATCHES "Clang")
    OR (CMAKE_CXX_COMPILER_ID STREQUAL "GNU" AND NOT CMAKE_CXX_COMPILER_VERSION VERSION_LESS 4.9))
    AND CMAKE_GENERATOR STREQUAL "Ninja")
  # These compilers generate coloured output, but by default only when their output channel is a
  # terminal (TTY/PTY). Ninja however captures all output in a pipe (per-subprocess), disabling
  # coloured compiler diagnostics. We forcefully enable it because Ninja, since 1.0.0
  # (ninja-build/ninja#198) takes care to strip VT100 CSI control sequences from the output if Ninja
  # itself is writing to a pipe instead of a terminal. As a result this should give us the best of
  # both worlds: coloured output when we're running in a terminal, plain output for editors, IDEs,
  # etc.
  set(CMAKE_CXX_FLAGS "-fdiagnostics-color=always ${CMAKE_CXX_FLAGS}" CACHE STRING "" FORCE)
  if(DEFINED CMAKE_CXX_CLANG_TIDY)
    list(APPEND CMAKE_CXX_CLANG_TIDY --use-color=1)
  endif()
endif()

if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
  add_compile_options(
    -Werror
    -Wall
    -Wextra
    -Wconversion
  )
endif()

set(PUBLIC_HEADER_TOP_DIR include/awesome)
set(PUBLIC_HEADER_DIR ${PUBLIC_HEADER_TOP_DIR}/assert)
set(PUBLIC_HEADERS_TOP
  ${PUBLIC_HEADER_TOP_DIR}/assert.hpp
)
set(PUBLIC_HEADERS
  ${PUBLIC_HEADER_DIR}/format-helpers.hpp
)

add_library(${PROJECT_NAME}_static STATIC
  src/assert.cpp
  src/format-helpers.cpp
  ${PUBLIC_HEADERS_TOP}
  ${PUBLIC_HEADERS}
)
target_include_directories(${PROJECT_NAME}_static PUBLIC
  $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/include>
  $<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include>
)
target_compile_features(${PROJECT_NAME}_static PUBLIC
  cxx_constexpr
  cxx_explicit_conversions
  cxx_extern_templates
  cxx_func_identifier
  cxx_noexcept
  cxx_nullptr
  cxx_override
  cxx_range_for
  cxx_relaxed_constexpr
  cxx_rvalue_references
  cxx_static_assert
  cxx_strong_enums
  cxx_trailing_return_types
)
add_library(${PROJECT_NAME} SHARED
  $<TARGET_PROPERTY:${PROJECT_NAME}_static,SOURCES>
)
target_include_directories(${PROJECT_NAME} PUBLIC
  $<TARGET_PROPERTY:${PROJECT_NAME}_static,INTERFACE_INCLUDE_DIRECTORIES>
)
target_compile_features(${PROJECT_NAME} PUBLIC
  $<TARGET_PROPERTY:${PROJECT_NAME}_static,INTERFACE_COMPILE_FEATURES>
)
set_target_properties(
    ${PROJECT_NAME}_static
    ${PROJECT_NAME}
  PROPERTIES
    VISIBILITY_INLINES_HIDDEN ON
    CXX_VISIBILITY_PRESET hidden
)

include(GenerateExportHeader)

set(EXPORT_FILE_NAME ${PROJECT_BINARY_DIR}/${PUBLIC_HEADER_TOP_DIR}/awesome_export.h)
list(APPEND PUBLIC_HEADERS_TOP ${EXPORT_FILE_NAME})
generate_export_header(${PROJECT_NAME}
  BASE_NAME Awesome
  EXPORT_FILE_NAME ${EXPORT_FILE_NAME}
)

# Make generated export header available for the target and its dependendees
target_include_directories(${PROJECT_NAME}_static PUBLIC
  $<BUILD_INTERFACE:${PROJECT_BINARY_DIR}/${PUBLIC_HEADER_TOP_DIR}>
)
target_compile_definitions(${PROJECT_NAME}_static PUBLIC AWESOME_STATIC_DEFINE)

include(GNUInstallDirs)
if(NOT DEFINED CMAKE_INSTALL_ARCHIVEDIR)
  set(CMAKE_INSTALL_ARCHIVEDIR "${CMAKE_INSTALL_LIBDIR}")
endif()

cmake_path(RELATIVE_PATH PUBLIC_HEADER_TOP_DIR BASE_DIRECTORY include)
cmake_path(RELATIVE_PATH PUBLIC_HEADER_DIR BASE_DIRECTORY include)

install(TARGETS
  ${PROJECT_NAME} ${PROJECT_NAME}_static
  EXPORT ${PROJECT_NAME}
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_ARCHIVEDIR}
)
install(FILES
  ${PUBLIC_HEADERS_TOP}
  DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/${PUBLIC_HEADER_TOP_DIR}
)
install(FILES
  ${PUBLIC_HEADERS}
  DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/${PUBLIC_HEADER_DIR}
)
install(EXPORT ${PROJECT_NAME}
  DESTINATION ${CMAKE_INSTALL_ARCHIVEDIR}/cmake
  NAMESPACE   Awesome::
  FILE        ${PROJECT_NAME}Config.cmake
)
set_target_properties(${PROJECT_NAME}        PROPERTIES EXPORT_NAME Assert)
set_target_properties(${PROJECT_NAME}_static PROPERTIES EXPORT_NAME Assert_static)
export(EXPORT ${PROJECT_NAME})

enable_testing()

add_executable(${PROJECT_NAME}.Test.simple
  test/simple.cpp
)
target_link_libraries(${PROJECT_NAME}.Test.simple PRIVATE AwesomeAssert)
add_test(NAME ${PROJECT_NAME}.Test.simple.failure
  COMMAND sh -c "$<TARGET_FILE:${PROJECT_NAME}.Test.simple> && :"
)
add_test(NAME ${PROJECT_NAME}.Test.simple.success
  COMMAND ${PROJECT_NAME}.Test.simple 2 3 4 5
)
set_tests_properties(${PROJECT_NAME}.Test.simple.failure
  PROPERTIES
    PASS_REGULAR_EXPRESSION "Assertion `argc >= 10 - argc && \"[^\"]*\"', with expansion `1 >= 9 && [^']*', failed"
    ENVIRONMENT "NO_COLOR=1"
)
