#
# Copyright (C) 2016 Giel van Schijndel
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

cmake_minimum_required(VERSION 3.2)

project(AwesomeAssert)

set(PUBLIC_HEADER_DIR include/awesome)
set(PUBLIC_HEADERS
  ${PUBLIC_HEADER_DIR}/assert.hpp
)

add_library(${PROJECT_NAME}_static STATIC
  src/assert.cpp
  ${PUBLIC_HEADERS}
)
target_include_directories(${PROJECT_NAME}_static PUBLIC
  $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/include>
  $<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include>
)
add_library(${PROJECT_NAME} SHARED
  $<TARGET_PROPERTY:${PROJECT_NAME}_static,SOURCES>
)
target_include_directories(${PROJECT_NAME} PUBLIC
  $<TARGET_PROPERTY:${PROJECT_NAME}_static,INTERFACE_INCLUDE_DIRECTORIES>
)
set_target_properties(
    ${PROJECT_NAME}_static
    ${PROJECT_NAME}
  PROPERTIES
    VISIBILITY_INLINES_HIDDEN ON
    CXX_VISIBILITY_PRESET hidden
)

include(GenerateExportHeader)

set(EXPORT_FILE_NAME ${PROJECT_BINARY_DIR}/${PUBLIC_HEADER_DIR}/awesome_export.h)
list(APPEND PUBLIC_HEADERS ${EXPORT_FILE_NAME})
generate_export_header(${PROJECT_NAME}
  BASE_NAME Awesome
  EXPORT_FILE_NAME ${EXPORT_FILE_NAME}
)

# Make generated export header available for the target and its dependendees
target_include_directories(${PROJECT_NAME}_static PUBLIC
  $<BUILD_INTERFACE:${PROJECT_BINARY_DIR}/${PUBLIC_HEADER_DIR}>
)
target_compile_definitions(${PROJECT_NAME}_static PUBLIC AWESOME_STATIC_DEFINE)

install(TARGETS
  ${PROJECT_NAME} ${PROJECT_NAME}_static
  EXPORT ${PROJECT_NAME}
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib
)
install(FILES
  ${PUBLIC_HEADERS}
  DESTINATION ${PUBLIC_HEADER_DIR}
)
install(EXPORT ${PROJECT_NAME}
  DESTINATION lib/cmake
)
export(EXPORT ${PROJECT_NAME})

enable_testing()

add_executable(${PROJECT_NAME}.Test.simple
  test/simple.cpp
)
target_link_libraries(${PROJECT_NAME}.Test.simple PRIVATE AwesomeAssert)
add_test(NAME ${PROJECT_NAME}.Test.simple.failure
  COMMAND sh -c $<TARGET_FILE:${PROJECT_NAME}.Test.simple>
)
add_test(NAME ${PROJECT_NAME}.Test.simple.success
  COMMAND ${PROJECT_NAME}.Test.simple 2 3 4 5
)
set_tests_properties(${PROJECT_NAME}.Test.simple.failure
  PROPERTIES
    PASS_REGULAR_EXPRESSION "Assertion `argc >= 10 - argc', with expansion `1 >= 9', failed"
)
