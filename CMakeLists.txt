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

cmake_minimum_required(VERSION 3.5)

project(AwesomeAssert)

set(PUBLIC_HEADER_DIR include/awesome)
set(PUBLIC_HEADERS
  ${PUBLIC_HEADER_DIR}/assert.hpp
)

add_library(${PROJECT_NAME} STATIC
  src/assert.cpp
  ${PUBLIC_HEADERS}
)
target_include_directories(${PROJECT_NAME} PUBLIC
  include
)

enable_testing()

add_executable(${PROJECT_NAME}.Test.simple
  test/simple.cpp
)
target_link_libraries(${PROJECT_NAME}.Test.simple PRIVATE AwesomeAssert)
add_test(NAME ${PROJECT_NAME}.Test.simple.failure
  COMMAND ${PROJECT_NAME}.Test.simple
)
add_test(NAME ${PROJECT_NAME}.Test.simple.success
  COMMAND ${PROJECT_NAME}.Test.simple 2 3 4 5
)
set_tests_properties(${PROJECT_NAME}.Test.simple.failure
  PROPERTIES
    PASS_REGULAR_EXPRESSION "Assertion `argc >= 10 - argc', with expansion `1 >= 9', failed"
)
