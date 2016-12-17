/*
 *  Copyright (C) 2016 Giel van Schijndel
 *
 *  This file is part of AwesomeAssert.
 *
 *  AwesomeAssert is free software: you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public License
 *  as published by the Free Software Foundation, either version 3 of
 *  the License, or (at your option) any later version.
 *
 *  AwesomeAssert is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the
 *  GNU Lesser General Public License along with AwesomeAssert.
 *  If not, see <http://www.gnu.org/licenses/>.
*/

#include <awesome/assert.hpp>
#include <cstdlib>
#include <iostream>

namespace AwesomeAssert {

void assert_failed(const char* file, int line, const char* function, const char* expr_str) AWESOME_NOEXCEPT
{
  using namespace std;

  cerr << boolalpha
    << file << ":" << line << ": " << function << ": Assertion `"
    << expr_str
    << "' failed."
    << endl // Using 'endl' instead of "\n" because we need its flush.
    ;

  abort();
}

}
