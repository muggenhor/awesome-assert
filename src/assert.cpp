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

#ifndef WIN32
  #include <unistd.h>
#endif

namespace AwesomeAssert {

namespace
{
  struct TColor
  {
    enum TCode
    {
      None,
      Red,
      Cyan,
      Grey,
      Bright,
    };
  };

  std::ostream& operator<<(std::ostream& os, TColor::TCode color)
  {
#if _XOPEN_VERSION >= 700 || _POSIX_VERSION >= 200112L
    if ((os.rdbuf() == std::cout.rdbuf() && isatty(STDOUT_FILENO))
     || (os.rdbuf() == std::cerr.rdbuf() && isatty(STDERR_FILENO)))
    {
      switch (color)
      {
          case TColor::Red:         return os << "\033[22;31m";
          case TColor::Cyan:        return os << "\033[22;36m";
          case TColor::Grey:        return os << "\033[1;30m";
          case TColor::Bright:      return os << "\033[1;39m";
          case TColor::None:
          default:                  return os << "\033[22;39m";
      }
    }
#endif

    return os;
  }
}

void assert_failed(const char* file, int line, const char* function, const char* expr_str) AWESOME_NOEXCEPT
{
  using namespace std;

  cerr << boolalpha
    << TColor::Bright << file << ":" << line << ": " << function << ": "
    << TColor::Grey   << "Assertion `"
    << TColor::Cyan   << expr_str
    << TColor::None   << "' "
    << TColor::Red    << "failed"
    << TColor::None   << "."
    << endl // Using 'endl' instead of "\n" because we need its flush.
    ;

  abort();
}

}
