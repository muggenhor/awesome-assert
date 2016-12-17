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
      Yellow,
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
          case TColor::Yellow:      return os << "\033[22;33m";
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

namespace detail {
  static std::ostream& operator<<(std::ostream& os, const bool_expression& expr)
  {
    if (expr.begin() == expr.end())
      return os << TColor::Cyan << true;

    bool is_operator = false;
    for (bool_expression::const_iterator token = expr.begin(); token != expr.end(); ++token)
    {
      if (token != expr.begin())
        os << os.widen(' ');
      os << (is_operator ? TColor::Yellow : TColor::Cyan) << *token;
      is_operator = !is_operator;
    }
    return os;
  }

  std::ostream& operator<<(std::ostream& os, const compare_eq&) { return os << "=="; }
  std::ostream& operator<<(std::ostream& os, const compare_ne&) { return os << "!="; }
  std::ostream& operator<<(std::ostream& os, const compare_lt&) { return os << "<" ; }
  std::ostream& operator<<(std::ostream& os, const compare_le&) { return os << "<="; }
  std::ostream& operator<<(std::ostream& os, const compare_gt&) { return os << ">" ; }
  std::ostream& operator<<(std::ostream& os, const compare_ge&) { return os << ">="; }

  bool_expression::~bool_expression()
  {
    clear();
  }

  void bool_expression::clear()
  {
    while (token_count)
    {
      --token_count;
      delete [] fail_expression[token_count];
      fail_expression[token_count] = NULL;
    }
    delete [] fail_expression;
    fail_expression = NULL;
  }

  bool_expression::const_iterator bool_expression::begin() const
  {
    return &fail_expression[0];
  }

  bool_expression::const_iterator bool_expression::end() const
  {
    return &fail_expression[token_count];
  }
}

void assert_failed(const char* file, int line, const char* function, const char* expr_str, detail::bool_expression expr) AWESOME_NOEXCEPT
{
  {
    using namespace std;

    cerr << boolalpha
      << TColor::Bright << file << ":" << line << ": " << function << ": "
      << TColor::Grey   << "Assertion `"
      << TColor::Cyan   << expr_str
      << TColor::None   << "', with expansion `"
                        << expr
      << TColor::None   << "', "
      << TColor::Red    << "failed"
      << TColor::None   << "."
      << endl // Using 'endl' instead of "\n" because we need its flush.
      ;

    // Prevent memory leak detectors from complaining about our memory (abort() prevents destructors from running).
    expr.clear();
  }

  abort();
}

}
