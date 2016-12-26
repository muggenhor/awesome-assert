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
#include <cstring>
#include <iostream>
#include <sstream>
#include <string>

#if __GLIBCXX__
  #include <ext/stdio_sync_filebuf.h>
#endif

#ifndef WIN32
  #include <unistd.h>
#endif

#ifdef __GNUC__
  #define AWESOME_ATTR_WEAK __attribute__((__weak__))
#else
  #define AWESOME_ATTR_WEAK
#endif

namespace AwesomeAssert {

stringifier::~stringifier() AWESOME_NOEXCEPT
{
  while (next)
  {
    stringifier* const to_delete = next;

    // Delete on next iteration
    next = next->next;

    // Prevent recursion of unknown depth in destructor calls
    to_delete->next = NULL;
    delete to_delete;
  }
}

std::ostream& operator<<(std::ostream& os, const stringifier& str)
{
  return str.convert(os);
}

#if __cplusplus >= 201103L
template struct string_maker<bool>;
template struct string_maker<short>;
template struct string_maker<unsigned short>;
template struct string_maker<int>;
template struct string_maker<unsigned int>;
template struct string_maker<long>;
template struct string_maker<unsigned long>;
template struct string_maker<long long>;
template struct string_maker<unsigned long long>;
template struct string_maker<float>;
template struct string_maker<double>;
template struct string_maker<long double>;
template struct string_maker<void*>;
template struct string_maker<const void*>;
template struct string_maker<char>;
template struct string_maker<signed char>;
template struct string_maker<unsigned char>;
template struct string_maker<const char*>;
template struct string_maker<const signed char*>;
template struct string_maker<const unsigned char*>;
#else
template <> std::ostream& string_maker<bool                >::convert(std::ostream& os) const { return os << val; }
template <> std::ostream& string_maker<short               >::convert(std::ostream& os) const { return os << val; }
template <> std::ostream& string_maker<unsigned short      >::convert(std::ostream& os) const { return os << val; }
template <> std::ostream& string_maker<int                 >::convert(std::ostream& os) const { return os << val; }
template <> std::ostream& string_maker<unsigned int        >::convert(std::ostream& os) const { return os << val; }
template <> std::ostream& string_maker<long                >::convert(std::ostream& os) const { return os << val; }
template <> std::ostream& string_maker<unsigned long       >::convert(std::ostream& os) const { return os << val; }
template <> std::ostream& string_maker<long long           >::convert(std::ostream& os) const { return os << val; }
template <> std::ostream& string_maker<unsigned long long  >::convert(std::ostream& os) const { return os << val; }
template <> std::ostream& string_maker<float               >::convert(std::ostream& os) const { return os << val; }
template <> std::ostream& string_maker<double              >::convert(std::ostream& os) const { return os << val; }
template <> std::ostream& string_maker<long double         >::convert(std::ostream& os) const { return os << val; }
template <> std::ostream& string_maker<void*               >::convert(std::ostream& os) const { return os << val; }
template <> std::ostream& string_maker<const void*         >::convert(std::ostream& os) const { return os << val; }
template <> std::ostream& string_maker<char                >::convert(std::ostream& os) const { return os << val; }
template <> std::ostream& string_maker<signed char         >::convert(std::ostream& os) const { return os << val; }
template <> std::ostream& string_maker<unsigned char       >::convert(std::ostream& os) const { return os << val; }
template <> std::ostream& string_maker<const char*         >::convert(std::ostream& os) const { return os << val; }
template <> std::ostream& string_maker<const signed char*  >::convert(std::ostream& os) const { return os << val; }
template <> std::ostream& string_maker<const unsigned char*>::convert(std::ostream& os) const { return os << val; }
#endif

const char* string_maker<detail::compare_eq>::str() const AWESOME_NOEXCEPT { return "=="; }
const char* string_maker<detail::compare_ne>::str() const AWESOME_NOEXCEPT { return "!="; }
const char* string_maker<detail::compare_lt>::str() const AWESOME_NOEXCEPT { return "<" ; }
const char* string_maker<detail::compare_le>::str() const AWESOME_NOEXCEPT { return "<="; }
const char* string_maker<detail::compare_gt>::str() const AWESOME_NOEXCEPT { return ">" ; }
const char* string_maker<detail::compare_ge>::str() const AWESOME_NOEXCEPT { return ">="; }

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
      Bright
    };
  };

  std::ostream& operator<<(std::ostream& os, TColor::TCode color)
  {
    if (!os
     || !os.rdbuf())
      return os;

#if _XOPEN_VERSION >= 700 || _POSIX_VERSION >= 200112L
    bool is_a_tty = false;
#if __GLIBCXX__
    // Support for glibc++: query the actual file-descriptor, can work for other things than stdout/stderr too
    if (__gnu_cxx::stdio_sync_filebuf<char>* rdbuf = dynamic_cast<__gnu_cxx::stdio_sync_filebuf<char>*>(os.rdbuf()))
    {
      is_a_tty = rdbuf->file() && isatty(fileno(rdbuf->file()));
    }
#elif _LIBCPP_VERSION
    // NOTE: the same isn't possible for Clang/libc++ because it hides the RTTI required to perform a dynamic_cast
    //       to std::__stdoutbuf<char> from its dynamic symbol table. (The lacking private access is something that
    //       could be worked around in a standards-compliant way, this cannot.)
#endif

    is_a_tty = is_a_tty
     || (os.rdbuf() == std::cout.rdbuf() && isatty(STDOUT_FILENO))
     || (os.rdbuf() == std::cerr.rdbuf() && isatty(STDERR_FILENO))
     ;

    if (is_a_tty)
    {
      switch (color)
      {
          case TColor::Red:         return os.write("\033[22;31m", 8);
          case TColor::Cyan:        return os.write("\033[22;36m", 8);
          case TColor::Yellow:      return os.write("\033[22;33m", 8);
          case TColor::Grey:        return os.write("\033[1;30m", 7);
          case TColor::Bright:      return os.write("\033[1;39m", 7);
          case TColor::None:        return os.write("\033[22;39m", 8);
      }
    }
#endif

    return os;
  }

  class expression_colorizer
  {
  public:
    expression_colorizer(const char* const expr_str, const detail::bool_expression& expr)
      : _lhs(expr_str)
      , _end(expr_str + strlen(expr_str))
      , _op(_end)
      , _rhs(_end)
    {
      using namespace std;

      for (detail::bool_expression::const_iterator token = expr.begin(); token != expr.end(); ++token)
      {
        const detail::string_maker_op* const op = dynamic_cast<const detail::string_maker_op*>(&*token);
        if (!op)
          continue;

        // Multiple operators found in the expression: don't even try to separate them
        if (_op != _end)
        {
          _rhs = _op = _end;
          return;
        }

        const char* const op_str = op->str();
        _op = strstr(_lhs, op_str);
        if (!_op)
        {
          _op = _end;
          continue;
        }

        _rhs = _op + strlen(op_str);
      }
    }

    friend std::ostream& operator<<(std::ostream& os, const expression_colorizer& expr)
    {
      os << TColor::Cyan;
      if (os.good())
        os.write(expr._lhs, expr._op  - expr._lhs);

      os << TColor::Yellow;
      if (os.good())
        os.write(expr._op , expr._rhs - expr._op );

      os << TColor::Cyan;
      if (os.good())
        os.write(expr._rhs, expr._end - expr._rhs);
      return os << TColor::None;
    }

  private:
    const char* _lhs;
    const char* _end;
    const char* _op;
    const char* _rhs;
  };
}

namespace detail
{
  std::ostream& string_maker_op::convert(std::ostream& os) const
  {
    return os << this->str();
  }

  std::ostream& operator<<(std::ostream& os, const bool_expression& expr)
  {
    if (expr.begin() == expr.end())
    {
      os << TColor::Cyan;
      if (os.good())
        os << true;
    }

    bool is_operator = false;
    for (bool_expression::const_iterator token = expr.begin(); token != expr.end() && os.good(); ++token)
    {
      if (token != expr.begin())
        os << os.fill();
      os << (is_operator ? TColor::Yellow : TColor::Cyan);
      if (os.good())
        os << *token;
      is_operator = !is_operator;
    }

    return os << TColor::None;
  }

  bool_expression::const_iterator::const_iterator() AWESOME_NOEXCEPT
    : cur(NULL)
  {}

  bool_expression::const_iterator::const_iterator(const stringifier* cur_) AWESOME_NOEXCEPT
    : cur(cur_)
  {}

  bool_expression::const_iterator& bool_expression::const_iterator::operator++() AWESOME_NOEXCEPT
  {
    cur = cur->next;
    return *this;
  }

  bool_expression::const_iterator bool_expression::const_iterator::operator++(int) AWESOME_NOEXCEPT
  {
    const_iterator prev(*this);
    ++*this;
    return prev;
  }

  stringifier const& bool_expression::const_iterator::operator*() const AWESOME_NOEXCEPT
  {
    return *cur;
  }

  stringifier const* bool_expression::const_iterator::operator->() const AWESOME_NOEXCEPT
  {
    return cur;
  }

  bool bool_expression::const_iterator::operator==(const const_iterator& rhs) const AWESOME_NOEXCEPT
  {
    return !(this->cur != rhs.cur);
  }

  bool bool_expression::const_iterator::operator!=(const const_iterator& rhs) const AWESOME_NOEXCEPT
  {
    return this->cur != rhs.cur;
  }

  bool_expression::const_iterator bool_expression::begin() const AWESOME_NOEXCEPT
  {
    return fail_expression;
  }

  bool_expression::const_iterator bool_expression::end() const AWESOME_NOEXCEPT
  {
    return const_iterator();
  }
}

violation_info::violation_info() AWESOME_NOEXCEPT
  : line_number(-1)
  , file_name(NULL)
  , function_name(NULL)
  , comment(NULL)
  , expression(true /* aka no failure */)
{
}

violation_info::violation_info(
    const char*                     file
  , int                             line
  , const char*                     function
  , const char*                     expr_str
  , detail::bool_expression         expr
  ) AWESOME_NOEXCEPT
  : line_number(line)
  , file_name(file)
  , function_name(function)
  , comment(expr_str)
  , expression(AWESOME_MOVE(expr))
{
}

namespace
{
  std::string assert_fail_to_string(const violation_info& info)
  {
    std::ostringstream os;
    assert_fail_default_log(os, info);
    return os.str();
  }
}

precondition_error::precondition_error(violation_info info)
  : std::invalid_argument(assert_fail_to_string(info))
  , _info(AWESOME_MOVE(info))
{
}

std::ostream& operator<<(std::ostream& os, const precondition_error& error)
{
  return assert_fail_default_log(os, error._info);
}

std::ostream& assert_fail_default_log(std::ostream& os, const violation_info& info) AWESOME_NOEXCEPT
{
  const std::ios_base::fmtflags flags(os.flags());

  os << std::boolalpha
    << TColor::Bright << info.file_name << ":" << info.line_number << ": " << info.function_name << ": "
    << TColor::Grey   << "Assertion"
    << TColor::None   << " `"
                      << expression_colorizer(info.comment, info.expression)
                      << "', with expansion `"
                      << info.expression
                      << "', "
    << TColor::Red    << "failed"
    << TColor::None   << ".\n"
    ;
  os.flags(flags);
  return os;
}

void assert_fail_default_log(const violation_info& info) AWESOME_NOEXCEPT
{
  assert_fail_default_log(std::cerr, info)
    // Must flush() because we're likely to terminate after this
    .flush();
}

namespace
{
  AWESOME_NORETURN
  void assert_failed_default(violation_info in_info) AWESOME_NOEXCEPT
  {
    // To get abort(), regardless of which namespace it's in.
    using namespace std;

    {
      // Prevent memory leak detectors from complaining about our memory (abort() prevents destructors
      // from running), by ensuring this gets destroyed as soon as we leave this scope.
      violation_info info(AWESOME_MOVE(in_info));

      assert_fail_default_log(info);
    }

    abort();
  }
}

AWESOME_ATTR_WEAK
void assert_failed_precondition(
    const char*                     file
  , int                             line
  , const char*                     function
  , const char*                     expr_str
  , detail::bool_expression         expr
  ) AWESOME_PRECONDITION_NOEXCEPT
{
  violation_info info(file, line, function, expr_str, AWESOME_MOVE(expr));

#if __cplusplus >= 201103L
  if (noexcept(assert_failed_precondition(file, line, function, expr_str, AWESOME_MOVE(expr))))
    assert_failed_default(AWESOME_MOVE(info));
#endif

#ifndef AWESOME_PRECONDITION_NO_NOEXCEPT
  assert_failed_default(AWESOME_MOVE(info));
#else
  throw precondition_error(AWESOME_MOVE(info));
#endif
}

AWESOME_ATTR_WEAK
void assert_failed_invariant(
    const char*                     file
  , int                             line
  , const char*                     function
  , const char*                     expr_str
  , detail::bool_expression         expr
  ) AWESOME_INVARIANT_NOEXCEPT
{
  assert_failed_default(violation_info(file, line, function, expr_str, AWESOME_MOVE(expr)));
}

AWESOME_ATTR_WEAK
void assert_failed_postcondition(
    const char*                     file
  , int                             line
  , const char*                     function
  , const char*                     expr_str
  , detail::bool_expression         expr
  ) AWESOME_POSTCONDITION_NOEXCEPT
{
  assert_failed_default(violation_info(file, line, function, expr_str, AWESOME_MOVE(expr)));
}

}
