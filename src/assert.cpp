/*
 *  Copyright (C) 2016-2018 Giel van Schijndel
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
#include <sstream>
#include <string>

#if defined(__GNUC__) || defined(__clang__)
  #define AWESOME_ATTR_WEAK __attribute__((__weak__))
#else
  #define AWESOME_ATTR_WEAK
#endif

namespace AwesomeAssert {

stringifier::~stringifier() noexcept
{
  while (next)
  {
    auto to_delete = std::move(next);

    // Delete on next iteration and prevent recursion of unknown depth in destructor call of to_delete
    next = std::move(to_delete->next);
  }
}

std::ostream& operator<<(std::ostream& os, const stringifier& str)
{
  return str.convert(os);
}

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

using namespace std::literals::string_view_literals;

std::string_view string_maker<::std::    equal_to <>>::str() const noexcept { return "=="sv; }
std::string_view string_maker<::std::not_equal_to <>>::str() const noexcept { return "!="sv; }
std::string_view string_maker<::std::   less      <>>::str() const noexcept { return  "<"sv; }
std::string_view string_maker<::std::   less_equal<>>::str() const noexcept { return "<="sv; }
std::string_view string_maker<::std::greater      <>>::str() const noexcept { return  ">"sv; }
std::string_view string_maker<::std::greater_equal<>>::str() const noexcept { return ">="sv; }
std::string_view string_maker<::std::    bit_and  <>>::str() const noexcept { return  "&"sv; }
std::string_view string_maker<::std::logical_and  <>>::str() const noexcept { return "&&"sv; }
std::string_view string_maker<::std::logical_or   <>>::str() const noexcept { return "||"sv; }

namespace detail
{
  std::ostream& string_maker_op::convert(std::ostream& os) const
  {
    return os << this->str();
  }

  bool string_maker_op::is_operator() const
  {
    return true;
  }
}

namespace
{
  std::string assert_fail_to_string(const violation_info& info)
  {
    std::ostringstream os;
    assert_fail_default_log(os, info);
    return std::move(os).str();
  }
}

precondition_error::precondition_error(violation_info info)
  : std::invalid_argument(assert_fail_to_string(info))
  , _info(std::move(info))
{
}

std::ostream& operator<<(std::ostream& os, const precondition_error& error)
{
  return assert_fail_default_log(os, error._info);
}

void assert_fail_default_log(const violation_info& info) noexcept
{
  assert_fail_default_log(std::cerr, info)
    // Must flush() because we're likely to terminate after this
    .flush();
}

namespace
{
  [[noreturn]]
  void assert_failed_default(violation_info in_info) noexcept
  {
    // To get abort(), regardless of which namespace it's in.
    using namespace std;

    {
      // Prevent memory leak detectors from complaining about our memory (abort() prevents destructors
      // from running), by ensuring this gets destroyed as soon as we leave this scope.
      const violation_info info(std::move(in_info));

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
  violation_info info(file, line, function, expr_str, std::move(expr));

#ifndef AWESOME_PRECONDITION_NO_NOEXCEPT
  assert_failed_default(std::move(info));
#else
  throw precondition_error(std::move(info));
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
  assert_failed_default(violation_info(file, line, function, expr_str, std::move(expr)));
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
  assert_failed_default(violation_info(file, line, function, expr_str, std::move(expr)));
}

}
