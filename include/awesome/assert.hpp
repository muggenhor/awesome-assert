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

#ifndef __INCLUDED_AWESOME_ASSERT_HPP__
#define __INCLUDED_AWESOME_ASSERT_HPP__

#include <cstring>
#include <ostream>
#include <sstream>

#if __cplusplus >= 201103L
  #define AWESOME_NOEXCEPT noexcept
#else
  #define AWESOME_NOEXCEPT throw()
#endif

#if   defined(_MSC_VER)
  #define AWESOME_NORETURN __declspec(noreturn)
#elif defined(__GNUC__) || defined(__clang__)
  #define AWESOME_NORETURN __attribute__ ((__noreturn__))
#elif __cplusplus >= 201103L
  // Unfortunately some versions of GCC with -std=c++0x claim to be C++11
  // without supporting either attributes or the [[noreturn]] attribute. Which
  // is why we're using GCC's own __noreturn__ attribute above first.
  #define AWESOME_NORETURN [[noreturn]]
#else
  #define AWESOME_NORETURN
#endif

namespace AwesomeAssert
{
  /**
   * \brief String converter helper template class.
   *
   * Specialize this to use an alternate string conversion if desired.
   */
  template <typename T>
  struct string_maker
  {
    static char* convert(const T& val)
    {
      using namespace std;

      ostringstream os;
      os << val;
      string const str(os.str());
      char* const buf = new char[str.size() + 1];
      memcpy(buf, str.data(), str.size());
      buf[str.size()] = '\0';

      return buf;
    }
  };

  namespace detail
  {
    // Replacements for std::equal, std::less, etc. that have the template on the function instead
    // of the class. Necessary to permit comparisons of differing types without forcing a conversion.
    // This gets solved in a later C++ standard (14 or 17, IIRC), but that doesn't help us now.
    struct compare_eq { template <class TL, class TR> bool operator()(const TL& lhs, const TR& rhs) const { return lhs == rhs; } };
    struct compare_ne { template <class TL, class TR> bool operator()(const TL& lhs, const TR& rhs) const { return lhs != rhs; } };
    struct compare_lt { template <class TL, class TR> bool operator()(const TL& lhs, const TR& rhs) const { return lhs <  rhs; } };
    struct compare_le { template <class TL, class TR> bool operator()(const TL& lhs, const TR& rhs) const { return lhs <= rhs; } };
    struct compare_gt { template <class TL, class TR> bool operator()(const TL& lhs, const TR& rhs) const { return lhs >  rhs; } };
    struct compare_ge { template <class TL, class TR> bool operator()(const TL& lhs, const TR& rhs) const { return lhs >= rhs; } };

    std::ostream& operator<<(std::ostream& os, const compare_eq&);
    std::ostream& operator<<(std::ostream& os, const compare_ne&);
    std::ostream& operator<<(std::ostream& os, const compare_lt&);
    std::ostream& operator<<(std::ostream& os, const compare_le&);
    std::ostream& operator<<(std::ostream& os, const compare_gt&);
    std::ostream& operator<<(std::ostream& os, const compare_ge&);

    struct bool_expression
    {
    private:
      template <typename T>
      static char** create_expression_list(const T& val)
      {
        char** const expr = new char*[1];
        try
        {
          expr[0] = string_maker<T>::convert(val);
          return expr;
        }
        catch (...)
        {
          delete [] expr;
          throw;
        }
      }

      template <typename TL, typename TO, typename TR>
      static char** create_expression_list(const TL& lhs, const TO& op, const TR& rhs)
      {
        char** const expr = new char*[3];
        expr[0] = expr[1] = expr[2] = NULL;
        try
        {
          expr[0] = string_maker<TL>::convert(lhs);
          expr[1] = string_maker<TO>::convert(op);
          expr[2] = string_maker<TR>::convert(rhs);
          return expr;
        }
        catch (...)
        {
          delete [] expr[2];
          delete [] expr[1];
          delete [] expr[0];
          delete [] expr;
          throw;
        }
      }

    public:
      typedef char const* const* const_iterator;
      typedef const_iterator iterator;

      template <typename T>
      bool_expression(const T& val)
        : fail_expression(val ? NULL : create_expression_list(val))
        , token_count(1)
      {
        if (!fail_expression)
          token_count = 0;
      }

      template <typename TL, typename TO, typename TR>
      bool_expression(
          const TL& lhs, const TO& op, const TR& rhs)
        : fail_expression(op(lhs, rhs) ? NULL : create_expression_list(lhs, op, rhs))
        , token_count(3)
      {
        if (!fail_expression)
          token_count = 0;
      }

      ~bool_expression();
      void clear();
      const_iterator begin() const;
      const_iterator end() const;

      operator bool() const
      {
        return !fail_expression;
      }

    private:
      char** fail_expression;
      signed char token_count;
    };

    template <typename T>
    struct expression_lhs
    {
      expression_lhs(T lhs_)
        : lhs(lhs_)
      {}

      // For unary expressions
      operator bool_expression() { return bool_expression(lhs); }

      template <class R> bool_expression operator==(const R& rhs) { return bool_expression(lhs, compare_eq(), rhs); }
      template <class R> bool_expression operator!=(const R& rhs) { return bool_expression(lhs, compare_ne(), rhs); }
      template <class R> bool_expression operator< (const R& rhs) { return bool_expression(lhs, compare_lt(), rhs); }
      template <class R> bool_expression operator<=(const R& rhs) { return bool_expression(lhs, compare_le(), rhs); }
      template <class R> bool_expression operator> (const R& rhs) { return bool_expression(lhs, compare_gt(), rhs); }
      template <class R> bool_expression operator>=(const R& rhs) { return bool_expression(lhs, compare_ge(), rhs); }

    private:
      T lhs;
    };

    struct expression_decomposer
    {
      template <typename T>
      expression_lhs<T> operator<<(const T& lhs)
      {
        return expression_lhs<T>(lhs);
      }
    };
  }

  /**
   * \brief Handler for assert failures.
   * 
   * This function is \c noreturn to permit the compiler to optimize the remaining code with the certainty that the
   * asserted condition is met.  The \c noexcept tells the compiler it doesn't have to produce stack unwinding
   * information for exceptions, giving another optimisation opportunity.
   */
  AWESOME_NORETURN
  void assert_failed(const char* file, int line, const char* function, const char* expr_str, detail::bool_expression expr) AWESOME_NOEXCEPT;
}

#define AWESOME_ASSERT(expr) \
  do { \
    ::AwesomeAssert::detail::bool_expression evalExpr = (::AwesomeAssert::detail::expression_decomposer() << expr); \
    if (!evalExpr) \
    { \
      ::AwesomeAssert::assert_failed(__FILE__, __LINE__, __FUNCTION__, #expr, evalExpr); \
    } \
  } while (0)

#undef AWESOME_NORETURN

#endif // __INCLUDED_AWESOME_ASSERT_HPP__
