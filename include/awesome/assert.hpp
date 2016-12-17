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

#include <ostream>

#if __cplusplus >= 201103L
  #define AWESOME_NOEXCEPT noexcept
  #define AWESOME_OVERRIDE override
#else
  #define AWESOME_NOEXCEPT throw()
  #define AWESOME_OVERRIDE
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
  struct stringifier
  {
    virtual ~stringifier();
    virtual std::ostream& convert(std::ostream& os) const = 0;
  };

  /**
   * \brief String converter helper template class.
   *
   * Specialize this to use an alternate string conversion if desired.
   */
  template <typename T>
  struct string_maker : stringifier
  {
    string_maker(T val_)
      : val(val_)
    {}

    virtual std::ostream& convert(std::ostream& os) const AWESOME_OVERRIDE
    {
      return os << val;
    }

  private:
    T val;
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
  }

  template <> struct string_maker<detail::compare_eq> : stringifier { virtual std::ostream& convert(std::ostream& os) const AWESOME_OVERRIDE; };
  template <> struct string_maker<detail::compare_ne> : stringifier { virtual std::ostream& convert(std::ostream& os) const AWESOME_OVERRIDE; };
  template <> struct string_maker<detail::compare_lt> : stringifier { virtual std::ostream& convert(std::ostream& os) const AWESOME_OVERRIDE; };
  template <> struct string_maker<detail::compare_le> : stringifier { virtual std::ostream& convert(std::ostream& os) const AWESOME_OVERRIDE; };
  template <> struct string_maker<detail::compare_gt> : stringifier { virtual std::ostream& convert(std::ostream& os) const AWESOME_OVERRIDE; };
  template <> struct string_maker<detail::compare_ge> : stringifier { virtual std::ostream& convert(std::ostream& os) const AWESOME_OVERRIDE; };

  namespace detail
  {
    struct bool_expression
    {
    private:
      template <typename T>
      static stringifier** create_expression_list(const T& val)
      {
        stringifier** const expr = new stringifier*[1];
        try
        {
          expr[0] = new string_maker<T>(val);
          return expr;
        }
        catch (...)
        {
          delete [] expr;
          throw;
        }
      }

      template <typename TL, typename TO, typename TR>
      static stringifier** create_expression_list(const TL& lhs, const TO&, const TR& rhs)
      {
        stringifier** const expr = new stringifier*[3];
        expr[0] = expr[1] = expr[2] = NULL;
        try
        {
          expr[0] = new string_maker<TL>(lhs);
          expr[1] = new string_maker<TO>();
          expr[2] = new string_maker<TR>(rhs);
          return expr;
        }
        catch (...)
        {
          delete expr[2];
          delete expr[1];
          delete expr[0];
          delete [] expr;
          throw;
        }
      }

    public:
      typedef stringifier const* const* const_iterator;
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
      //! Storing string convertors instead of strings to prevent inlining of conversion code.
      stringifier** fail_expression;
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

#if defined(__GNUC__)
# define AWESOME_UNLIKELY(x) __builtin_expect(x, 0)
#else
# define AWESOME_UNLIKELY(x) x
#endif

#define AWESOME_ASSERT(expr) \
  do { \
    ::AwesomeAssert::detail::bool_expression evalExpr = (::AwesomeAssert::detail::expression_decomposer() << expr); \
    if (AWESOME_UNLIKELY(!evalExpr)) \
    { \
      ::AwesomeAssert::assert_failed(__FILE__, __LINE__, __FUNCTION__, #expr, evalExpr); \
    } \
  } while (0)

#undef AWESOME_NORETURN

#endif // __INCLUDED_AWESOME_ASSERT_HPP__
