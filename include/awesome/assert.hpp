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

#ifndef INCLUDED_AWESOME_ASSERT_HPP
#define INCLUDED_AWESOME_ASSERT_HPP

#include "awesome_export.h"
#include <iosfwd>
#include <iterator>
#include <memory>
#include <stdexcept>

#if __cplusplus >= 201103L || (defined(_MSC_VER) && _MSC_VER >= 1700) /* Visual Studio 2012 supports 'override' */
  #define AWESOME_OVERRIDE override
#else
  #define AWESOME_OVERRIDE
#endif

#ifndef AWESOME_PRECONDITION_NO_NOEXCEPT
  #define AWESOME_PRECONDITION_NOEXCEPT noexcept
#else
  #define AWESOME_PRECONDITION_NOEXCEPT
#endif

#ifndef AWESOME_INVARIANT_NO_NOEXCEPT
  #define AWESOME_INVARIANT_NOEXCEPT noexcept
#else
  #define AWESOME_INVARIANT_NOEXCEPT
#endif

#ifndef AWESOME_POSTCONDITION_NO_NOEXCEPT
  #define AWESOME_POSTCONDITION_NOEXCEPT noexcept
#else
  #define AWESOME_POSTCONDITION_NOEXCEPT
#endif

#if   defined(_MSC_VER)
  #define AWESOME_NORETURN __declspec(noreturn)
#elif defined(__GNUC__) || defined(__clang__)
  #define AWESOME_NORETURN __attribute__ ((__noreturn__))
#else
  // Unfortunately some versions of GCC (4.7) with -std=c++0x claim to be C++11
  // without supporting either attributes or the [[noreturn]] attribute. Which
  // is why we're using GCC's own __noreturn__ attribute above first.
  #define AWESOME_NORETURN [[noreturn]]
#endif

#include <utility>

namespace AwesomeAssert
{
  namespace detail
  {
    struct bool_expression;
  }

  struct AWESOME_EXPORT stringifier
  {
    virtual ~stringifier() noexcept;
    virtual std::ostream& convert(std::ostream& os) const = 0;

  private:
    friend struct detail::bool_expression;
    // Must be inline to ensure the compiler has the full body available for constant propagation
    stringifier* set_next(std::unique_ptr<stringifier> next_) noexcept
    {
      next = std::move(next_);
      return this;
    }

    std::unique_ptr<stringifier> next;
  };

  AWESOME_EXPORT std::ostream& operator<<(std::ostream& os, const stringifier& str);

  /**
   * \brief String converter helper template class.
   *
   * Specialize this to use an alternate string conversion if desired.
   */
  template <typename T>
  struct string_maker : stringifier
  {
    explicit string_maker(T val_)
      : val(std::move(val_))
    {}

    std::ostream& convert(std::ostream& os) const AWESOME_OVERRIDE
    {
      return os << val;
    }

  private:
    T val;
  };

  // Reduce amount of code needing to be duplicated across object files
  extern template struct AWESOME_EXPORT string_maker<bool>;
  extern template struct AWESOME_EXPORT string_maker<short>;
  extern template struct AWESOME_EXPORT string_maker<unsigned short>;
  extern template struct AWESOME_EXPORT string_maker<int>;
  extern template struct AWESOME_EXPORT string_maker<unsigned int>;
  extern template struct AWESOME_EXPORT string_maker<long>;
  extern template struct AWESOME_EXPORT string_maker<unsigned long>;
  extern template struct AWESOME_EXPORT string_maker<long long>;
  extern template struct AWESOME_EXPORT string_maker<unsigned long long>;
  extern template struct AWESOME_EXPORT string_maker<float>;
  extern template struct AWESOME_EXPORT string_maker<double>;
  extern template struct AWESOME_EXPORT string_maker<long double>;
  extern template struct AWESOME_EXPORT string_maker<void*>;
  extern template struct AWESOME_EXPORT string_maker<const void*>;
  extern template struct AWESOME_EXPORT string_maker<char>;
  extern template struct AWESOME_EXPORT string_maker<signed char>;
  extern template struct AWESOME_EXPORT string_maker<unsigned char>;
  extern template struct AWESOME_EXPORT string_maker<const char*>;
  extern template struct AWESOME_EXPORT string_maker<const signed char*>;
  extern template struct AWESOME_EXPORT string_maker<const unsigned char*>;

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

    //! Internal marker type in the hierarchy for retrieving operators
    struct AWESOME_EXPORT string_maker_op : stringifier
    {
      std::ostream& convert(std::ostream& os) const AWESOME_OVERRIDE;
      virtual const char* str() const noexcept = 0;
    };
  }

  template <> struct AWESOME_EXPORT string_maker<detail::compare_eq> : detail::string_maker_op { const char* str() const noexcept AWESOME_OVERRIDE; };
  template <> struct AWESOME_EXPORT string_maker<detail::compare_ne> : detail::string_maker_op { const char* str() const noexcept AWESOME_OVERRIDE; };
  template <> struct AWESOME_EXPORT string_maker<detail::compare_lt> : detail::string_maker_op { const char* str() const noexcept AWESOME_OVERRIDE; };
  template <> struct AWESOME_EXPORT string_maker<detail::compare_le> : detail::string_maker_op { const char* str() const noexcept AWESOME_OVERRIDE; };
  template <> struct AWESOME_EXPORT string_maker<detail::compare_gt> : detail::string_maker_op { const char* str() const noexcept AWESOME_OVERRIDE; };
  template <> struct AWESOME_EXPORT string_maker<detail::compare_ge> : detail::string_maker_op { const char* str() const noexcept AWESOME_OVERRIDE; };

  namespace detail
  {
    template <typename T>
    struct expression_lhs;

    struct AWESOME_EXPORT bool_expression
    {
    private:
      template <typename T>
      static std::unique_ptr<stringifier> create_expression_list(T&& val)
      {
        return std::unique_ptr<string_maker<T>>(new string_maker<T>(std::forward<T>(val)));
      }

      template <typename TL, typename TO, typename TR>
      static std::unique_ptr<stringifier> create_expression_list(TL&& lhs, TO&&, TR&& rhs)
      {
        // Constructing in reverse order because of the linked-list structure
        std::unique_ptr<stringifier> expr(new string_maker<TR>(std::forward<TR>(rhs)));
        expr.reset((new string_maker<TO>)->set_next(std::move(expr)));
        expr.reset((new string_maker<TL>(std::forward<TL>(lhs)))->set_next(std::move(expr)));
        return expr;
      }

    public:
      struct AWESOME_EXPORT const_iterator : std::iterator<std::forward_iterator_tag, const stringifier>
      {
        const_iterator() noexcept;
        const_iterator(const stringifier* cur_) noexcept;
        const_iterator& operator++() noexcept;
        const_iterator operator++(int) noexcept;
        stringifier const& operator*() const noexcept;
        stringifier const* operator->() const noexcept;
        bool operator==(const const_iterator& rhs) const noexcept;
        bool operator!=(const const_iterator& rhs) const noexcept;

      private:
        //! non-owning pointer, raw pointers are never owners
        const stringifier* cur;
      };

      typedef const_iterator iterator;

      // For unary expressions
      template <typename T>
      explicit bool_expression(expression_lhs<T> unaryExpr)
        : bool_expression(std::move(unaryExpr.val))
      {
      }

      template <typename T>
      explicit bool_expression(T&& val)
        : fail_expression(val ? nullptr : create_expression_list(std::forward<T>(val)))
      {
      }

      template <typename TL, typename TO, typename TR>
      bool_expression(TL&& lhs, TO&& op, TR&& rhs)
        : fail_expression(op(lhs, rhs) ? nullptr : create_expression_list(std::forward<TL>(lhs), std::forward<TO>(op), std::forward<TR>(rhs)))
      {
      }

      bool_expression(bool_expression&& rhs) noexcept
        : fail_expression(std::move(rhs.fail_expression))
      {}

      ~bool_expression() noexcept = default;

      const_iterator begin() const noexcept;
      const_iterator end() const noexcept;

      // Must be inline, along with all code that can potentially change fail_expression's value.
      // This to ensure the compiler has the opportunity to determine that the asserted condition
      // is equal to fail_expression not being NULL.
      explicit operator bool() const noexcept
      {
        return !fail_expression;
      }

    private:
      template <typename>
      struct TFalse
      {
        static constexpr bool val = false;
      };
    public:

      // forbidding expressions with lower precedence than comparison operators above: we only wish to deal with binary comparisons
      template <typename R> int operator&  (const R&) { static_assert(TFalse<R>::val, "Expression too complex: rewrite as binary comparison"); return int(); }
      template <typename R> int operator^  (const R&) { static_assert(TFalse<R>::val, "Expression too complex: rewrite as binary comparison"); return int(); }
      template <typename R> int operator|  (const R&) { static_assert(TFalse<R>::val, "Expression too complex: rewrite as binary comparison"); return int(); }
      template <typename R> int operator&& (const R&) { static_assert(TFalse<R>::val, "Expression too complex: rewrite as binary comparison"); return int(); }
      template <typename R> int operator|| (const R&) { static_assert(TFalse<R>::val, "Expression too complex: rewrite as binary comparison"); return int(); }
      template <typename R> int operator=  (const R&) { static_assert(TFalse<R>::val, "Expression too complex: rewrite as binary comparison"); return int(); }
      template <typename R> int operator+= (const R&) { static_assert(TFalse<R>::val, "Expression too complex: rewrite as binary comparison"); return int(); }
      template <typename R> int operator-= (const R&) { static_assert(TFalse<R>::val, "Expression too complex: rewrite as binary comparison"); return int(); }
      template <typename R> int operator*= (const R&) { static_assert(TFalse<R>::val, "Expression too complex: rewrite as binary comparison"); return int(); }
      template <typename R> int operator/= (const R&) { static_assert(TFalse<R>::val, "Expression too complex: rewrite as binary comparison"); return int(); }
      template <typename R> int operator%= (const R&) { static_assert(TFalse<R>::val, "Expression too complex: rewrite as binary comparison"); return int(); }
      template <typename R> int operator<<=(const R&) { static_assert(TFalse<R>::val, "Expression too complex: rewrite as binary comparison"); return int(); }
      template <typename R> int operator>>=(const R&) { static_assert(TFalse<R>::val, "Expression too complex: rewrite as binary comparison"); return int(); }
      template <typename R> int operator&= (const R&) { static_assert(TFalse<R>::val, "Expression too complex: rewrite as binary comparison"); return int(); }
      template <typename R> int operator^= (const R&) { static_assert(TFalse<R>::val, "Expression too complex: rewrite as binary comparison"); return int(); }
      template <typename R> int operator|= (const R&) { static_assert(TFalse<R>::val, "Expression too complex: rewrite as binary comparison"); return int(); }

    private:
      //! Storing string converters instead of strings to prevent inlining of conversion code.
      //! Either \c NULL or terminated with a \c NULL sentinel. This removes the need for a separate
      //! size field, which would increase code size for setting up and copying that field.
      std::unique_ptr<stringifier> fail_expression;
    };

    AWESOME_EXPORT std::ostream& operator<<(std::ostream& os, const bool_expression& expr);

    template <typename T>
    struct expression_lhs
    {
      expression_lhs(T lhs_)
        : val(std::move(lhs_))
      {}

      template <class R> friend bool_expression operator==(expression_lhs<T> lhs, R&& rhs) { return bool_expression(std::move(lhs.val), compare_eq(), std::forward<R>(rhs)); }
      template <class R> friend bool_expression operator!=(expression_lhs<T> lhs, R&& rhs) { return bool_expression(std::move(lhs.val), compare_ne(), std::forward<R>(rhs)); }
      template <class R> friend bool_expression operator< (expression_lhs<T> lhs, R&& rhs) { return bool_expression(std::move(lhs.val), compare_lt(), std::forward<R>(rhs)); }
      template <class R> friend bool_expression operator<=(expression_lhs<T> lhs, R&& rhs) { return bool_expression(std::move(lhs.val), compare_le(), std::forward<R>(rhs)); }
      template <class R> friend bool_expression operator> (expression_lhs<T> lhs, R&& rhs) { return bool_expression(std::move(lhs.val), compare_gt(), std::forward<R>(rhs)); }
      template <class R> friend bool_expression operator>=(expression_lhs<T> lhs, R&& rhs) { return bool_expression(std::move(lhs.val), compare_ge(), std::forward<R>(rhs)); }

      // Necessary to permit usage of these in expressions. They should be allowed because they
      // have higher precedence than comparison operators, but they're not without this because we
      // use left shift in expression_decomposer.
      template <typename R>
      friend auto operator<<(expression_lhs<T> lhs, R&& rhs)
        -> expression_lhs<decltype(T() << std::forward<R>(rhs))>
      {
        return std::move(lhs.val) << std::forward<R>(rhs);
      }
      template <typename R>
      friend auto operator>>(expression_lhs<T> lhs, R&& rhs)
        -> expression_lhs<decltype(T() >> std::forward<R>(rhs))>
      {
        return std::move(lhs.val) >> std::forward<R>(rhs);
      }

    private:
      friend bool_expression;
      T val;
    };

    struct expression_decomposer
    {
      template <typename T>
      expression_lhs<typename std::remove_reference<T>::type> operator<<(T&& lhs)
      {
        return expression_lhs<typename std::remove_reference<T>::type>(std::forward<T>(lhs));
      }
    };
  }

  struct AWESOME_EXPORT violation_info
  {
    violation_info() noexcept;
    violation_info(
        const char*                     file
      , int                             line
      , const char*                     function
      , const char*                     expr_str
      , detail::bool_expression         expr
      ) noexcept;
    violation_info(violation_info&& rhs) noexcept
      : line_number  (rhs.line_number  )
      , file_name    (rhs.file_name    )
      , function_name(rhs.function_name)
      , comment      (rhs.comment      )
      , expression   (std::move(rhs.expression))
    {
    }


    int                     line_number;
    const char*             file_name;
    const char*             function_name;
    const char*             comment;
    detail::bool_expression expression;
  };

#ifdef _MSC_VER
  // Apparently MSVC's C++ library doesn't properly DLL export it's std::exception hierarchy. It's warning about
  // exporting our exception class that derives from that without exporting the base. Nothing we can do about it, so
  // just ignore and hope for the best.
#pragma warning(push)
#pragma warning(disable:4275)
#endif
  class AWESOME_EXPORT precondition_error : public std::invalid_argument
  {
  public:
    precondition_error(violation_info info);

    friend AWESOME_EXPORT std::ostream& operator<<(std::ostream& os, const precondition_error& error);

  private:
    violation_info _info;
  };
#ifdef _MSC_VER
#pragma warning(pop)
#endif

  AWESOME_EXPORT
  std::ostream& assert_fail_default_log(
      std::ostream&                   os
    , const violation_info&           info
    ) noexcept;

  AWESOME_EXPORT
  void assert_fail_default_log(
      const violation_info&           info
    ) noexcept;

  /**
   * \brief Handlers for assert failures.
   *
   * These functions are \c noreturn to permit the compiler to optimize the remaining code with the certainty that the
   * asserted condition is met.  The \c noexcept tells the compiler it doesn't have to produce stack unwinding
   * information for exceptions, giving another optimisation opportunity.
   */
  AWESOME_NORETURN AWESOME_EXPORT
  void assert_failed_precondition(
      const char*                     file
    , int                             line
    , const char*                     function
    , const char*                     expr_str
    , detail::bool_expression         expr
    ) AWESOME_PRECONDITION_NOEXCEPT;

  AWESOME_NORETURN AWESOME_EXPORT
  void assert_failed_invariant(
      const char*                     file
    , int                             line
    , const char*                     function
    , const char*                     expr_str
    , detail::bool_expression         expr
    ) AWESOME_INVARIANT_NOEXCEPT;

  AWESOME_NORETURN AWESOME_EXPORT
  void assert_failed_postcondition(
      const char*                     file
    , int                             line
    , const char*                     function
    , const char*                     expr_str
    , detail::bool_expression         expr
    ) AWESOME_POSTCONDITION_NOEXCEPT;
}

#if defined(__GNUC__)
# define AWESOME_UNLIKELY(x) __builtin_expect(x, 0)
#else
# define AWESOME_UNLIKELY(x) x
#endif

#if defined(__GNUC__)
# define AWESOME_FUNCTION __PRETTY_FUNCTION__
#elif defined(__FUNCSIG__)
# define AWESOME_FUNCTION __FUNCSIG__
#else
# define AWESOME_FUNCTION __func__
#endif

#define AWESOME_ASSERT_IMPL(fail_func, expr) \
  do { \
    ::AwesomeAssert::detail::bool_expression evalExpr(::AwesomeAssert::detail::expression_decomposer() << expr); \
    if (AWESOME_UNLIKELY(!evalExpr)) \
    { \
      fail_func(__FILE__, __LINE__, AWESOME_FUNCTION, #expr, std::move(evalExpr)); \
    } \
  } while (0)

#ifdef __clang__
  #define AWESOME_ASSERT_PROXY(fail_func, expr) \
    do { \
      _Pragma("clang diagnostic push") \
      _Pragma("clang diagnostic ignored \"-Woverloaded-shift-op-parentheses\"") \
      AWESOME_ASSERT_IMPL(fail_func, expr); \
      _Pragma("clang diagnostic pop") \
    } while (0)
#else
  #define AWESOME_ASSERT_PROXY(fail_func, expr) AWESOME_ASSERT_IMPL(fail_func, expr)
#endif

#define AWESOME_EXPECTS(expr) AWESOME_ASSERT_PROXY(::AwesomeAssert::assert_failed_precondition , expr)
#define AWESOME_ASSERT(expr)  AWESOME_ASSERT_PROXY(::AwesomeAssert::assert_failed_invariant    , expr)
#define AWESOME_ENSURES(expr) AWESOME_ASSERT_PROXY(::AwesomeAssert::assert_failed_postcondition, expr)

#endif // INCLUDED_AWESOME_ASSERT_HPP
