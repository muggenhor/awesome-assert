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

#ifndef INCLUDED_AWESOME_ASSERT_HPP
#define INCLUDED_AWESOME_ASSERT_HPP

#include "awesome_export.h"
#include <iosfwd>
#include <iterator>
#include <stdexcept>

#if __cplusplus >= 201103L || (defined(_MSC_VER) && _MSC_VER >= 1700) /* Visual Studio 2012 supports 'override' */
  #define AWESOME_OVERRIDE override
#else
  #define AWESOME_OVERRIDE
#endif

#if __cplusplus >= 201402L || (defined(_MSC_VER) && _MSC_VER >= 1910) /* Visual Studio 2017 supports relaxed 'constexpr' */
  #define AWESOME_CXX14_CONSTEXPR constexpr
#else
  #define AWESOME_CXX14_CONSTEXPR
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

// Helps preventing C4910 on MSVC: '__declspec(dllexport)' and 'extern' are incompatible on an explicit instantiation
#if defined(AwesomeAssert_EXPORTS) && defined(_MSC_VER)
  #define AWESOME_IMPORT
#else
  #define AWESOME_IMPORT AWESOME_EXPORT
#endif

#include <type_traits>
#include <utility>

namespace AwesomeAssert
{
  struct stringifier;

  namespace detail
  {
    // std::unique_ptr<stringifier> variant that's marked for DLL export
    class AWESOME_EXPORT stringifier_ptr
    {
    public:
      constexpr stringifier_ptr() noexcept = default;
      constexpr explicit stringifier_ptr(stringifier* _p) noexcept
        : ptr(_p)
      {}
      constexpr stringifier_ptr(std::nullptr_t) noexcept
        : ptr(nullptr)
      {}

      AWESOME_CXX14_CONSTEXPR stringifier_ptr(stringifier_ptr&& rhs) noexcept
        : ptr(rhs.ptr)
      {
        rhs.ptr = nullptr;
      }

      ~stringifier_ptr() noexcept;

      friend void swap(stringifier_ptr& lhs, stringifier_ptr& rhs) noexcept
      {
        using std::swap;
        swap(lhs.ptr, rhs.ptr);
      }

      stringifier_ptr& operator=(stringifier_ptr rhs) noexcept
      {
        // NOTE: this implementation _requires_ the argument to be by value instead of rvalue reference!
        swap(*this, rhs);
        return *this;
      }

            stringifier& operator *()       noexcept { return *ptr; }
      const stringifier& operator *() const noexcept { return *ptr; }
            stringifier* operator->()       noexcept { return  ptr; }
      const stringifier* operator->() const noexcept { return  ptr; }
            stringifier*        get()       noexcept { return  ptr; }
      const stringifier*        get() const noexcept { return  ptr; }

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable:4800)
#endif
      explicit operator bool() const noexcept { return static_cast<bool>(ptr); }
#ifdef _MSC_VER
#pragma warning(pop)
#endif
      bool     operator !   () const noexcept { return !ptr; }

    private:
      stringifier* ptr = nullptr;
    };

    template <typename T, typename... Args>
    typename std::enable_if<
        std::is_base_of<stringifier, T>::value
      , detail::stringifier_ptr>::type
    prepend(detail::stringifier_ptr tail, Args&&... args);

    struct bool_expression;
  }

  struct AWESOME_EXPORT stringifier
  {
    virtual ~stringifier() noexcept;
    virtual std::ostream& convert(std::ostream& os) const = 0;

    // Necessary to prevent Visual Studio from defining the copy constructor and failing because stringifier_ptr isn't coypable
    stringifier(stringifier&&) = default;
    constexpr stringifier() noexcept = default;

  private:
    friend struct detail::bool_expression;
    // Must be inline to ensure the compiler has the full body available for constant propagation
    template <typename T, typename... Args>
    friend typename std::enable_if<
        std::is_base_of<stringifier, T>::value
      , detail::stringifier_ptr>::type
    detail::prepend(detail::stringifier_ptr tail, Args&&... args);

    detail::stringifier_ptr next;
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
      : val(std::forward<T>(val_))
    {}

    std::ostream& convert(std::ostream& os) const AWESOME_OVERRIDE
    {
      return os << val;
    }

  private:
    T val;
  };

  // Reduce amount of code needing to be duplicated across object files
  extern template struct AWESOME_IMPORT string_maker<bool>;
  extern template struct AWESOME_IMPORT string_maker<short>;
  extern template struct AWESOME_IMPORT string_maker<unsigned short>;
  extern template struct AWESOME_IMPORT string_maker<int>;
  extern template struct AWESOME_IMPORT string_maker<unsigned int>;
  extern template struct AWESOME_IMPORT string_maker<long>;
  extern template struct AWESOME_IMPORT string_maker<unsigned long>;
  extern template struct AWESOME_IMPORT string_maker<long long>;
  extern template struct AWESOME_IMPORT string_maker<unsigned long long>;
  extern template struct AWESOME_IMPORT string_maker<float>;
  extern template struct AWESOME_IMPORT string_maker<double>;
  extern template struct AWESOME_IMPORT string_maker<long double>;
  extern template struct AWESOME_IMPORT string_maker<void*>;
  extern template struct AWESOME_IMPORT string_maker<const void*>;
  extern template struct AWESOME_IMPORT string_maker<char>;
  extern template struct AWESOME_IMPORT string_maker<signed char>;
  extern template struct AWESOME_IMPORT string_maker<unsigned char>;
  extern template struct AWESOME_IMPORT string_maker<const char*>;
  extern template struct AWESOME_IMPORT string_maker<const signed char*>;
  extern template struct AWESOME_IMPORT string_maker<const unsigned char*>;

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
    // Must be inline to ensure the compiler has the full body available for constant propagation
    inline stringifier_ptr::~stringifier_ptr() noexcept
    {
      delete ptr;
    }

    template <typename T, typename... Args>
    typename std::enable_if<
        std::is_base_of<stringifier, T>::value
      , detail::stringifier_ptr>::type
    prepend(detail::stringifier_ptr tail, Args&&... args)
    {
      detail::stringifier_ptr head(new T(std::forward<Args>(args)...));
      head->next = std::move(tail);
      return head;
    }

    template <typename T>
    struct expression_lhs;

    struct AWESOME_EXPORT bool_expression
    {
    private:
      template <typename T>
      static stringifier_ptr create_expression_list(T&& val)
      {
        return stringifier_ptr(new string_maker<T>(std::forward<T>(val)));
      }

      template <typename TL, typename TO, typename TR>
      static stringifier_ptr create_expression_list(TL&& lhs, TO&&, TR&& rhs)
      {
        // Constructing in reverse order because of the linked-list structure
        stringifier_ptr expr(new string_maker<TR>(std::forward<TR>(rhs)));
        expr = prepend<string_maker<TO>>(std::move(expr));
        expr = prepend<string_maker<TL>>(std::move(expr), std::forward<TL>(lhs));
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

      constexpr explicit bool_expression() = default;

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

      AWESOME_CXX14_CONSTEXPR bool_expression(bool_expression&& rhs) noexcept = default;
      bool_expression& operator=(bool_expression&& rhs) noexcept = default;

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
      stringifier_ptr fail_expression;
    };

    AWESOME_EXPORT std::ostream& operator<<(std::ostream& os, const bool_expression& expr);

    template <typename T>
    struct expression_lhs
    {
      constexpr expression_lhs(T lhs_)
        noexcept(noexcept(T{std::move(lhs_)}))
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
      friend constexpr auto operator<<(expression_lhs<T> lhs, R&& rhs)
        noexcept(noexcept(std::move(lhs.val) << std::forward<R>(rhs)))
        -> expression_lhs<decltype(T() << std::forward<R>(rhs))>
      {
        return std::move(lhs.val) << std::forward<R>(rhs);
      }
      template <typename R>
      friend constexpr auto operator>>(expression_lhs<T> lhs, R&& rhs)
        noexcept(noexcept(std::move(lhs.val) >> std::forward<R>(rhs)))
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
      constexpr
      expression_lhs<typename std::remove_reference<T>::type> operator<<(T&& lhs) const
        noexcept(noexcept(expression_lhs<typename std::remove_reference<T>::type>(std::forward<T>(lhs))))
      {
        return expression_lhs<typename std::remove_reference<T>::type>(std::forward<T>(lhs));
      }
    };
  }

  struct AWESOME_EXPORT violation_info
  {
    constexpr violation_info() noexcept = default;
    AWESOME_CXX14_CONSTEXPR violation_info(violation_info&& rhs) noexcept = default;

    AWESOME_CXX14_CONSTEXPR violation_info(
        const char*                     file
      , int                             line
      , const char*                     function
      , const char*                     expr_str
      , detail::bool_expression&&       expr
      ) noexcept
      : line_number{line}
      , file_name{file}
      , function_name{function}
      , comment{expr_str}
      , expression{std::move(expr)}
    {
    }

    int                     line_number   = -1;
    const char*             file_name     = nullptr;
    const char*             function_name = nullptr;
    const char*             comment       = nullptr;
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
