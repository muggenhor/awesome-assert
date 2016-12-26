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
#include <stdexcept>

#if __cplusplus >= 201103L
  #define AWESOME_NOEXCEPT noexcept
  #define AWESOME_OVERRIDE override
#else
  #define AWESOME_NOEXCEPT throw()
  #define AWESOME_OVERRIDE
#endif

#ifndef AWESOME_PRECONDITION_NO_NOEXCEPT
  #define AWESOME_PRECONDITION_NOEXCEPT AWESOME_NOEXCEPT
#else
  #define AWESOME_PRECONDITION_NOEXCEPT
#endif

#ifndef AWESOME_INVARIANT_NO_NOEXCEPT
  #define AWESOME_INVARIANT_NOEXCEPT AWESOME_NOEXCEPT
#else
  #define AWESOME_INVARIANT_NOEXCEPT
#endif

#ifndef AWESOME_POSTCONDITION_NO_NOEXCEPT
  #define AWESOME_POSTCONDITION_NOEXCEPT AWESOME_NOEXCEPT
#else
  #define AWESOME_POSTCONDITION_NOEXCEPT
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

#if __cplusplus >= 201103L
  #include <utility>
  #define AWESOME_FWD_REF(T) T&&
  #define AWESOME_MOVE(x) std::move(x)
  #define AWESOME_FWD(T, x) std::forward<T>(x)
  #define AWESOME_REMOVE_REF(T) typename std::remove_reference<T>::type
  #define AWESOME_RREF_OR_CONST &&
#else
  #define AWESOME_FWD_REF(T) const T&
  #define AWESOME_MOVE(x) x
  #define AWESOME_FWD(T, x) x
  #define AWESOME_REMOVE_REF(T) T
  #define AWESOME_RREF_OR_CONST const
#endif

namespace AwesomeAssert
{
  namespace detail
  {
    struct bool_expression;
  }

  struct AWESOME_EXPORT stringifier
  {
    stringifier() AWESOME_NOEXCEPT
      : next(NULL)
    {}
    virtual ~stringifier() AWESOME_NOEXCEPT;
    virtual std::ostream& convert(std::ostream& os) const = 0;

  private:
    friend struct detail::bool_expression;
    // Must be inline to ensure the compiler has the full body available for constant propagation
    stringifier* set_next(stringifier* const next_) AWESOME_NOEXCEPT
    {
      delete next;
      next = next_;
      return this;
    }

    stringifier* next;
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
      : val(AWESOME_MOVE(val_))
    {}

    virtual std::ostream& convert(std::ostream& os) const AWESOME_OVERRIDE
    {
      return os << val;
    }

  private:
    T val;
  };

  // Reduce amount of code needing to be duplicated across object files
#if __cplusplus >= 201103L
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
#else
  template <> AWESOME_EXPORT std::ostream& string_maker<bool                >::convert(std::ostream&) const;
  template <> AWESOME_EXPORT std::ostream& string_maker<short               >::convert(std::ostream&) const;
  template <> AWESOME_EXPORT std::ostream& string_maker<unsigned short      >::convert(std::ostream&) const;
  template <> AWESOME_EXPORT std::ostream& string_maker<int                 >::convert(std::ostream&) const;
  template <> AWESOME_EXPORT std::ostream& string_maker<unsigned int        >::convert(std::ostream&) const;
  template <> AWESOME_EXPORT std::ostream& string_maker<long                >::convert(std::ostream&) const;
  template <> AWESOME_EXPORT std::ostream& string_maker<unsigned long       >::convert(std::ostream&) const;
  template <> AWESOME_EXPORT std::ostream& string_maker<long long           >::convert(std::ostream&) const;
  template <> AWESOME_EXPORT std::ostream& string_maker<unsigned long long  >::convert(std::ostream&) const;
  template <> AWESOME_EXPORT std::ostream& string_maker<float               >::convert(std::ostream&) const;
  template <> AWESOME_EXPORT std::ostream& string_maker<double              >::convert(std::ostream&) const;
  template <> AWESOME_EXPORT std::ostream& string_maker<long double         >::convert(std::ostream&) const;
  template <> AWESOME_EXPORT std::ostream& string_maker<void*               >::convert(std::ostream&) const;
  template <> AWESOME_EXPORT std::ostream& string_maker<const void*         >::convert(std::ostream&) const;
  template <> AWESOME_EXPORT std::ostream& string_maker<char                >::convert(std::ostream&) const;
  template <> AWESOME_EXPORT std::ostream& string_maker<signed char         >::convert(std::ostream&) const;
  template <> AWESOME_EXPORT std::ostream& string_maker<unsigned char       >::convert(std::ostream&) const;
  template <> AWESOME_EXPORT std::ostream& string_maker<const char*         >::convert(std::ostream&) const;
  template <> AWESOME_EXPORT std::ostream& string_maker<const signed char*  >::convert(std::ostream&) const;
  template <> AWESOME_EXPORT std::ostream& string_maker<const unsigned char*>::convert(std::ostream&) const;
#endif

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
      virtual std::ostream& convert(std::ostream& os) const AWESOME_OVERRIDE;
      virtual const char* str() const AWESOME_NOEXCEPT = 0;
    };
  }

  template <> struct AWESOME_EXPORT string_maker<detail::compare_eq> : detail::string_maker_op { virtual const char* str() const AWESOME_NOEXCEPT AWESOME_OVERRIDE; };
  template <> struct AWESOME_EXPORT string_maker<detail::compare_ne> : detail::string_maker_op { virtual const char* str() const AWESOME_NOEXCEPT AWESOME_OVERRIDE; };
  template <> struct AWESOME_EXPORT string_maker<detail::compare_lt> : detail::string_maker_op { virtual const char* str() const AWESOME_NOEXCEPT AWESOME_OVERRIDE; };
  template <> struct AWESOME_EXPORT string_maker<detail::compare_le> : detail::string_maker_op { virtual const char* str() const AWESOME_NOEXCEPT AWESOME_OVERRIDE; };
  template <> struct AWESOME_EXPORT string_maker<detail::compare_gt> : detail::string_maker_op { virtual const char* str() const AWESOME_NOEXCEPT AWESOME_OVERRIDE; };
  template <> struct AWESOME_EXPORT string_maker<detail::compare_ge> : detail::string_maker_op { virtual const char* str() const AWESOME_NOEXCEPT AWESOME_OVERRIDE; };

  namespace detail
  {
    struct bool_expression
    {
    private:
      template <typename T>
      static stringifier* create_expression_list(AWESOME_FWD_REF(T) val)
      {
        return new string_maker<T>(AWESOME_FWD(T, val));
      }

      template <typename TL, typename TO, typename TR>
      static stringifier* create_expression_list(AWESOME_FWD_REF(TL) lhs, const TO&, AWESOME_FWD_REF(TR) rhs)
      {
        // Constructing in reverse order because of the linked-list structure
        stringifier* expr = new string_maker<TR>(AWESOME_FWD(TR, rhs));
        try
        {
          expr = (new string_maker<TO>)->set_next(expr);
          expr = (new string_maker<TL>(AWESOME_FWD(TL, lhs)))->set_next(expr);
          return expr;
        }
        catch (...)
        {
          delete expr;
          throw;
        }
      }

    public:
      struct const_iterator : std::iterator<std::forward_iterator_tag, const stringifier>
      {
        const_iterator() AWESOME_NOEXCEPT;
        const_iterator(const stringifier* cur_) AWESOME_NOEXCEPT;
        const_iterator& operator++() AWESOME_NOEXCEPT;
        const_iterator operator++(int) AWESOME_NOEXCEPT;
        stringifier const& operator*() const AWESOME_NOEXCEPT;
        stringifier const* operator->() const AWESOME_NOEXCEPT;
        bool operator==(const const_iterator& rhs) const AWESOME_NOEXCEPT;
        bool operator!=(const const_iterator& rhs) const AWESOME_NOEXCEPT;

      private:
        const stringifier* cur;
      };

      typedef const_iterator iterator;

      template <typename T>
      explicit bool_expression(AWESOME_FWD_REF(T) val)
        : fail_expression(val ? NULL : create_expression_list(AWESOME_FWD(T, val)))
      {
      }

      template <typename TL, typename TO, typename TR>
      bool_expression(
          AWESOME_FWD_REF(TL) lhs, AWESOME_FWD_REF(TO) op, AWESOME_FWD_REF(TR) rhs)
        : fail_expression(op(lhs, rhs) ? NULL : create_expression_list(AWESOME_FWD(TL, lhs), AWESOME_FWD(TO, op), AWESOME_FWD(TR, rhs)))
      {
      }

      bool_expression(AWESOME_FWD_REF(bool_expression) rhs) AWESOME_NOEXCEPT
        : fail_expression(rhs.fail_expression)
      {
        // Const cast necessary because this is a stealing copy constructor when on C++98.
        const_cast<bool_expression&>(rhs).fail_expression = NULL;
      }

      ~bool_expression() AWESOME_NOEXCEPT
      {
        delete fail_expression;
      }

      const_iterator begin() const AWESOME_NOEXCEPT;
      const_iterator end() const AWESOME_NOEXCEPT;

      // Must be inline, along with all code that can potentially change fail_expression's value.
      // This to ensure the compiler has the opportunity to determine that the asserted condition
      // is equal to fail_expression not being NULL.
      operator bool() const AWESOME_NOEXCEPT
      {
        return !fail_expression;
      }

#if __cplusplus >= 201103L
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
#endif

    private:
      //! Storing string converters instead of strings to prevent inlining of conversion code.
      //! Either \c NULL or terminated with a \c NULL sentinel. This removes the need for a separate
      //! size field, which would increase code size for setting up and copying that field.
      stringifier* fail_expression;
    };

    AWESOME_EXPORT std::ostream& operator<<(std::ostream& os, const bool_expression& expr);

    template <typename T>
    struct expression_lhs
    {
      expression_lhs(T lhs_)
        : lhs(AWESOME_MOVE(lhs_))
      {}

      // For unary expressions
      operator bool_expression() AWESOME_RREF_OR_CONST { return bool_expression(AWESOME_MOVE(lhs)); }

      template <class R> bool_expression operator==(AWESOME_FWD_REF(R) rhs) AWESOME_RREF_OR_CONST { return bool_expression(AWESOME_MOVE(lhs), compare_eq(), AWESOME_FWD(R, rhs)); }
      template <class R> bool_expression operator!=(AWESOME_FWD_REF(R) rhs) AWESOME_RREF_OR_CONST { return bool_expression(AWESOME_MOVE(lhs), compare_ne(), AWESOME_FWD(R, rhs)); }
      template <class R> bool_expression operator< (AWESOME_FWD_REF(R) rhs) AWESOME_RREF_OR_CONST { return bool_expression(AWESOME_MOVE(lhs), compare_lt(), AWESOME_FWD(R, rhs)); }
      template <class R> bool_expression operator<=(AWESOME_FWD_REF(R) rhs) AWESOME_RREF_OR_CONST { return bool_expression(AWESOME_MOVE(lhs), compare_le(), AWESOME_FWD(R, rhs)); }
      template <class R> bool_expression operator> (AWESOME_FWD_REF(R) rhs) AWESOME_RREF_OR_CONST { return bool_expression(AWESOME_MOVE(lhs), compare_gt(), AWESOME_FWD(R, rhs)); }
      template <class R> bool_expression operator>=(AWESOME_FWD_REF(R) rhs) AWESOME_RREF_OR_CONST { return bool_expression(AWESOME_MOVE(lhs), compare_ge(), AWESOME_FWD(R, rhs)); }

#if __cplusplus >= 201103L
      // Necessary to permit usage of these in expressions. They should be allowed because they
      // have higher precedence than comparison operators, but they're not without this because we
      // use left shift in expression_decomposer.
      template <typename R>
      auto operator<<(AWESOME_FWD_REF(R) rhs) AWESOME_RREF_OR_CONST
        -> expression_lhs<decltype(T() << AWESOME_FWD(R, rhs))>
      {
        return AWESOME_MOVE(lhs) << AWESOME_FWD(R, rhs);
      }
      template <typename R>
      auto operator>>(AWESOME_FWD_REF(R) rhs) AWESOME_RREF_OR_CONST
        -> expression_lhs<decltype(T() >> AWESOME_FWD(R, rhs))>
      {
        return AWESOME_MOVE(lhs) >> AWESOME_FWD(R, rhs);
      }
#endif

    private:
      T lhs;
    };

    struct expression_decomposer
    {
      template <typename T>
      expression_lhs<AWESOME_REMOVE_REF(T)> operator<<(AWESOME_FWD_REF(T) lhs)
      {
        return expression_lhs<AWESOME_REMOVE_REF(T)>(AWESOME_FWD(T, lhs));
      }
    };
  }

  struct AWESOME_EXPORT violation_info
  {
    violation_info() AWESOME_NOEXCEPT;
    violation_info(
        const char*                     file
      , int                             line
      , const char*                     function
      , const char*                     expr_str
      , detail::bool_expression         expr
      ) AWESOME_NOEXCEPT;
    violation_info(AWESOME_FWD_REF(violation_info) rhs) AWESOME_NOEXCEPT
      : line_number  (rhs.line_number  )
      , file_name    (rhs.file_name    )
      , function_name(rhs.function_name)
      , comment      (rhs.comment      )
      , expression   (AWESOME_MOVE(rhs.expression))
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
    ) AWESOME_NOEXCEPT;

  AWESOME_EXPORT
  void assert_fail_default_log(
      const violation_info&           info
    ) AWESOME_NOEXCEPT;

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
#elif __cplusplus >= 201103
# define AWESOME_FUNCTION __func__
#else
# define AWESOME_FUNCTION __FUNCTION__
#endif

#define AWESOME_ASSERT_IMPL(fail_func, expr) \
  do { \
    ::AwesomeAssert::detail::bool_expression evalExpr(::AwesomeAssert::detail::expression_decomposer() << expr); \
    if (AWESOME_UNLIKELY(!evalExpr)) \
    { \
      fail_func(__FILE__, __LINE__, AWESOME_FUNCTION, #expr, AWESOME_MOVE(evalExpr)); \
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

#undef AWESOME_FWD

#endif // INCLUDED_AWESOME_ASSERT_HPP
