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
#include <functional>
#include <iosfwd>
#include <iterator>
#include <stdexcept>

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
      stringifier_ptr() = default;
      constexpr explicit stringifier_ptr(stringifier* _p) noexcept
        : ptr(_p)
      {}
      constexpr stringifier_ptr(std::nullptr_t) noexcept
        : ptr(nullptr)
      {}

      constexpr stringifier_ptr(stringifier_ptr&& rhs) noexcept
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

      constexpr       stringifier& operator *()       noexcept { return *ptr; }
      constexpr const stringifier& operator *() const noexcept { return *ptr; }
      constexpr       stringifier* operator->()       noexcept { return  ptr; }
      constexpr const stringifier* operator->() const noexcept { return  ptr; }
      constexpr       stringifier*        get()       noexcept { return  ptr; }
      constexpr const stringifier*        get() const noexcept { return  ptr; }

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable:4800)
#endif
      constexpr explicit operator bool() const noexcept { return static_cast<bool>(ptr); }
#ifdef _MSC_VER
#pragma warning(pop)
#endif
      constexpr bool     operator !   () const noexcept { return !ptr; }

    private:
      stringifier* ptr = nullptr;
    };

    template <typename T>
    auto prepend(detail::stringifier_ptr tail, T&& obj);

    struct bool_expression;
  }

  struct AWESOME_EXPORT stringifier
  {
    virtual ~stringifier() noexcept;
    virtual std::ostream& convert(std::ostream& os) const = 0;

    // Necessary to prevent Visual Studio from defining the copy constructor and failing because stringifier_ptr isn't coypable
    stringifier(stringifier&&) = default;
    stringifier() = default;

  private:
    friend struct detail::bool_expression;
    // Must be inline to ensure the compiler has the full body available for constant propagation
    template <typename T>
    friend auto detail::prepend(detail::stringifier_ptr tail, T&& obj);

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
    constexpr explicit string_maker(T val_)
      : val(std::forward<T>(val_))
    {}

    std::ostream& convert(std::ostream& os) const override
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
    //! Internal marker type in the hierarchy for retrieving operators
    struct AWESOME_EXPORT string_maker_op : stringifier
    {
      std::ostream& convert(std::ostream& os) const override;
      virtual const char* str() const noexcept = 0;
    };
  }

  template <> struct AWESOME_EXPORT string_maker<::std::    equal_to <>> : detail::string_maker_op { const char* str() const noexcept override; };
  template <> struct AWESOME_EXPORT string_maker<::std::not_equal_to <>> : detail::string_maker_op { const char* str() const noexcept override; };
  template <> struct AWESOME_EXPORT string_maker<::std::   less      <>> : detail::string_maker_op { const char* str() const noexcept override; };
  template <> struct AWESOME_EXPORT string_maker<::std::   less_equal<>> : detail::string_maker_op { const char* str() const noexcept override; };
  template <> struct AWESOME_EXPORT string_maker<::std::greater      <>> : detail::string_maker_op { const char* str() const noexcept override; };
  template <> struct AWESOME_EXPORT string_maker<::std::greater_equal<>> : detail::string_maker_op { const char* str() const noexcept override; };
  template <> struct AWESOME_EXPORT string_maker<::std::    bit_and  <>> : detail::string_maker_op { const char* str() const noexcept override; };
  template <> struct AWESOME_EXPORT string_maker<::std::logical_and  <>> : detail::string_maker_op { const char* str() const noexcept override; };
  template <> struct AWESOME_EXPORT string_maker<::std::logical_or   <>> : detail::string_maker_op { const char* str() const noexcept override; };

  namespace detail
  {
    // Must be inline to ensure the compiler has the full body available for constant propagation
    inline stringifier_ptr::~stringifier_ptr() noexcept
    {
      delete ptr;
    }

    template <typename T>
    auto create_expression_list(T&& val)
    {
      return stringifier_ptr(new string_maker<T>(std::forward<T>(val)));
    }

    // Overloads that *don't* forward their parameter to the string_maker constructor.
    inline auto create_expression_list(::std::    equal_to <> val) { return stringifier_ptr(new string_maker<decltype(val)>()); }
    inline auto create_expression_list(::std::not_equal_to <> val) { return stringifier_ptr(new string_maker<decltype(val)>()); }
    inline auto create_expression_list(::std::   less      <> val) { return stringifier_ptr(new string_maker<decltype(val)>()); }
    inline auto create_expression_list(::std::   less_equal<> val) { return stringifier_ptr(new string_maker<decltype(val)>()); }
    inline auto create_expression_list(::std::greater      <> val) { return stringifier_ptr(new string_maker<decltype(val)>()); }
    inline auto create_expression_list(::std::greater_equal<> val) { return stringifier_ptr(new string_maker<decltype(val)>()); }
    inline auto create_expression_list(::std::    bit_and  <> val) { return stringifier_ptr(new string_maker<decltype(val)>()); }
    inline auto create_expression_list(::std::logical_and  <> val) { return stringifier_ptr(new string_maker<decltype(val)>()); }
    inline auto create_expression_list(::std::logical_or   <> val) { return stringifier_ptr(new string_maker<decltype(val)>()); }

    template <typename T>
    auto prepend(detail::stringifier_ptr tail, T&& obj)
    {
      detail::stringifier_ptr head(create_expression_list(std::forward<T>(obj)));

      // Find the end of the prepended expression
      auto cur = head.get();
      while (cur->next) cur = cur->next.get();

      cur->next = std::move(tail);
      return head;
    }

    template <typename T>
    struct expression_lhs;

    template <typename T>
    struct expression_rhs;

    template <typename TL, typename TO, typename TR>
    struct expression_binary;

    template <typename T>
    struct is_unary_expression : std::false_type {};

    template <typename T>
    struct is_unary_expression<expression_lhs<T>> : std::true_type {};

    template <typename T>
    struct is_unary_expression<expression_rhs<T>> : std::true_type {};

    template <typename T>
    struct is_binary_expression : std::false_type {};

    template <typename... Ts>
    struct is_binary_expression<expression_binary<Ts...>> : std::true_type {};

    template <typename T>
    struct is_expression : std::integral_constant<bool, is_unary_expression<T>::value || is_binary_expression<T>::value> {};

    struct AWESOME_EXPORT bool_expression
    {
    public:
      struct AWESOME_EXPORT const_iterator
      {
        // Type aliases required until C++20 for iterators
        using iterator_category = std::forward_iterator_tag;
        using value_type        = const stringifier;
        using difference_type   = std::ptrdiff_t;
        using pointer           = const stringifier*;
        using reference         = const stringifier&;

        explicit const_iterator() = default;
        constexpr explicit const_iterator(const stringifier* cur_) noexcept
          : cur{cur_}
        {
        }

        constexpr const_iterator& operator++() noexcept
        {
          cur = cur->next.get();
          return *this;
        }

        constexpr const_iterator operator++(int) noexcept
        {
          auto prev = *this;
          ++*this;
          return prev;
        }

        constexpr stringifier const& operator *() const noexcept { return *cur; }
        constexpr stringifier const* operator->() const noexcept { return  cur; }

        constexpr bool operator==(const const_iterator& rhs) const noexcept
        {
          return this->cur == rhs.cur;
        }

        constexpr bool operator!=(const const_iterator& rhs) const noexcept
        {
          return !(*this == rhs);
        }

      private:
        //! non-owning pointer, raw pointers are never owners
        const stringifier* cur = nullptr;
      };

      using iterator = const_iterator;

      explicit bool_expression() = default;

      template <typename T, typename std::enable_if<is_expression<typename std::remove_cv<typename std::remove_reference<T>::type>::type>::value>::type* = nullptr>
      explicit constexpr bool_expression(T expr)
        : fail_expression(expr ? nullptr : create_expression_list(std::move(expr)))
      {
      }

      bool_expression(bool_expression&& rhs) = default;
      bool_expression& operator=(bool_expression&& rhs) = default;

      constexpr const_iterator begin() const noexcept
      {
        return const_iterator{fail_expression.get()};
      }

      constexpr const_iterator end() const noexcept
      {
        return const_iterator{};
      }

      // Must be inline, along with all code that can potentially change fail_expression's value.
      // This to ensure the compiler has the opportunity to determine that the asserted condition
      // is equal to fail_expression not being NULL.
      constexpr explicit operator bool() const noexcept
      {
        return !fail_expression;
      }

    private:
      //! Storing string converters instead of strings to prevent inlining of conversion code.
      //! Either \c NULL or terminated with a \c NULL sentinel. This removes the need for a separate
      //! size field, which would increase code size for setting up and copying that field.
      stringifier_ptr fail_expression;
    };

    AWESOME_EXPORT std::ostream& operator<<(std::ostream& os, const bool_expression& expr);

    template <typename TL, typename TO, typename TR>
    struct expression_binary
    {
      explicit constexpr operator bool() const
        noexcept(noexcept(std::declval<TO>()(std::declval<TL>(), std::declval<TR>())))
      {
        return op(lhs, rhs);
      }

      friend auto create_expression_list(expression_binary&& expr)
      {
        // Constructing in reverse order because of the linked-list structure
        stringifier_ptr str_expr(create_expression_list(std::forward<TR>(expr.rhs)));
        str_expr = prepend(std::move(str_expr), std::forward<TO>(expr.op));
        str_expr = prepend(std::move(str_expr), std::forward<TL>(expr.lhs));
        return str_expr;
      }

      //! Don't use assignment operators in assertions!
      template <typename R>
      void operator=(R&&) = delete;

      template <typename UL, typename UO, typename UR>
      constexpr auto operator&&(expression_binary<UL, UO, UR>&& rhs_) &&
        noexcept(std::is_nothrow_move_constructible<expression_binary<TL, TO, TR>>::value
              && std::is_nothrow_move_constructible<expression_binary<UL, UO, UR>>::value)
      {
        return expression_binary<
            expression_binary<TL, TO, TR>
          , std::logical_and<>
          , expression_binary<UL, UO, UR>
          >{
            std::move(*this),
            std::move(rhs_),
        };
      }

      template <typename UL, typename UO, typename UR>
      constexpr auto operator||(expression_binary<UL, UO, UR>&& rhs_) &&
        noexcept(std::is_nothrow_move_constructible<expression_binary<TL, TO, TR>>::value
              && std::is_nothrow_move_constructible<expression_binary<UL, UO, UR>>::value)
      {
        return expression_binary<
            expression_binary<TL, TO, TR>
          , std::logical_or<>
          , expression_binary<UL, UO, UR>
          >{
            std::move(*this),
            std::move(rhs_),
        };
      }

      TL lhs;
      TR rhs;
      TO op = {};
    };

    struct empty {};

    template <typename D>
    struct contextually_convertible_bool
    {
      explicit constexpr operator bool() const
          noexcept(std::is_nothrow_constructible<bool, const decltype(D::val)&>::value)
      {
        return static_cast<bool>(static_cast<const D&>(*this).val);
      }
    };

    // CRTP helper to make the deriving class D be contextually convertible to bool if it's member T
    // D::*val is convertible to bool.
    template <typename T, typename D>
    using optional_bool_convert_t = typename std::conditional<
        std::is_constructible<bool, const T&>::value
      , contextually_convertible_bool<D>
      , empty
      >::type;

    template <typename T>
    struct expression_lhs : optional_bool_convert_t<T, expression_lhs<T>>
    {
      constexpr expression_lhs(T lhs_)
        noexcept(std::is_nothrow_move_constructible<T>::value)
        : val(std::forward<T>(lhs_))
      {}

      friend auto create_expression_list(expression_lhs lhs)
      {
        return create_expression_list(std::forward<T>(lhs.val));
      }

      //! Don't use assignment operators in assertions!
      template <typename R>
      void operator=(R&&) = delete;

      template <class R, typename std::enable_if<!is_unary_expression<typename std::remove_cv<typename std::remove_reference<R>::type>::type>::value>::type* = nullptr>
      friend constexpr auto operator==(expression_lhs<T> lhs, R&& rhs) { return expression_binary<T, ::std::    equal_to <>, R>{std::forward<T>(lhs.val), std::forward<R>(rhs)}; }
      template <class R, typename std::enable_if<!is_unary_expression<typename std::remove_cv<typename std::remove_reference<R>::type>::type>::value>::type* = nullptr>
      friend constexpr auto operator!=(expression_lhs<T> lhs, R&& rhs) { return expression_binary<T, ::std::not_equal_to <>, R>{std::forward<T>(lhs.val), std::forward<R>(rhs)}; }
      template <class R, typename std::enable_if<!is_unary_expression<typename std::remove_cv<typename std::remove_reference<R>::type>::type>::value>::type* = nullptr>
      friend constexpr auto operator< (expression_lhs<T> lhs, R&& rhs) { return expression_binary<T, ::std::   less      <>, R>{std::forward<T>(lhs.val), std::forward<R>(rhs)}; }
      template <class R, typename std::enable_if<!is_unary_expression<typename std::remove_cv<typename std::remove_reference<R>::type>::type>::value>::type* = nullptr>
      friend constexpr auto operator<=(expression_lhs<T> lhs, R&& rhs) { return expression_binary<T, ::std::   less_equal<>, R>{std::forward<T>(lhs.val), std::forward<R>(rhs)}; }
      template <class R, typename std::enable_if<!is_unary_expression<typename std::remove_cv<typename std::remove_reference<R>::type>::type>::value>::type* = nullptr>
      friend constexpr auto operator> (expression_lhs<T> lhs, R&& rhs) { return expression_binary<T, ::std::greater      <>, R>{std::forward<T>(lhs.val), std::forward<R>(rhs)}; }
      template <class R, typename std::enable_if<!is_unary_expression<typename std::remove_cv<typename std::remove_reference<R>::type>::type>::value>::type* = nullptr>
      friend constexpr auto operator>=(expression_lhs<T> lhs, R&& rhs) { return expression_binary<T, ::std::greater_equal<>, R>{std::forward<T>(lhs.val), std::forward<R>(rhs)}; }
      template <class R, typename std::enable_if<!is_unary_expression<typename std::remove_cv<typename std::remove_reference<R>::type>::type>::value>::type* = nullptr>
      friend constexpr auto operator& (expression_lhs<T> lhs, R&& rhs) { return expression_binary<T, ::std::    bit_and  <>, R>{std::forward<T>(lhs.val), std::forward<R>(rhs)}; }

      template <typename R, typename std::enable_if<!is_unary_expression<typename std::remove_cv<typename std::remove_reference<R>::type>::type>::value>::type* = nullptr>
      friend constexpr auto operator&&(expression_lhs<T> lhs, R&& rhs)
      {
        static_assert(
            !std::is_same<R, bool>::value, "Expression too complex: rewrite as binary comparison"
        );
        return expression_binary<T, ::std::logical_and<>, R>{
          std::forward<T>(lhs.val),
          std::forward<R>(rhs),
        };
      }

      template <typename R, typename std::enable_if<!is_unary_expression<typename std::remove_cv<typename std::remove_reference<R>::type>::type>::value>::type* = nullptr>
      friend constexpr auto operator||(expression_lhs<T> lhs, R&& rhs)
      {
        static_assert(
            !std::is_same<R, bool>::value, "Expression too complex: rewrite as binary comparison"
        );
        return expression_binary<T, ::std::logical_or<>, R>{
          std::forward<T>(lhs.val),
          std::forward<R>(rhs),
        };
      }

      // Necessary to permit usage of these in expressions. They should be allowed because they
      // have higher precedence than comparison operators, but they're not without this because we
      // use left shift in expression_decomposer.
      template <typename R, typename std::enable_if<!is_unary_expression<typename std::remove_cv<typename std::remove_reference<R>::type>::type>::value>::type* = nullptr>
      friend constexpr auto operator<<(expression_lhs<T> lhs, R&& rhs)
        noexcept(noexcept(std::declval<T>() << std::declval<R>()))
        -> expression_lhs<decltype(std::declval<T>() << std::forward<R>(rhs))>
      {
        return std::move(lhs.val) << std::forward<R>(rhs);
      }
      template <typename R, typename std::enable_if<!is_unary_expression<typename std::remove_cv<typename std::remove_reference<R>::type>::type>::value>::type* = nullptr>
      friend constexpr auto operator>>(expression_lhs<T> lhs, R&& rhs)
        noexcept(noexcept(std::declval<T>() >> std::declval<R>()))
        -> expression_lhs<decltype(std::declval<T>() >> std::forward<R>(rhs))>
      {
        return std::move(lhs.val) >> std::forward<R>(rhs);
      }

    private:
      friend contextually_convertible_bool<expression_lhs<T>>;
      template <typename>
      friend struct expression_rhs;
      T val;
    };

    template <typename T>
    struct expression_rhs : optional_bool_convert_t<T, expression_rhs<T>>
    {
      constexpr expression_rhs(T rhs_)
        noexcept(std::is_nothrow_move_constructible<T>::value)
        : val(std::forward<T>(rhs_))
      {}

      friend auto create_expression_list(expression_rhs rhs)
      {
        return create_expression_list(std::forward<T>(rhs.val));
      }

      //! Don't use assignment operators in assertions!
      template <typename R>
      void operator=(R&&) = delete;

      template <class L, typename std::enable_if<!is_unary_expression<typename std::remove_cv<typename std::remove_reference<L>::type>::type>::value>::type* = nullptr>
      friend constexpr auto operator==(L&& lhs, expression_rhs<T> rhs) { return expression_binary<L, ::std::    equal_to <>, T>{std::forward<L>(lhs), std::forward<T>(rhs.val)}; }
      template <class L, typename std::enable_if<!is_unary_expression<typename std::remove_cv<typename std::remove_reference<L>::type>::type>::value>::type* = nullptr>
      friend constexpr auto operator!=(L&& lhs, expression_rhs<T> rhs) { return expression_binary<L, ::std::not_equal_to <>, T>{std::forward<L>(lhs), std::forward<T>(rhs.val)}; }
      template <class L, typename std::enable_if<!is_unary_expression<typename std::remove_cv<typename std::remove_reference<L>::type>::type>::value>::type* = nullptr>
      friend constexpr auto operator< (L&& lhs, expression_rhs<T> rhs) { return expression_binary<L, ::std::   less      <>, T>{std::forward<L>(lhs), std::forward<T>(rhs.val)}; }
      template <class L, typename std::enable_if<!is_unary_expression<typename std::remove_cv<typename std::remove_reference<L>::type>::type>::value>::type* = nullptr>
      friend constexpr auto operator<=(L&& lhs, expression_rhs<T> rhs) { return expression_binary<L, ::std::   less_equal<>, T>{std::forward<L>(lhs), std::forward<T>(rhs.val)}; }
      template <class L, typename std::enable_if<!is_unary_expression<typename std::remove_cv<typename std::remove_reference<L>::type>::type>::value>::type* = nullptr>
      friend constexpr auto operator> (L&& lhs, expression_rhs<T> rhs) { return expression_binary<L, ::std::greater      <>, T>{std::forward<L>(lhs), std::forward<T>(rhs.val)}; }
      template <class L, typename std::enable_if<!is_unary_expression<typename std::remove_cv<typename std::remove_reference<L>::type>::type>::value>::type* = nullptr>
      friend constexpr auto operator>=(L&& lhs, expression_rhs<T> rhs) { return expression_binary<L, ::std::greater_equal<>, T>{std::forward<L>(lhs), std::forward<T>(rhs.val)}; }
      template <class L, typename std::enable_if<!is_unary_expression<typename std::remove_cv<typename std::remove_reference<L>::type>::type>::value>::type* = nullptr>
      friend constexpr auto operator& (L&& lhs, expression_rhs<T> rhs) { return expression_binary<L, ::std::    bit_and  <>, T>{std::forward<L>(lhs), std::forward<T>(rhs.val)}; }

      template <typename L, typename std::enable_if<!is_unary_expression<typename std::remove_cv<typename std::remove_reference<L>::type>::type>::value>::type* = nullptr>
      friend constexpr auto operator&&(L&& lhs, expression_rhs<T> rhs)
        noexcept(std::is_nothrow_move_constructible<L>::value
              && std::is_nothrow_move_constructible<T>::value)
      {
        static_assert(
            !std::is_same<L, bool>::value, "Expression too complex: rewrite as binary comparison"
        );
        return expression_binary<L, ::std::logical_and<>, T>{
          std::forward<L>(lhs),
          std::forward<T>(rhs.val),
        };
      }

      template <typename L, typename std::enable_if<!is_unary_expression<typename std::remove_cv<typename std::remove_reference<L>::type>::type>::value>::type* = nullptr>
      friend constexpr auto operator||(L&& lhs, expression_rhs<T> rhs)
        noexcept(std::is_nothrow_move_constructible<L>::value
              && std::is_nothrow_move_constructible<T>::value)
      {
        static_assert(
            !std::is_same<L, bool>::value, "Expression too complex: rewrite as binary comparison"
        );
        return expression_binary<L, ::std::logical_or<>, T>{
          std::forward<L>(lhs),
          std::forward<T>(rhs.val),
        };
      }

      template <class L> friend constexpr auto operator==(expression_lhs<L> lhs, expression_rhs<T> rhs) { return expression_binary<L, ::std::    equal_to <>, T>{std::forward<L>(lhs.val), std::forward<T>(rhs.val)}; }
      template <class L> friend constexpr auto operator!=(expression_lhs<L> lhs, expression_rhs<T> rhs) { return expression_binary<L, ::std::not_equal_to <>, T>{std::forward<L>(lhs.val), std::forward<T>(rhs.val)}; }
      template <class L> friend constexpr auto operator< (expression_lhs<L> lhs, expression_rhs<T> rhs) { return expression_binary<L, ::std::   less      <>, T>{std::forward<L>(lhs.val), std::forward<T>(rhs.val)}; }
      template <class L> friend constexpr auto operator<=(expression_lhs<L> lhs, expression_rhs<T> rhs) { return expression_binary<L, ::std::   less_equal<>, T>{std::forward<L>(lhs.val), std::forward<T>(rhs.val)}; }
      template <class L> friend constexpr auto operator> (expression_lhs<L> lhs, expression_rhs<T> rhs) { return expression_binary<L, ::std::greater      <>, T>{std::forward<L>(lhs.val), std::forward<T>(rhs.val)}; }
      template <class L> friend constexpr auto operator>=(expression_lhs<L> lhs, expression_rhs<T> rhs) { return expression_binary<L, ::std::greater_equal<>, T>{std::forward<L>(lhs.val), std::forward<T>(rhs.val)}; }
      template <class L> friend constexpr auto operator& (expression_lhs<L> lhs, expression_rhs<T> rhs) { return expression_binary<L, ::std::    bit_and  <>, T>{std::forward<L>(lhs.val), std::forward<T>(rhs.val)}; }
      template <class L> friend constexpr auto operator&&(expression_lhs<L> lhs, expression_rhs<T> rhs) { return expression_binary<L, ::std::logical_and  <>, T>{std::forward<L>(lhs.val), std::forward<T>(rhs.val)}; }
      template <class L> friend constexpr auto operator||(expression_lhs<L> lhs, expression_rhs<T> rhs) { return expression_binary<L, ::std::logical_or   <>, T>{std::forward<L>(lhs.val), std::forward<T>(rhs.val)}; }

      // Necessary to permit usage of these in expressions. They should be allowed because they
      // have higher precedence than comparison operators, but they're not without this because we
      // use left shift in expression_decomposer.
      template <typename L, typename std::enable_if<!is_unary_expression<typename std::remove_cv<typename std::remove_reference<L>::type>::type>::value>::type* = nullptr>
      friend constexpr auto operator<<(L&& lhs, expression_rhs<T> rhs)
        noexcept(noexcept(std::declval<L>() << std::declval<T>()))
        -> expression_rhs<decltype(std::forward<L>(lhs) << std::declval<T>())>
      {
        return std::forward<L>(lhs) << std::move(rhs.val);
      }
      template <typename L, typename std::enable_if<!is_unary_expression<typename std::remove_cv<typename std::remove_reference<L>::type>::type>::value>::type* = nullptr>
      friend constexpr auto operator>>(L&& lhs, expression_rhs<T> rhs)
        noexcept(noexcept(std::declval<L>() >> std::declval<T>()))
        -> expression_rhs<decltype(std::forward<L>(lhs) >> std::declval<T>())>
      {
        return std::forward<L>(lhs) >> std::move(rhs.val);
      }

      template <typename L>
      friend constexpr auto operator<<(expression_lhs<L> lhs, expression_rhs<T> rhs)
        noexcept(std::declval<L>() << std::declval<T>())
        -> expression_lhs<decltype(std::declval<L>() << std::declval<T>())>
      {
        return std::forward<L>(lhs.val) << std::forward<T>(rhs.val);
      }

      template <typename L>
      friend constexpr auto operator>>(expression_lhs<L> lhs, expression_rhs<T> rhs)
        noexcept(std::declval<L>() >> std::declval<T>())
        -> expression_lhs<decltype(std::declval<L>() >> std::declval<T>())>
      {
        return std::forward<L>(lhs.val) >> std::forward<T>(rhs.val);
      }

    private:
      friend contextually_convertible_bool<expression_rhs<T>>;
      T val;
    };

    struct expression_decomposer
    {
      template <typename T>
      using storage_type = typename std::conditional<
          std::is_array<typename std::remove_reference<T>::type>::value
        , T
        , typename std::remove_reference<T>::type
        >::type;

      template <
          typename T
        , typename std::enable_if<!std::is_same<
              typename std::remove_cv<typename std::remove_reference<T>::type>::type
            , expression_decomposer
            >::value>::type* = nullptr>
      constexpr
      auto operator<<(T&& lhs) const
        noexcept(std::is_nothrow_move_constructible<T>::value)
      {
        return expression_lhs<storage_type<T>>(std::forward<T>(lhs));
      }

      template <
          typename T
        , typename std::enable_if<!std::is_same<
              typename std::remove_cv<typename std::remove_reference<T>::type>::type
            , expression_decomposer
            >::value>::type* = nullptr>
      constexpr
      friend auto operator<<(T&& rhs, const expression_decomposer&)
        noexcept(std::is_nothrow_move_constructible<T>::value)
      {
        return expression_rhs<storage_type<T>>(std::forward<T>(rhs));
      }
    };
  }

  struct AWESOME_EXPORT violation_info
  {
    violation_info() = default;
    violation_info(violation_info&& rhs) = default;

    constexpr violation_info(
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
  [[noreturn]] AWESOME_EXPORT
  void assert_failed_precondition(
      const char*                     file
    , int                             line
    , const char*                     function
    , const char*                     expr_str
    , detail::bool_expression         expr
    ) AWESOME_PRECONDITION_NOEXCEPT;

  [[noreturn]] AWESOME_EXPORT
  void assert_failed_invariant(
      const char*                     file
    , int                             line
    , const char*                     function
    , const char*                     expr_str
    , detail::bool_expression         expr
    ) AWESOME_INVARIANT_NOEXCEPT;

  [[noreturn]] AWESOME_EXPORT
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
    ::AwesomeAssert::detail::bool_expression evalExpr(::AwesomeAssert::detail::expression_decomposer() \
        << expr << ::AwesomeAssert::detail::expression_decomposer()); \
    if (AWESOME_UNLIKELY(!evalExpr)) \
    { \
      fail_func(__FILE__, __LINE__, AWESOME_FUNCTION, #expr, std::move(evalExpr)); \
    } \
  } while (0)

#ifdef __clang__
  #define AWESOME_ASSERT_PROXY(fail_func, expr) \
    do { \
      _Pragma("clang diagnostic push") \
      _Pragma("clang diagnostic ignored \"-Wshift-op-parentheses\"") \
      _Pragma("clang diagnostic ignored \"-Woverloaded-shift-op-parentheses\"") \
      AWESOME_ASSERT_IMPL(fail_func, expr); \
      _Pragma("clang diagnostic pop") \
    } while (0)
#elif defined(__GNUC__)
  #define AWESOME_ASSERT_PROXY(fail_func, expr) \
    do { \
      _Pragma("GCC diagnostic push") \
      _Pragma("GCC diagnostic ignored \"-Wparentheses\"") \
      AWESOME_ASSERT_IMPL(fail_func, expr); \
      _Pragma("GCC diagnostic pop") \
    } while (0)
#else
  #define AWESOME_ASSERT_PROXY(fail_func, expr) AWESOME_ASSERT_IMPL(fail_func, expr)
#endif

#define AWESOME_EXPECTS(expr) AWESOME_ASSERT_PROXY(::AwesomeAssert::assert_failed_precondition , expr)
#define AWESOME_ASSERT(expr)  AWESOME_ASSERT_PROXY(::AwesomeAssert::assert_failed_invariant    , expr)
#define AWESOME_ENSURES(expr) AWESOME_ASSERT_PROXY(::AwesomeAssert::assert_failed_postcondition, expr)

#endif // INCLUDED_AWESOME_ASSERT_HPP
