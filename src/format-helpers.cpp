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
#include <awesome/assert/format-helpers.hpp>
#include <cstdlib>
#include <cstddef>
#include <cstdio>
#include <cstring>
#include <iostream>
#include <utility>
#include <vector>

#if __GLIBCXX__
  #include <ext/stdio_sync_filebuf.h>
#endif

#ifndef WIN32
  #include <unistd.h>
#endif

#if defined(__GNUC__) || defined(__clang__)
  #define AWESOME_ATTR_WEAK __attribute__((__weak__))
#else
  #define AWESOME_ATTR_WEAK
#endif

namespace AwesomeAssert
{
  namespace
  {
    class expression_colorizer
    {
    struct token_t
    {
      msg         msg_type  = msg::expression;
      const char* str       = nullptr;
      std::size_t len       = 0;
    };

    public:
      expression_colorizer(const char* const expr_str, const detail::bool_expression& expr)
      : _tokens{
          {msg::expression, expr_str, std::strlen(expr_str)},
        }
      {
        for (const auto& token : expr)
        {
          const auto* const op = dynamic_cast<const detail::string_maker_op*>(&token);
          if (!op)
            continue;

          // Find the operator in the last trailing part of the expression string
          const char* const start = _tokens.back().str;
          const char* const op_str = op->str();
          const auto op_pos = [=] {
            const char* const str = strstr(start, op_str);
            return static_cast<std::size_t>(str ? str - start : 0);
          }();
          if (!op_pos)
            continue;

          const auto orig_size = std::exchange(_tokens.back().len, op_pos);

          const auto op_len = strlen(op_str);
          _tokens.push_back({msg::operator_, start + op_pos, op_len});
          _tokens.push_back({msg::expression, start + op_pos + op_len, orig_size - op_pos - op_len});
        }
      }

      friend std::ostream& operator<<(std::ostream& os, const expression_colorizer& expr)
      {
        for (const auto& token : expr._tokens)
        {
          if (!os.good()) return os;
          os << token.msg_type;
          if (!os.good()) return os;
          os.write(token.str, static_cast<std::ptrdiff_t>(token.len));
        }

        if (!os.good()) return os;
        return os << msg::plain_text;
      }

    private:
      std::vector<token_t> _tokens;
    };
  }

  native_handle_t get_stream_handle(std::ostream& os) noexcept
  {
    if (!os.rdbuf())
      return invalid_native_handle;

#if _XOPEN_VERSION >= 700 || _POSIX_VERSION >= 200112L
#if __GLIBCXX__
    // Support for glibc++: query the actual file-descriptor, can work for other things than stdout/stderr too
    if (__gnu_cxx::stdio_sync_filebuf<char>* rdbuf = dynamic_cast<__gnu_cxx::stdio_sync_filebuf<char>*>(os.rdbuf()))
    {
      if (auto file = rdbuf->file())
        return fileno(file);
    }
#elif _LIBCPP_VERSION
    // NOTE: the same isn't possible for Clang/libc++ because it hides the RTTI required to perform a dynamic_cast
    //       to std::__stdoutbuf<char> from its dynamic symbol table. (The lacking private access is something that
    //       could be worked around in a standards-compliant way, this cannot.)
#endif

    if (os.rdbuf() == std::cout.rdbuf())
      return STDOUT_FILENO;
    if (os.rdbuf() == std::cerr.rdbuf())
      return STDERR_FILENO;
#endif

    return invalid_native_handle;
  }

  AWESOME_ATTR_WEAK
  bool use_colors(std::ostream& os) noexcept
  {
    // Suppress colors according to https://no-color.org/ convention.
    static const auto no_color = [] {
      const char* const envvar = std::getenv("NO_COLOR");
      if (!envvar)
        return false;

      // non-empty content of NO_COLOR env var should disable color
      return envvar[0] != '\0';
    }();

    if (no_color)
      return false;

    // Force or suppress colors according to http://bixense.com/clicolors/ convention.
    static const auto clicolor_force = [] {
      const char* const envvar = std::getenv("CLICOLOR_FORCE");
      if (!envvar)
        return false;
      return std::strcmp(envvar, "0") != 0;
    }();

    if (clicolor_force)
      return true;

    static const auto clicolor_disable = [] {
      const char* const envvar = std::getenv("CLICOLOR");
      if (!envvar)
        return false;
      return std::strcmp(envvar, "0") == 0;
    }();

    if (clicolor_disable)
      return false;

    const auto handle = get_stream_handle(os);
    if (handle == invalid_native_handle)
      return false;

    return ::isatty(handle);
  }

  namespace
  {
    enum class TColor
    {
      None,
      Red,
      Cyan,
      Yellow,
      Grey,
      Bright
    };

    std::ostream& operator<<(std::ostream& os, TColor color)
    {
      if (!use_colors(os))
        return os;

#if _XOPEN_VERSION >= 700 || _POSIX_VERSION >= 200112L
      switch (color)
      {
        case TColor::Red:         return os.write("\033[22;31m", 8);
        case TColor::Cyan:        return os.write("\033[22;36m", 8);
        case TColor::Yellow:      return os.write("\033[22;33m", 8);
        case TColor::Grey:        return os.write("\033[1;30m", 7);
        case TColor::Bright:      return os.write("\033[1;39m", 7);
        case TColor::None:        return os.write("\033[22;39m", 8);
      }
#endif

      return os;
    }
  }

  AWESOME_ATTR_WEAK
  std::ostream& operator<<(std::ostream& os, msg type)
  {
    switch (type)
    {
      case msg::plain_text:         return os << TColor::None;
      case msg::expression:         return os << TColor::Cyan;
      case msg::operator_:          return os << TColor::Yellow;
      case msg::error:              return os << TColor::Red;
      case msg::heading:            return os << TColor::Grey;
      case msg::location_info:
#if __cplusplus >= 201703L
                                    [[fallthrough]];
#endif
      case msg::title:              return os << TColor::Bright;
    }

    return os;
  }

  std::ostream& detail::operator<<(std::ostream& os, const bool_expression& expr)
  {
    bool is_operator = false;
    bool first = true;
    for (const auto& token : expr)
    {
      if (!first)
        os << ' ';
      else
        first = false;
      os << (is_operator ? msg::operator_ : msg::expression);
      if (os.good())
        os << token;
      is_operator = !is_operator;
    }

    if (first)
    {
      os << msg::expression;
      if (os.good())
        os << true;
    }

    return os << msg::plain_text;
  }

  std::ostream& assert_fail_default_log(std::ostream& os, const violation_info& info) noexcept
  {
    const std::ios_base::fmtflags flags(os.flags());

    os << std::boolalpha
      << msg::location_info << info.file_name << ":" << info.line_number << ": " << info.function_name << ": "
      << msg::heading       << "Assertion"
      << msg::plain_text    << " `"
                            << expression_colorizer(info.comment, info.expression)
                            << "', with expansion `"
                            << info.expression
                            << "', "
      << msg::error         << "failed"
      << msg::plain_text    << ".\n"
      ;
    os.flags(flags);
    return os;
  }

}
