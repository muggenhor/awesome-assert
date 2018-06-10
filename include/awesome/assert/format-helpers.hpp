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

#ifndef INCLUDED_AWESOME_ASSERT_FORMAT_HELPERS_HPP
#define INCLUDED_AWESOME_ASSERT_FORMAT_HELPERS_HPP

#include "awesome_export.h"
#include <ostream>

namespace AwesomeAssert
{
  enum class AWESOME_EXPORT TColor
  {
    None,
    Red,
    Cyan,
    Yellow,
    Grey,
    Bright
  };

  AWESOME_EXPORT std::ostream& operator<<(std::ostream& os, TColor color);

#ifdef _WIN32
  using native_handle_t = void*;
  constexpr const native_handle_t invalid_native_handle = INVALID_HANDLE_VALUE;
#else
  using native_handle_t = int;
  constexpr const native_handle_t invalid_native_handle = -1;
#endif

  /**
   * Tries to determine the native handle associated with @a os.
   *
   * @returns the associated handle or @a invalid_native_handle if it couldn't determine the handle.
   */
  AWESOME_EXPORT native_handle_t get_stream_handle(std::ostream& os) noexcept;

  /**
   * Overridable extension point that determines whether AwesomeAssert should use colors when
   * writing to the given output stream. Defaults to ::isatty(get_stream_handle(os)).
   */
  AWESOME_EXPORT bool use_colors(std::ostream& os) noexcept;
}

#endif // INCLUDED_AWESOME_ASSERT_FORMAT_HELPERS_HPP
