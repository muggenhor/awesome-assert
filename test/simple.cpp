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

#include <ostream>
#include <type_traits>
#include <utility>

template <typename T>
struct move_only_t
{
  move_only_t() = delete;

  constexpr move_only_t(T v) noexcept(std::is_nothrow_move_constructible<T>::value)
    : val{std::move(v)}
  {
  }

  move_only_t(move_only_t&&) = default;
  move_only_t& operator=(move_only_t&&) = default;

  // modifying operators are purposefully by-value to force the params to be r-values.
  friend constexpr move_only_t operator<<(move_only_t lhs, move_only_t rhs) noexcept
  {
    return move_only_t{lhs.val << rhs.val};
  }

  friend constexpr move_only_t operator-(move_only_t lhs, move_only_t rhs) noexcept
  {
    return move_only_t{lhs.val - rhs.val};
  }

  friend constexpr bool operator>=(const move_only_t& lhs, const move_only_t& rhs) noexcept
  {
    return lhs.val >= rhs.val;
  }

  friend constexpr bool operator==(const move_only_t& lhs, const move_only_t& rhs) noexcept
  {
    return lhs.val == rhs.val;
  }

  friend std::ostream& operator<<(std::ostream& os, const move_only_t& rhs)
  {
    return os << rhs.val;
  }

  T val;
};

template <typename T>
constexpr move_only_t<std::remove_cv_t<std::remove_reference_t<T>>> move_only(T&& val)
  noexcept(std::is_nothrow_move_constructible<T>::value)
{
  return std::forward<T>(val);
}

int main(int argc, char**)
{
  // Compile-test only
  AWESOME_EXPECTS(true);
  AWESOME_EXPECTS(true && "Uh-oh boolean logic doesn't work!");

  AWESOME_ASSERT(move_only(1) << move_only(3) == move_only(8));

  auto val = 6;
  AWESOME_EXPECTS(1 <= val && val <= 10);
  val = 42;
  AWESOME_EXPECTS(1 > val || val > 10);

  // bitmasks
  const int perms = 0755;
  constexpr const int executable = 1;
  AWESOME_EXPECTS(perms & executable);

  AWESOME_EXPECTS(
      argc >= 10 - argc
   && "not enough command line arguments given"
  );
}
