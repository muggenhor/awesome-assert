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
}

#endif // INCLUDED_AWESOME_ASSERT_FORMAT_HELPERS_HPP
