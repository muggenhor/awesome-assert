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
   * \brief Handler for assert failures.
   * 
   * This function is \c noreturn to permit the compiler to optimize the remaining code with the certainty that the
   * asserted condition is met.  The \c noexcept tells the compiler it doesn't have to produce stack unwinding
   * information for exceptions, giving another optimisation opportunity.
   */
  AWESOME_NORETURN
  void assert_failed(const char* file, int line, const char* function, const char* expr_str) AWESOME_NOEXCEPT;
}

#define AWESOME_ASSERT(expr) \
  do { \
    if (!(expr)) \
    { \
      ::AwesomeAssert::assert_failed(__FILE__, __LINE__, __FUNCTION__, #expr); \
    } \
  } while (0)

#undef AWESOME_NORETURN

#endif // __INCLUDED_AWESOME_ASSERT_HPP__
