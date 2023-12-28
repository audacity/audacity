/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  MirUtils.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include <algorithm>
#include <vector>

namespace MIR
{
/*!
 * @brief Function to generate numbers whose prime factorization contains only
 * twos or threes
 */
std::vector<int> GetPossibleBarDivisors(int lower, int upper);

std::vector<int> GetPeakIndices(const std::vector<float>& x);

std::vector<float> GetNormalizedHann(int size);

/*!
 * @brief Returns the index of the maximum value in a vector. Useful when
 * dealing with symmetric spectra reduced only to their positive half.
 * See tests below for more details.
 * @param fullSize The size of the original vector. Must be strictly positive
 * and even, or the function will return 0.
 */
constexpr auto MapToPositiveHalfIndex(int index, int fullSize)
{
   const auto inputIsValid = fullSize > 0 && fullSize % 2 == 0;
   if (!inputIsValid)
      return 0;
   if (index >= 0)
      index = index % fullSize;
   else
      index = fullSize - (-index % fullSize);
   if (index > fullSize / 2)
      index = fullSize - index;
   return index;
}

constexpr auto IsPowOfTwo(int x)
{
   return x > 0 && (x & (x - 1)) == 0;
}

//*******************************
//          TESTS
//*******************************
static_assert(MapToPositiveHalfIndex(-3, 4) == 1);
static_assert(MapToPositiveHalfIndex(-2, 4) == 2);
static_assert(MapToPositiveHalfIndex(-1, 4) == 1);
static_assert(MapToPositiveHalfIndex(0, 4) == 0);
static_assert(MapToPositiveHalfIndex(1, 4) == 1);
static_assert(MapToPositiveHalfIndex(2, 4) == 2);
static_assert(MapToPositiveHalfIndex(3, 4) == 1);
static_assert(MapToPositiveHalfIndex(4, 4) == 0);
static_assert(MapToPositiveHalfIndex(0, 0) == 0);
static_assert(MapToPositiveHalfIndex(1, 0) == 0);
static_assert(MapToPositiveHalfIndex(1, -1) == 0);
static_assert(MapToPositiveHalfIndex(1, -2) == 0);

static_assert(!IsPowOfTwo(-2));
static_assert(!IsPowOfTwo(-1));
static_assert(!IsPowOfTwo(0));
static_assert(IsPowOfTwo(1));
static_assert(IsPowOfTwo(2));
static_assert(!IsPowOfTwo(3));
static_assert(IsPowOfTwo(4));
} // namespace MIR
