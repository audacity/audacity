/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  MirUtils.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include <algorithm>
#include <cassert>
#include <vector>

namespace MIR
{
// Normal distribution parameters obtained by fitting a gaussian in the GTZAN
// dataset tempo values.
static constexpr auto bpmExpectedValue = 126.3333;

// Function to generate numbers whose prime factorization contains only twos or
// threes
std::vector<int> GetPossibleBarDivisors(int lower, int upper);

std::vector<int> GetPeakIndices(const std::vector<float>& odf);

std::vector<float> GetNormalizedHann(int size);

constexpr auto MapToPositiveHalfIndex(int index, int fullSize)
{
   assert(fullSize % 2 == 0);
   while (index < 0)
      index += fullSize;
   while (index >= fullSize)
      index -= fullSize;
   if (index > fullSize / 2)
      index = fullSize - index;
   return index;
}

static_assert(MapToPositiveHalfIndex(-4, 4) == 0);
static_assert(MapToPositiveHalfIndex(-3, 4) == 1);
static_assert(MapToPositiveHalfIndex(-2, 4) == 2);
static_assert(MapToPositiveHalfIndex(-1, 4) == 1);
static_assert(MapToPositiveHalfIndex(0, 4) == 0);
static_assert(MapToPositiveHalfIndex(1, 4) == 1);
static_assert(MapToPositiveHalfIndex(2, 4) == 2);
static_assert(MapToPositiveHalfIndex(3, 4) == 1);
static_assert(MapToPositiveHalfIndex(4, 4) == 0);

constexpr auto IsPowOfTwo(int x)
{
   return (x & (x - 1)) == 0;
}

static_assert(IsPowOfTwo(1));
static_assert(IsPowOfTwo(2));
static_assert(!IsPowOfTwo(3));
static_assert(IsPowOfTwo(4));
} // namespace MIR
