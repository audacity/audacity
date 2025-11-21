#pragma once

#include <cassert>

/*!
 * @brief Useful when dealing with symmetric spectra reduced only to their
 * positive half. See tests below for more details.
 * @param fullSize The size of the original vector. Must be strictly positive
 * and even, or the function will return 0.
 * @pre fullSize > 0 && fullSize % 2 == 0
 */
constexpr auto MapToPositiveHalfIndex(int index, int fullSize)
{
    assert(fullSize > 0 && fullSize % 2 == 0);
    if (index >= 0) {
        index = index % fullSize;
    } else {
        index = fullSize - (-index % fullSize);
    }
    if (index > fullSize / 2) {
        index = fullSize - index;
    }
    return index;
}

static_assert(MapToPositiveHalfIndex(-3, 4) == 1);
static_assert(MapToPositiveHalfIndex(-2, 4) == 2);
static_assert(MapToPositiveHalfIndex(-1, 4) == 1);
static_assert(MapToPositiveHalfIndex(0, 4) == 0);
static_assert(MapToPositiveHalfIndex(1, 4) == 1);
static_assert(MapToPositiveHalfIndex(2, 4) == 2);
static_assert(MapToPositiveHalfIndex(3, 4) == 1);
static_assert(MapToPositiveHalfIndex(4, 4) == 0);
