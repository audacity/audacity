#pragma once

/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file HexHelpers.h
 @brief Define helper functions for hex-to-num conversion.

 Dmitry Vedenko
 **********************************************************************/

#include <cstdint>
#include <cassert>
#include <cctype>

namespace audacity {
inline uint8_t HexCharToNum(char c) noexcept
{
    assert(std::isxdigit(c) != 0);

    if ('0' <= c && c <= '9') {
        return c - '0';
    } else if ('A' <= c && c <= 'F') {
        return c - 'A' + 10;
    } else if ('a' <= c && c <= 'f') {
        return c - 'a' + 10;
    }

    return 0;
}
}
