#pragma once

#include <cstdint>
#include <cassert>

namespace audacity
{
inline uint8_t IsHexChar (char c) noexcept
{
    return 
        ('0' <= c && c <= '9') ||
        ('A' <= c && c <= 'F') ||
        ('a' <= c && c <= 'f');
}

inline uint8_t HexCharToNum (char c) noexcept
{
    assert (IsHexChar (c));

    if ('0' <= c && c <= '9')
        return c - '0';
    else if ('A' <= c && c <= 'F')
        return c - 'A' + 10;
    else if ('a' <= c && c <= 'f')
        return c - 'a' + 10;

    return 0;
}
}