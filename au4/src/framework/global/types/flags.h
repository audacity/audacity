/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 *
 * MuseScore
 * Music Composition & Notation
 *
 * Copyright (C) 2021 MuseScore BVBA and others
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 3 as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
#ifndef MU_GLOBAL_FLAGS_H
#define MU_GLOBAL_FLAGS_H

#include <cstdint>

namespace mu {
class Flag
{
public:
    constexpr inline Flag(int value) noexcept
        : i(value) {}
    constexpr inline Flag(unsigned int value) noexcept
        : i(int(value)) {}
    constexpr inline Flag(short value) noexcept
        : i(int(value)) {}
    constexpr inline Flag(unsigned short value) noexcept
        : i(int(value)) {}

    constexpr inline operator int() const noexcept {
        return i;
    }
    constexpr inline operator unsigned() const noexcept {
        return unsigned(i);
    }

private:
    int i = 0;
};

template<typename Enum>
class Flags
{
    static_assert(sizeof(Enum) <= sizeof(int));
    static_assert(std::is_enum<Enum>::value);

public:
    typedef typename std::conditional<
            std::is_unsigned<typename std::underlying_type<Enum>::type>::value,
            unsigned int,
            signed int
            >::type Int;

    typedef Enum enum_type;

    constexpr inline Flags()
        : i(0) {}
    constexpr inline Flags(Enum flags)
        : i(Int(flags)) {}
    constexpr inline Flags(Flag flag)
        : i(flag) {}

    constexpr inline Flags& operator&=(int mask) { i &= mask; return *this; }
    constexpr inline Flags& operator&=(unsigned int mask) { i &= mask; return *this; }
    constexpr inline Flags& operator&=(Enum mask) { i &= Int(mask); return *this; }
    constexpr inline Flags& operator|=(Flags other) { i |= other.i; return *this; }
    constexpr inline Flags& operator|=(Enum other) { i |= Int(other); return *this; }
    constexpr inline Flags& operator^=(Flags other) { i ^= other.i; return *this; }
    constexpr inline Flags& operator^=(Enum other) { i ^= Int(other); return *this; }
    constexpr inline operator Int() const {
        return i;
    }
    constexpr inline Flags operator|(Flags other) const { return Flags(Flag(i | other.i)); }
    constexpr inline Flags operator|(Enum other) const { return Flags(Flag(i | Int(other))); }
    constexpr inline Flags operator^(Flags other) const { return Flags(Flag(i ^ other.i)); }
    constexpr inline Flags operator^(Enum other) const { return Flags(Flag(i ^ Int(other))); }
    constexpr inline Flags operator&(int mask) const { return Flags(Flag(i & mask)); }
    constexpr inline Flags operator&(unsigned int mask) const { return Flags(Flag(i & mask)); }
    constexpr inline Flags operator&(Enum other) const { return Flags(Flag(i & Int(other))); }
    constexpr inline Flags operator~() const { return Flags(Flag(~i)); }
    constexpr inline bool operator!() const { return !i; }
    constexpr inline bool testFlag(Enum flag) const
    {
        return (i & Int(flag)) == Int(flag) && (Int(flag) != 0 || i == Int(flag));
    }

    constexpr inline Flags& setFlag(Enum flag, bool on = true)
    {
        return on ? (*this |= flag) : (*this &= ~Int(flag));
    }

private:
    Int i = 0;
};

#define DECLARE_FLAGS(Name, Enum) \
    using Name = Flags<Enum>;

#define DECLARE_OPERATORS_FOR_FLAGS(Name) \
    constexpr inline Flags<Name::enum_type> operator|(Name::enum_type f1, Name::enum_type f2) { return Flags<Name::enum_type>(f1) | f2; }
}

#endif // MU_GLOBAL_FLAGS_H
