/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  Size.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <cmath>
#include <type_traits>

namespace graphics {
namespace details {
template<typename DataType>
constexpr DataType GetPositiveSizeValue(DataType value) noexcept
{
    if constexpr (std::is_unsigned_v<DataType>) {
        return value;
    } else {
        return value >= DataType {}
    } ? value : -value;
}
} // namespace details

//! A class that represents size in 2D space.
template<typename DataType>
struct SizeType final
{
    //! Width
    DataType width {};
    //! Height
    DataType height {};

    SizeType& operator+=(SizeType rhs) noexcept
    {
        width = GetPositiveSizeValue(width + rhs.width);
        height = GetPositiveSizeValue(height + rhs.height);

        return *this;
    }

    SizeType& operator-=(SizeType rhs) noexcept
    {
        width = GetPositiveSizeValue(width - rhs.width);
        height = GetPositiveSizeValue(height - rhs.height);

        return *this;
    }

    SizeType& operator*=(SizeType rhs) noexcept
    {
        width = GetPositiveSizeValue(width * rhs.width);
        height = GetPositiveSizeValue(height * rhs.height);

        return *this;
    }

    SizeType& operator/=(SizeType rhs) noexcept
    {
        width = GetPositiveSizeValue(width / rhs.width);
        height = GetPositiveSizeValue(height / rhs.height);

        return *this;
    }

    template<typename ScaleType> SizeType& operator*=(ScaleType scale) noexcept
    {
        width = static_cast<DataType>(GetPositiveSizeValue(width * scale));
        height = static_cast<DataType>(GetPositiveSizeValue(height * scale));

        return *this;
    }

    template<typename ScaleType> SizeType& operator/=(ScaleType scale) noexcept
    {
        width = static_cast<DataType>(GetPositiveSizeValue(width / scale));
        height = static_cast<DataType>(GetPositiveSizeValue(height / scale));

        return *this;
    }

    SizeType& operator-() noexcept
    {
        static_assert(std::is_signed_v<DataType>);

        width = -width;
        height = -height;

        return *this;
    }

    //! Return true if both width and height are zero.
    bool IsZero() const noexcept
    {
        return std::abs(width) <= std::numeric_limits<DataType>::epsilon()
               && std::abs(height) <= std::numeric_limits<DataType>::epsilon();
    }
};

//! Casts size to another data type.
template<typename To, typename From>
SizeType<To> size_cast(SizeType<From> point)
{
    return { static_cast<To>(point.width), static_cast<To>(point.height) };
}

template<typename DataType>
bool operator==(SizeType<DataType> lhs, SizeType<DataType> rhs) noexcept
{
    return lhs.width == rhs.width && lhs.height == rhs.height;
}

template<typename DataType>
bool operator!=(SizeType<DataType> lhs, SizeType<DataType> rhs) noexcept
{
    return !(lhs == rhs);
}

template<typename DataType>
SizeType<DataType>
operator+(SizeType<DataType> lhs, SizeType<DataType> rhs) noexcept
{
    return { lhs.width + rhs.width, lhs.height + rhs.height };
}

template<typename DataType>
SizeType<DataType>
operator-(SizeType<DataType> lhs, SizeType<DataType> rhs) noexcept
{
    return { lhs.width - rhs.width, lhs.height - rhs.height };
}

template<typename DataType>
SizeType<DataType>
operator*(SizeType<DataType> lhs, SizeType<DataType> rhs) noexcept
{
    return { lhs.width * rhs.width, lhs.height * rhs.height };
}

template<typename DataType>
SizeType<DataType>
operator/(SizeType<DataType> lhs, SizeType<DataType> rhs) noexcept
{
    return { lhs.width / rhs.width, lhs.height / rhs.height };
}

template<typename DataType, typename ScaleType>
SizeType<DataType> operator*(SizeType<DataType> lhs, ScaleType rhs) noexcept
{
    return { static_cast<DataType>(lhs.width * rhs),
             static_cast<DataType>(lhs.height * rhs) };
}

template<typename DataType, typename ScaleType>
SizeType<DataType> operator*(ScaleType lhs, SizeType<DataType> rhs) noexcept
{
    return { static_cast<DataType>(lhs * rhs.width),
             static_cast<DataType>(lhs * rhs.height) };
}

template<typename DataType, typename ScaleType>
SizeType<DataType> operator/(SizeType<DataType> lhs, ScaleType rhs) noexcept
{
    return { static_cast<DataType>(lhs.width / rhs),
             static_cast<DataType>(lhs.height / rhs) };
}

//! Alias for SizeType<float>
using Size = SizeType<float>;
} // namespace graphics
