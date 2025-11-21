/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  Point.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <cmath>
#include <type_traits>
#include <numeric>

namespace graphics {
//! A point in 2D space.
template<typename DataType>
struct PointType final
{
    DataType x {};
    DataType y {};

    PointType& operator+=(PointType rhs) noexcept
    {
        x += rhs.x;
        y += rhs.y;

        return *this;
    }

    PointType& operator-=(PointType rhs) noexcept
    {
        x -= rhs.x;
        y -= rhs.y;

        return *this;
    }

    PointType& operator*=(PointType rhs) noexcept
    {
        x *= rhs.x;
        y *= rhs.y;

        return *this;
    }

    PointType& operator/=(PointType rhs) noexcept
    {
        x /= rhs.x;
        y /= rhs.y;

        return *this;
    }

    template<typename ScaleType> PointType& operator*=(ScaleType scale) noexcept
    {
        x = static_cast<DataType>(x * scale);
        y = static_cast<DataType>(y * scale);

        return *this;
    }

    template<typename ScaleType> PointType& operator/=(ScaleType scale) noexcept
    {
        x = static_cast<DataType>(x / scale);
        y = static_cast<DataType>(y / scale);

        return *this;
    }

    PointType operator-() const noexcept
    {
        static_assert(std::is_signed_v<DataType>);

        return { -x, -y };
    }

    bool IsZero() const noexcept
    {
        return std::abs(x) <= std::numeric_limits<DataType>::epsilon()
               && std::abs(y) <= std::numeric_limits<DataType>::epsilon();
    }
};

//! Casts a point to another point type.
template<typename To, typename From>
PointType<To> point_cast(PointType<From> point)
{
    return { static_cast<To>(point.x), static_cast<To>(point.y) };
}

template<typename DataType>
bool operator==(PointType<DataType> lhs, PointType<DataType> rhs) noexcept
{
    return lhs.x == rhs.x && lhs.y == rhs.y;
}

template<typename DataType>
bool operator!=(PointType<DataType> lhs, PointType<DataType> rhs) noexcept
{
    return !(lhs == rhs);
}

template<typename DataType>
PointType<DataType>
operator+(PointType<DataType> lhs, PointType<DataType> rhs) noexcept
{
    return { lhs.x + rhs.x, lhs.y + rhs.y };
}

template<typename DataType>
PointType<DataType>
operator-(PointType<DataType> lhs, PointType<DataType> rhs) noexcept
{
    return { lhs.x - rhs.x, lhs.y - rhs.y };
}

template<typename DataType>
PointType<DataType>
operator*(PointType<DataType> lhs, PointType<DataType> rhs) noexcept
{
    return { lhs.x * rhs.x, lhs.y * rhs.y };
}

template<typename DataType>
PointType<DataType>
operator/(PointType<DataType> lhs, PointType<DataType> rhs) noexcept
{
    return { lhs.x / rhs.x, lhs.y / rhs.y };
}

template<typename DataType, typename ScaleType>
PointType<DataType> operator*(PointType<DataType> lhs, ScaleType rhs) noexcept
{
    return { static_cast<DataType>(lhs.x * rhs),
             static_cast<DataType>(lhs.y * rhs) };
}

template<typename DataType, typename ScaleType>
PointType<DataType> operator*(ScaleType lhs, PointType<DataType> rhs) noexcept
{
    return { static_cast<DataType>(lhs * rhs.x),
             static_cast<DataType>(lhs * rhs.y) };
}

template<typename DataType, typename ScaleType>
PointType<DataType> operator/(PointType<DataType> lhs, ScaleType rhs) noexcept
{
    return { static_cast<DataType>(lhs.x / rhs),
             static_cast<DataType>(lhs.y / rhs) };
}

//! Returns the dot (inner) product between the vectors, defined by points.
template<typename DataType>
auto DotProduct(PointType<DataType> lhs, PointType<DataType> rhs) noexcept
{
    return lhs.x * rhs.x + lhs.y * rhs.y;
}

//! Returns the length of a vector matching the point.
template<typename DataType>
auto Norm(PointType<DataType> lhs) noexcept
{
    return std::sqrt(DotProduct(lhs, lhs));
}

//! Returns the distance between the two points
template<typename DataType>
auto Distance(PointType<DataType> lhs, PointType<DataType> rhs) noexcept
{
    return std::sqrt(DotProduct(lhs, rhs));
}

//! Returns a normalized vector in the direction of point
template<typename DataType>
auto Normalized(PointType<DataType> pt) noexcept
{
    const auto norm = Norm(pt);

    if (norm <= std::numeric_limits<DataType>::epsilon()) {
        return pt;
    }

    return pt / norm;
}

//! Alias for the point with float data type.
using Point = PointType<float>;
} // namespace graphics
