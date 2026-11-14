/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  Rect.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include "Point.h"
#include "Size.h"

#include <algorithm>

namespace graphics {
//! Rectangle that is defined by the top left corner and the size.
template<typename DataType>
struct RectType final
{
    //! Origin of the rectangle.
    PointType<DataType> origin;
    //! Size of the rectangle.
    SizeType<DataType> size;

    //! Return true if the rectangle is not empty.
    bool IsValid() const noexcept
    {
        return size.width > 0 && size.height > 0;
    }

    //! Return true if the rectangle contains the point specified.
    bool Contains(PointType<DataType> pt) const noexcept
    {
        return pt.x >= origin.x && pt.y >= origin.y
               && (pt.x < (origin.x + size.width))
               && (pt.y < (origin.y + size.height));
    }
};

template<typename DataType>
bool operator==(
    const RectType<DataType> lhs, const RectType<DataType> rhs) noexcept
{
    return lhs.origin == rhs.origin && lhs.size == rhs.size;
}

template<typename DataType>
bool operator!=(
    const RectType<DataType> lhs, const RectType<DataType> rhs) noexcept
{
    return !(lhs == rhs);
}

//! Cast the rectangle to another data type.
template<typename To, typename From>
RectType<To> rect_cast(RectType<From> rect)
{
    return { point_cast<To>(rect.origin), size_cast<To>(rect.size) };
}

//! Alias for rectangle with float data type.
using Rect = RectType<float>;
} // namespace graphics
