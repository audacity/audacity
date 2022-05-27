/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  Rect.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include "Point.h"
#include "Size.h"

#include <algorithm>

namespace graphics
{

template <typename DataType> struct RectType final
{
   PointType<DataType> origin;
   SizeType<DataType> size;

   bool IsValid() const noexcept
   {
      return size.width > 0 && size.height > 0;
   }

   bool Contains(PointType<DataType> pt) const noexcept
   {
      return pt.x >= origin.x && pt.y >= origin.y &&
             (pt.x < (origin.x + size.width)) &&
             (pt.y < (origin.y + size.height));
   }
};

template <typename DataType>
RectType<DataType>
Intersect(RectType<DataType> firstRect, RectType<DataType> secondRect) noexcept
{
   if (firstRect.size.IsZero() || secondRect.size.IsZero())
      return {};

   const PointType<DataType> p11 = firstRect.origin;
   const PointType<DataType> p12 = { firstRect.origin.x + firstRect.size.width,
                                     firstRect.origin.y +
                                        firstRect.size.height };
   const PointType<DataType> p21 = secondRect.origin;
   const PointType<DataType> p22 = {
      secondRect.origin.x + secondRect.size.width,
      secondRect.origin.y + secondRect.size.height
   };

   if (p12.x < p21.x || p11.x > p22.x || p11.y > p22.y || p12.y < p21.y)
      return {};

   const PointType<DataType> p31 = { std::max(p11.x, p21.x),
                                     std::max(p11.y, p21.y) };

   const PointType<DataType> p32 = { std::min(p12.x, p22.x),
                                     std::min(p12.y, p22.y) };

   return { p31, SizeType<DataType> { p32.x - p31.x, p32.y - p32.y - p31.y } };
}

template <typename DataType>
bool operator==(
   const RectType<DataType> lhs, const RectType<DataType> rhs) noexcept
{
   return lhs.origin == rhs.origin && lhs.size == rhs.size;
}

template <typename DataType>
bool operator!=(
   const RectType<DataType> lhs, const RectType<DataType> rhs) noexcept
{
   return !(lhs == rhs);
}

template <typename To, typename From>
RectType<To> rect_cast(RectType<From> rect)
{
   return { point_cast<To>(rect.origin), size_cast<To>(rect.size) };
}

using Rect = RectType<float>;

} // namespace graphics
