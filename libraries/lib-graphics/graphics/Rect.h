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

template <typename DataType>
struct RectType final
{
   PointType<DataType> Origin;
   SizeType<DataType> Size;

   bool Contains(PointType<DataType> pt) const noexcept
   {
      return pt.x >= Origin.x && pt.y >= Origin.y &&
             (pt.x < (Origin.x + Size.width)) &&
             (pt.y < (Origin.y + Size.height));
   }
};

template<typename DataType>
RectType<DataType>
Intersect(RectType<DataType> firstRect, RectType<DataType> secondRect) noexcept
{
   if (firstRect.Size.IsZero() || secondRect.Size.IsZero())
      return {};

   const PointType<DataType> p11 = firstRect.Origin;
   const PointType<DataType> p12 = { firstRect.Origin.x + firstRect.Size.width,
                                    firstRect.Origin.y + firstRect.Size.height };
   const PointType<DataType> p21 = secondRect.Origin;
   const PointType<DataType> p22 = { secondRect.Origin.x + secondRect.Size.width,
                                     secondRect.Origin.y + secondRect.Size.height };

   if (p12.x < p21.x || p11.x > p22.x || p11.y > p22.y || p12.y < p21.y)
      return {};

   const PointType<DataType> p31 = { std::max(p11.x, p21.x),
                                     std::max(p11.y, p21.y) };

   const PointType<DataType> p32 = { std::min(p12.x, p22.x),
                                     std::min(p12.y, p22.y) };

   return { p31, SizeType<DataType> { p32.x - p31.x, p32.y - p32.y - p31.y } };
}

template <typename DataType>
bool operator==(const RectType<DataType> lhs, const RectType<DataType> rhs) noexcept
{
   return lhs.Origin == rhs.Origin && lhs.Size == rhs.Size;
}

template <typename DataType>
bool operator!=(
   const RectType<DataType> lhs, const RectType<DataType> rhs) noexcept
{
   return !(lhs == rhs);
}

using Rect = RectType<float>;
