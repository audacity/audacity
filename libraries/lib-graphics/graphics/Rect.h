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

namespace graphics
{
//! Rectangle that is defined by the top left corner and the size.
template <typename DataType>
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
      return pt.x >= origin.x && pt.y >= origin.y &&
             (pt.x < (origin.x + size.width)) &&
             (pt.y < (origin.y + size.height));
   }
};

//! Calculates the intersection of two rectangles.
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

//! Cast the rectangle to another data type.
template <typename To, typename From>
RectType<To> rect_cast(RectType<From> rect)
{
   return { point_cast<To>(rect.origin), size_cast<To>(rect.size) };
}

//! Alias for rectangle with float data type.
using Rect = RectType<float>;

//! Axis aligned bounding box.
template<typename DataType>
struct AxisAlignedBoundingBoxType
{
   //! Top left corner of the box
   PointType<DataType> topLeft { std::numeric_limits<DataType>::max(),
                                 std::numeric_limits<DataType>::max() };

   //! Bottom right corner of the box
   PointType<DataType> bottomRight { std::numeric_limits<DataType>::min(),
                                     std::numeric_limits<DataType>::min() };

   //! Return true if the box is not empty.
   bool IsValid() const noexcept
   {
      return topLeft.x <= bottomRight.x && topLeft.y <= bottomRight.y;
   }

   AxisAlignedBoundingBoxType() = default;

   //! Constructs the box from the specified point.
   explicit AxisAlignedBoundingBoxType(PointType<DataType> point)
       : topLeft(point)
       , bottomRight(point)
   {
   }

   //! Constructs the box from the specified rectangle.
   explicit AxisAlignedBoundingBoxType(RectType<DataType> rect)
       : topLeft(rect.origin)
       , bottomRight(rect.origin + PointType<DataType>{ rect.size.width, rect.size.height })
   {
   }

   //! Constructs the box from the specified points.
   AxisAlignedBoundingBoxType(PointType<DataType> p1, PointType<DataType> p2)
       : topLeft { std::min(p1.x, p2.x), std::min(p1.y, p2.y) }
       , bottomRight { std::max(p1.x, p2.x), std::max(p1.y, p2.y) }
   {
   }

   AxisAlignedBoundingBoxType(const AxisAlignedBoundingBoxType &) = default;
   AxisAlignedBoundingBoxType(AxisAlignedBoundingBoxType &&) = default;
   AxisAlignedBoundingBoxType &operator=(const AxisAlignedBoundingBoxType &) = default;
   AxisAlignedBoundingBoxType &operator=(AxisAlignedBoundingBoxType &&) = default;

   //! Expands the box to include the specified point.
   AxisAlignedBoundingBoxType& Expand(PointType<DataType> point) noexcept
   {
      topLeft.x = std::min(topLeft.x, point.x);
      topLeft.y = std::min(topLeft.y, point.y);
      bottomRight.x = std::max(bottomRight.x, point.x);
      bottomRight.y = std::max(bottomRight.y, point.y);

      return *this;
   }

   //! Expands the copy of the box to include the specified rectangle.
   AxisAlignedBoundingBoxType Expanded(PointType<DataType> point) const noexcept
   {
      AxisAlignedBoundingBoxType result = *this;
      result.Expand(point);
      return result;
   }

   //! Expands the box to include the specified rectangle.
   AxisAlignedBoundingBoxType& Expand(const RectType<DataType>& rect) noexcept
   {
      topLeft.x = std::min(topLeft.x, rect.origin.x);
      topLeft.y = std::min(topLeft.y, rect.origin.y);
      bottomRight.x = std::max(bottomRight.x, rect.origin.x + rect.size.width);
      bottomRight.y = std::max(bottomRight.y, rect.origin.y + rect.size.height);

      return *this;
   }

   //! Expands the copy of the box to include the specified rectangle.
   AxisAlignedBoundingBoxType Expanded(const RectType<DataType>& rect) const noexcept
   {
      AxisAlignedBoundingBoxType result = *this;
      result.Expand(rect);
      return result;
   }

   //! Expands the box to include the specified box.
   AxisAlignedBoundingBoxType& Expand(const AxisAlignedBoundingBoxType<DataType>& other) noexcept
   {
      topLeft.x = std::min(topLeft.x, other.topLeft.x);
      topLeft.y = std::min(topLeft.y, other.topLeft.y);
      bottomRight.x = std::max(bottomRight.x, other.bottomRight.x);
      bottomRight.y = std::max(bottomRight.y, other.bottomRight.y);

      return *this;
   }

   //! Expands the copy of the box to include the specified box.
   AxisAlignedBoundingBoxType Expanded(const AxisAlignedBoundingBoxType other) const noexcept
   {
      AxisAlignedBoundingBoxType result = *this;
      result.Expand(other);
      return result;
   }

   //! Returns true if the box contains the specified point.
   bool Contains(PointType<DataType> point) const noexcept
   {
      return (point.x >= topLeft.x) && (point.x <= bottomRight.x) &&
             (point.y >= topLeft.y) && (point.y <= bottomRight.y);
   }

   //! Returns true if the box contains the specified rectangle.
   bool Contains(const RectType<DataType>& rect) const noexcept
   {
      return (rect.origin.x >= topLeft.x) &&
             (rect.origin.x + rect.size.width <= bottomRight.x) &&
             (rect.origin.y >= topLeft.y) &&
             (rect.origin.y + rect.size.height <= bottomRight.y);
   }

   //! Returns true if the box contains the specified box.
   bool Contains(const AxisAlignedBoundingBoxType<DataType>& other) const noexcept
   {
      return (other.topLeft.x >= topLeft.x) &&
             (other.bottomRight.x <= bottomRight.x) &&
             (other.topLeft.y >= topLeft.y) &&
             (other.bottomRight.y <= bottomRight.y);
   }

   //! Returns true if the box intersects the specified rectangle.
   bool Intersects(const RectType<DataType>& rect) const noexcept
   {
      return Intersects(AxisAlignedBoundingBoxType<DataType>(rect));
   }

   //! Returns true if the box intersects the specified box.
   bool Intersects(const AxisAlignedBoundingBoxType<DataType>& other) const noexcept
   {
      return !(
         bottomRight.x < other.topLeft.x || topLeft.x > other.bottomRight.x ||
         bottomRight.y < other.topLeft.y || topLeft.y > other.bottomRight.y);
   }
};

template <typename DataType>
bool operator==(
   const AxisAlignedBoundingBoxType<DataType> lhs,
   const AxisAlignedBoundingBoxType<DataType> rhs) noexcept
{
   return lhs.topLeft == rhs.topLeft && lhs.bottomRight == rhs.bottomRight;
}

template <typename DataType>
bool operator!=(
   const AxisAlignedBoundingBoxType<DataType> lhs,
   const AxisAlignedBoundingBoxType<DataType> rhs) noexcept
{
   return !(lhs == rhs);
}

//! Casts the AxisAlignedBoundingBoxType to another type.
template <typename To, typename From>
AxisAlignedBoundingBoxType<To> aabb_cast(AxisAlignedBoundingBoxType<From> aabb)
{
   return { point_cast<To>(aabb.topLeft), size_cast<To>(aabb.bottomRight) };
}

//! Alias for AxisAlignedBoundingBoxType<float>.
using AABB = AxisAlignedBoundingBoxType<float>;

} // namespace graphics
