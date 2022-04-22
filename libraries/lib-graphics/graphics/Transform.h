/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  Transform.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <cassert>
#include <cmath>
#include <numeric>

#include "Point.h"
#include "Size.h"
#include "Rect.h"

template <typename DataType>
class TransformType final
{
public:
   TransformType() = default;
   TransformType(const TransformType&) = default;
   TransformType(TransformType&&) = default;
   TransformType& operator=(const TransformType&) = default;
   TransformType& operator=(TransformType&&) = default;

   static TransformType<DataType> Translation(DataType x, DataType y) noexcept
   {
      return TransformType<DataType>(
         { x, y }, PointType<DataType> { DataType(1), DataType(1) }
      );
   }

   static TransformType<DataType> Translation(PointType<DataType> pt) noexcept
   {
      return TransformType<DataType>(
         pt, PointType<DataType> { DataType(1), DataType(1) });
   }

   static TransformType<DataType> Scaling(DataType sx, DataType sy) noexcept
   {
      return TransformType<DataType>(
         PointType<DataType>(),
         PointType<DataType> { sx, sy });
   }

   static TransformType<DataType> Scaling(DataType scale) noexcept
   {
      return TransformType<DataType>(
         PointType<DataType>(), PointType<DataType> { scale, scale });
   }

   static TransformType<DataType> Scaling(PointType<DataType> sc) noexcept
   {
      return TransformType<DataType>(PointType<DataType>(), sc);
   }

   bool HasTranslation() const noexcept
   {
      return std::abs(mTranslation.x) > std::numeric_limits<DataType>::epsilon() &&
             std::abs(mTranslation.y) > std::numeric_limits<DataType>::epsilon();
   }

   bool HasScale() const noexcept
   {
      return std::abs(mScale.x - DataType(1)) > std::numeric_limits<DataType>::epsilon() &&
             std::abs(mScale.y - DataType(1)) > std::numeric_limits<DataType>::epsilon();
   }

   bool IsIdentity() const noexcept
   {
      return !HasTranslation() && !HasScale();
   }

   PointType<DataType> Apply(DataType x, DataType y) const noexcept
   {
      return {
         mScale.x * x + mTranslation.x,
         mScale.y * y + mTranslation.y
      };
   }

   PointType<DataType> Apply(PointType<DataType> pt) const noexcept
   {
      return Apply(pt.x, pt.y);
   }

   SizeType<DataType> Apply(SizeType<DataType> sz) const noexcept
   {
      return sz * mScale;
   }

   RectType<DataType> Apply(RectType<DataType> rect) const noexcept
   {
      return { Apply(rect.Origin), Apply(rect.Size) };
   }

   TransformType& Translate(DataType x, DataType y) noexcept
   {
      mTranslation = Apply(x, y);
      return *this;
   }

   TransformType& Translate(PointType<DataType> pt) noexcept
   {
      mTranslation = Apply(pt.x, pt.y);
      return *this;
   }

   TransformType Translated(DataType x, DataType y) const noexcept
   {
      auto copy = *this;
      copy.Translate(x, y);
      return copy;
   }

   TransformType Translated(PointType<DataType> pt) const noexcept
   {
      auto copy = *this;
      copy.Translate(pt);
      return copy;
   }

   TransformType& Scale(DataType x, DataType y) noexcept
   {
      mScale.x *= x;
      mScale.y *= y;

      return *this;
   }

   TransformType& Scale(DataType s) noexcept
   {
      return Scale(s, s);
   }

   TransformType& Scale(PointType<DataType> pt) noexcept
   {
      return Scale(pt.x, pt.y);
   }

   TransformType Scaled(DataType x, DataType y) const noexcept
   {
      auto copy = *this;
      copy.Scale(x, y);
      return copy;
   }

   TransformType Scaled(DataType sc) const noexcept
   {
      auto copy = *this;
      copy.Scale(sc, sc);
      return copy;
   }

   TransformType Scaled(PointType<DataType> pt) const noexcept
   {
      auto copy = *this;
      copy.Scale(pt);
      return copy;
   }

   TransformType& Transform(const TransformType& tr) noexcept
   {
      mTranslation = Apply(tr.mTranslation);
      mScale *= tr.mScale;
      return *this;
   }

   TransformType Transformed(const TransformType& tr) const noexcept
   {
      auto copy = *this;
      copy.Transform(tr);
      return copy;
   }

   bool IsSingular() const noexcept
   {
      return std::abs(mScale.x) <= std::numeric_limits<DataType>::epsilon() ||
             std::abs(mScale.y) <= std::numeric_limits<DataType>::epsilon();
   }

   TransformType& Inverse() noexcept
   {
      if (IsSingular())
      {
         assert(IsSingular());
         return *this;
      }

      mTranslation /= -mScale;
      mScale =
         PointType<DataType> { DataType(1) / mScale.x, DataType(1) / mScale.y };

      return *this;
   }

   TransformType Inversed() const noexcept
   {
      auto copy = *this;
      copy.Inverse();
      return copy;
   }

   PointType<DataType> GetTranslation() const noexcept
   {
      return mTranslation;
   }

   PointType<DataType> GetScale() const noexcept
   {
      return mScale;
   }

   template <typename DataType>
   friend bool operator==(
      const TransformType<DataType> lhs,
      const TransformType<DataType> rhs) noexcept
   {
      return lhs.mTranslation == rhs.mTranslation && lhs.mScale == rhs.mScale;
   }

   template <typename DataType>
   friend bool operator!=(
      const TransformType<DataType> lhs,
      const TransformType<DataType> rhs) noexcept
   {
      return !(lhs == rhs);
   }

private:
   TransformType(PointType<DataType> tr, PointType<DataType> sc)
       : mTranslation(tr)
       , mScale(sc)
   {}

   PointType<DataType> mTranslation { 0, 0 };
   PointType<DataType> mScale { 1, 1 };
};


using Transform = TransformType<float>;
