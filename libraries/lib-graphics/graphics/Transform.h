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

namespace graphics
{

template <typename DataType> class TransformType final
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
         { x, y }, PointType<DataType> { DataType(1), DataType(1) });
   }

   static TransformType<DataType> Translation(PointType<DataType> pt) noexcept
   {
      return TransformType<DataType>(
         pt, PointType<DataType> { DataType(1), DataType(1) });
   }

   static TransformType<DataType> Scaling(DataType sx, DataType sy) noexcept
   {
      return TransformType<DataType>(
         PointType<DataType>(), PointType<DataType> { sx, sy });
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
      return std::abs(mTranslation.x) >
                std::numeric_limits<DataType>::epsilon() &&
             std::abs(mTranslation.y) >
                std::numeric_limits<DataType>::epsilon();
   }

   bool HasScale() const noexcept
   {
      return std::abs(mScale.x - DataType(1)) >
                std::numeric_limits<DataType>::epsilon() &&
             std::abs(mScale.y - DataType(1)) >
                std::numeric_limits<DataType>::epsilon();
   }

   bool IsIdentity() const noexcept
   {
      return !HasTranslation() && !HasScale();
   }

   PointType<DataType> Apply(DataType x, DataType y) const noexcept
   {
      return { mScale.x * x + mTranslation.x, mScale.y * y + mTranslation.y };
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
      return { Apply(rect.origin), Apply(rect.size) };
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

   friend bool operator==(
      const TransformType<DataType> lhs,
      const TransformType<DataType> rhs) noexcept
   {
      return lhs.mTranslation == rhs.mTranslation && lhs.mScale == rhs.mScale;
   }

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
   {
   }

   PointType<DataType> mTranslation { 0, 0 };
   PointType<DataType> mScale { 1, 1 };
};

using Transform = TransformType<float>;

template <typename DataType> class FullTransformType final
{
public:
   FullTransformType() = default;
   FullTransformType(const FullTransformType&) = default;
   FullTransformType(FullTransformType&&) = default;
   FullTransformType& operator=(const FullTransformType&) = default;
   FullTransformType& operator=(FullTransformType&&) = default;

   FullTransformType(const TransformType<DataType>& transform)
   {
      mMatrix[0][0] = transform.GetScale().x;
      mMatrix[0][1] = 0.0f;
      mMatrix[0][2] = transform.GetTranslation().x;

      mMatrix[1][0] = 0.0f;
      mMatrix[1][1] = transform.GetScale().y;
      mMatrix[1][2] = transform.GetTranslation().y;
   }

   static FullTransformType<DataType>
   Translation(DataType x, DataType y) noexcept
   {
      return FullTransformType<DataType>(
         DataType(1), DataType(0), x, DataType(0), DataType(1), y);
   }

   static FullTransformType<DataType>
   Translation(PointType<DataType> pt) noexcept
   {
      return Translation(pt.x, pt.y);
   }

   static FullTransformType<DataType> Scaling(DataType sx, DataType sy) noexcept
   {
      return FullTransformType<DataType>(
         sx, DataType(0), DataType(0), DataType(0), sy, DataType(0));
   }

   static FullTransformType<DataType> Scaling(DataType scale) noexcept
   {
      return Scaling(scale, scale);
   }

   static FullTransformType<DataType> Scaling(PointType<DataType> sc) noexcept
   {
      return Scaling(sc.x, sc.y);
   }

   static FullTransformType<DataType> Rotation(DataType angle)
   {
      const auto c = std::cos(angle);
      const auto s = std::sin(angle);

      return FullTransformType<DataType>(
         DataType(c), DataType(-s), DataType(0), DataType(s), DataType(c),
         DataType(0));
   }

   PointType<DataType> Apply(DataType x, DataType y) const noexcept
   {
      return { mMatrix[0][0] * x + mMatrix[0][1] * y + mMatrix[0][2],
               mMatrix[1][0] * x + mMatrix[1][1] * y + mMatrix[1][2] };
   }

   PointType<DataType> Apply(PointType<DataType> pt) const noexcept
   {
      return Apply(pt.x, pt.y);
   }

   FullTransformType& Translate(DataType x, DataType y) noexcept
   {
      return Transform(Translation(x, y));
   }

   FullTransformType& Translate(PointType<DataType> pt) noexcept
   {
      return Transform(Translation(pt));
   }

   FullTransformType Translated(DataType x, DataType y) const noexcept
   {
      auto copy = *this;
      copy.Translate(x, y);
      return copy;
   }

   FullTransformType Translated(PointType<DataType> pt) const noexcept
   {
      auto copy = *this;
      copy.Translate(pt);
      return copy;
   }

   FullTransformType& Scale(DataType x, DataType y) noexcept
   {
      return this->Transform(Scaling(x, y));
   }

   FullTransformType& Scale(DataType s) noexcept
   {
      return Scale(s, s);
   }

   FullTransformType& Scale(PointType<DataType> pt) noexcept
   {
      return Scale(pt.x, pt.y);
   }

   FullTransformType Scaled(DataType x, DataType y) const noexcept
   {
      auto copy = *this;
      copy.Scale(x, y);
      return copy;
   }

   FullTransformType Scaled(DataType sc) const noexcept
   {
      auto copy = *this;
      copy.Scale(sc, sc);
      return copy;
   }

   FullTransformType Scaled(PointType<DataType> pt) const noexcept
   {
      auto copy = *this;
      copy.Scale(pt);
      return copy;
   }

   FullTransformType& Transform(const FullTransformType& tr) noexcept
   {
      const DataType lhsRow0[3] = { mMatrix[0][0], mMatrix[0][1],
                                    mMatrix[0][2] };
      const DataType lhsRow1[3] = { mMatrix[1][0], mMatrix[1][1],
                                    mMatrix[1][2] };

      const DataType rhsRow0[3] = { tr.mMatrix[0][0], tr.mMatrix[0][1],
                                    tr.mMatrix[0][2] };
      const DataType rhsRow1[3] = { tr.mMatrix[1][0], tr.mMatrix[1][1],
                                    tr.mMatrix[1][2] };

      mMatrix[0][0] = lhsRow0[0] * rhsRow0[0] + lhsRow0[1] * rhsRow1[0];
      mMatrix[0][1] = lhsRow0[0] * rhsRow0[1] + lhsRow0[1] * rhsRow1[1];
      mMatrix[1][0] = lhsRow1[0] * rhsRow0[0] + lhsRow1[1] * rhsRow1[0];
      mMatrix[1][1] = lhsRow1[0] * rhsRow0[1] + lhsRow1[1] * rhsRow1[1];

      mMatrix[0][2] =
         lhsRow0[0] * rhsRow0[2] + lhsRow0[1] * rhsRow1[2] + lhsRow0[2];
      mMatrix[1][2] =
         lhsRow1[0] * rhsRow0[2] + lhsRow1[1] * rhsRow1[2] + lhsRow1[2];

      return *this;
   }

   FullTransformType Transformed(const FullTransformType& tr) const noexcept
   {
      auto copy = *this;
      copy.Transform(tr);
      return copy;
   }

   bool IsSingular() const noexcept
   {
      const float row0[3] = { mMatrix[0][0], mMatrix[0][1], mMatrix[0][2] };
      const float row1[3] = { mMatrix[1][0], mMatrix[1][1], mMatrix[1][2] };

      const float det = row0[0] * row1[1] - row0[1] * row1[0];

      return std::abs(det) <= std::numeric_limits<DataType>::epsilon();
   }

   FullTransformType& Inverse() noexcept
   {
      const float row0[3] = { mMatrix[0][0], mMatrix[0][1], mMatrix[0][2] };
      const float row1[3] = { mMatrix[1][0], mMatrix[1][1], mMatrix[1][2] };

      const float det = row0[0] * row1[1] - row0[1] * row1[0];

      if (std::abs(det) <= std::numeric_limits<DataType>::epsilon())
         return *this;

      const float invdet = 1.0f / det;

      mMatrix[0][0] = row1[1] * invdet;
      mMatrix[0][1] = -row0[1] * invdet;
      mMatrix[1][0] = -row1[0] * invdet;
      mMatrix[1][1] = row0[0] * invdet;

      mMatrix[0][2] = (row0[1] * row1[2] - row0[2] * row1[1]) * invdet;
      mMatrix[1][2] = (row0[2] * row1[0] - row0[0] * row1[2]) * invdet;

      return *this;
   }

   FullTransformType Inversed() const noexcept
   {
      auto copy = *this;
      copy.Inverse();
      return copy;
   }

private:
   FullTransformType(
      DataType m00, DataType m01, DataType m02, DataType m10, DataType m11,
      DataType m12) noexcept
       : mMatrix { { m00, m01, m02 }, { m10, m11, m12 } }
   {
   }

   DataType mMatrix[2][3] { { DataType(1), DataType(0), DataType(0) },
                            { DataType(0), DataType(1), DataType(0) } };
};

using FullTransform = FullTransformType<float>;

} // namespace graphics
