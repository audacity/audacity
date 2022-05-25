/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  StrokeGenerator.h

  Dmitry Vedenko

**********************************************************************/
#include "StrokeGenerator.h"

namespace graphics::gl
{
namespace
{
const float DotPattern[] = { 1.0f, 1.0f };
const float LongDashPattern[] = { 8.0f, 1.0f };
const float ShortDashPattern[] = { 4.0f, 1.0f };
const float DotDashPattern[] = { 1.0f, 1.0f, 4.0f, 1.0f };

std::pair<const float*, size_t> GetPattern(PenStyle style)
{
   switch (style)
   {
   case PenStyle::Dot:
      return { DotPattern, std::size(DotPattern) / sizeof(float) };
   case PenStyle::LongDash:
      return { LongDashPattern, std::size(LongDashPattern) / sizeof(float) };
   case PenStyle::ShortDash:
      return { ShortDashPattern, std::size(ShortDashPattern) / sizeof(float) };
   case PenStyle::DotDash:
      return { DotDashPattern, std::size(DotDashPattern) / sizeof(float) };
   default:
      return {};
   }
}

PointType<double> PreciseNormalized(PointType<double> point) noexcept
{
   const double x = point.x;
   const double y = point.y;

   const double m = std::max(std::abs(x), std::abs(y));

   if (m > std::numeric_limits<double>::epsilon())
   {
      const double sx = x / m;
      const double sy = y / m;

      const double norm = std::sqrt(sx * sx + sy * sy);

      return { sx / norm, sy / norm };
   }
   else
   {
      return {};
   }
}

PointType<double> GetNormal(PointType<double> point) noexcept
{
   return { -point.y, point.x };
}

} // namespace

void StrokeGenerator::StartStroke()
{
   mPoints.clear();
   mVertices.clear();
   mIndices.clear();
}

void StrokeGenerator::AddPoint(const Point& point)
{
   mPoints.push_back(PointData { Type::Inner, point_cast<double>(point) });
}

void StrokeGenerator::AddPoints(const Point* points, size_t count)
{
   for (size_t i = 0; i < count; ++i)
      AddPoint(points[i]);
}

void StrokeGenerator::EndStroke(
   PaintTarget& paintTarget, const Pen& pen, bool closed)
{
   if (mPoints.size() < 2 || pen.GetStyle() == PenStyle::None)
      return;
   
   if (pen.GetStyle() != PenStyle::Solid)
   {
      auto pattern = GetPattern(pen.GetStyle());
      ProcessDashes(pattern.first, pattern.second);
   }

   const float penWidth = std::max<float>(1.f, pen.GetWidth());
   const float halfWidth = penWidth / 2.0f;

   ProcessPoints(penWidth / 2.0f, closed && mPoints.size() > 2);

   for (size_t pointIndex = 0; pointIndex < mPoints.size(); ++pointIndex)
   {
      size_t pointsCount = mVertices.size();
      
      const auto points = GetPoints(pointIndex);
      const auto& currentPoint = mPoints[pointIndex];
      
      if (currentPoint.pointType == Type::Start)
      {
         if (pointsCount > 0)
            mIndices.push_back(PrimitiveRestartIndex);

         const auto p1 = points.currentPoint;
         const auto p2 = points.nextPoint;

         const auto d1 = PreciseNormalized(p2 - p1);
         const auto n1 = GetNormal(d1);

         mIndices.push_back(pointsCount);
         mIndices.push_back(pointsCount + 1);

         EmitVertex(pen, p1 - n1 * halfWidth);
         EmitVertex(pen, p1 + n1 * halfWidth);
      }
      else if (currentPoint.pointType == Type::End)
      {
         const auto p0 = points.prevPoint;
         const auto p1 = points.currentPoint;

         const auto d0 = PreciseNormalized(p1 - p0);
         const auto n0 = GetNormal(d0);

         mIndices.push_back(pointsCount);
         mIndices.push_back(pointsCount + 1);

         EmitVertex(pen, p1 - n0 * halfWidth);
         EmitVertex(pen, p1 + n0 * halfWidth);
      }
      else
      {
         const auto p0 = points.prevPoint;
         const auto p1 = points.currentPoint;
         const auto p2 = points.nextPoint;

         const auto d0 = PreciseNormalized(p1 - p0);
         const auto d1 = PreciseNormalized(p2 - p1);

         const auto n0 = GetNormal(d0);
         const auto n1 = GetNormal(d1);

         auto miterDirection = PreciseNormalized(n0 + n1);

         auto miterLength = halfWidth / DotProduct(miterDirection, n1);

         const float miterLimit = 2.0f * penWidth;
         
         if (miterLength > miterLimit)
         {
            miterLength = halfWidth;
            miterDirection = n1;
            
            mIndices.push_back(pointsCount++);
            mIndices.push_back(pointsCount++);
            mIndices.push_back(pointsCount++);

            if (DotProduct(d0, n1) < 0.0f)
            {
               EmitVertex(pen, p1 + halfWidth * n0);
               EmitVertex(pen, p1 + halfWidth * n1);
               EmitVertex(pen, p1);
            }
            else
            {
               EmitVertex(pen, p1 - halfWidth * n1);
               EmitVertex(pen, p1 - halfWidth * n0);
               EmitVertex(pen, p1);
            }
         }

         mIndices.push_back(pointsCount++);
         mIndices.push_back(pointsCount++);

         EmitVertex(pen, p1 + miterLength * miterDirection);
         EmitVertex(pen, p1 - miterLength * miterDirection);
      }
   }

   if (closed)
   {
      mIndices.push_back(0);
      mIndices.push_back(1);
   }

   paintTarget.SetDefaultShader();
   paintTarget.Append(GLenum::TRIANGLE_STRIP, mVertices.data(), mVertices.size(), mIndices.data(), mIndices.size());
}

void StrokeGenerator::ProcessPoints(float halfWidth, bool closed)
{
   const auto pointsCount = mPoints.size();
   
   if (!closed)
   {
      mPoints[0].pointType = Type::Start;
      mPoints[pointsCount - 1].pointType = Type::End;
   }

   for (size_t pointIndex = !closed ? 1 : 0;
        pointIndex < (!closed ? pointsCount - 1 : pointsCount); ++pointIndex)
   {
      mPoints[pointsCount - 1].pointType = Type::Inner;
   };
}

void StrokeGenerator::ProcessDashes(
   const float* dashPattern, size_t dashPatternSize)
{
}

StrokeGenerator::PointsRequestResult StrokeGenerator::GetPoints(size_t index) const noexcept
{
   const auto pointsCount = mPoints.size();

   PointsRequestResult result = {
      index == 0 ? mPoints[pointsCount - 1].point : mPoints[index - 1].point,
      mPoints[index].point,
      index == pointsCount - 1 ? mPoints[0].point : mPoints[index + 1].point,
   };

   return result;
}

void StrokeGenerator::EmitVertex(const Pen& pen, StrokePoint point)
{
   mVertices.emplace_back(Vertex {
      point_cast<float>(point), {}, Colors::Transparent, pen.GetColor() });
}

} // namespace graphics::gl
