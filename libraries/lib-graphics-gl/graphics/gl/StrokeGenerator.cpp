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

double PreciseNorm(PointType<double> Point) noexcept
{
   const double x = Point.x;
   const double y = Point.y;

   const double m = std::max(std::abs(x), std::abs(y));

   if (m > 0)
   {
      const double sx = x / m;
      const double sy = y / m;

      return std::sqrt(sx * sx + sy * sy) * m;
   }
   else
   {
      return 0.0;
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

struct Line final
{
   PointType<double> normal;
   double distance { 0 };
};

Line GetLine(
   PointType<double> start, PointType<double> end) noexcept
{
   const auto direction = end - start;
   const PointType<double> normal = GetNormal(direction);

   return { normal, DotProduct(normal, start) };
}

PointType<double> GetIntersection(const Line& lhs, const Line& rhs) noexcept
{
   const auto denom = lhs.normal.x * rhs.normal.y - lhs.normal.y * rhs.normal.x;

   return {
      (lhs.distance * rhs.normal.y - rhs.distance * lhs.normal.y) / denom,
      (lhs.normal.x * rhs.distance - lhs.distance * rhs.normal.x) / denom
   };
}

double GetPointOrientation(const Line& line, PointType<double> point) noexcept
{
   return DotProduct(line.normal, point) - line.distance;
}

float GetSignedDistance(
   const Line& line, PointType<double> point) noexcept
{
   return GetPointOrientation(line, point) / PreciseNorm(line.normal);
}

bool IsFinite(PointType<double> point) noexcept
{
   return std::isfinite(point.x) && std::isfinite(point.y);
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
   mPoints.push_back(PointData { Type::Start, point_cast<double>(point) });
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

   ProcessPoints(penWidth / 2.0f, closed && mPoints.size() > 2);

   for (size_t pointIndex = 0; pointIndex < mPoints.size(); ++pointIndex)
   {
      const auto& point = mPoints[pointIndex];
      
      if (point.pointType == Type::Start || point.pointType == Type::End)
      {
         const size_t pointsCount = mVertices.size();
         
         if (point.pointType == Type::Start && pointsCount > 0)
            mIndices.push_back(PrimitiveRestartIndex);

         mIndices.push_back(pointsCount);
         mIndices.push_back(pointsCount + 1);

         EmitVertex(pen, point.point + point.normal * point.miterDown);
         EmitVertex(pen, point.point + point.normal * point.miterUp);
      }
      else if (point.pointType == Type::InnerCCW || point.pointType == Type::InnerCW)
      {
         EmitJoin(pen, pointIndex);
      }
      else
      {
         EmitDegeneratedJoin(pen, pointIndex);
      }
   }

   if (closed)
   {
      mIndices.push_back(0);
      mIndices.push_back(1);
   }

   paintTarget.Append(GLenum::TRIANGLE_STRIP, mVertices.data(), mVertices.size(), mIndices.data(), mIndices.size());
}

void StrokeGenerator::ProcessPoints(float halfWidth, bool closed)
{
   const auto pointsCount = mPoints.size();
   
   if (!closed)
   {
      const auto firstDirection = PreciseNormalized(mPoints[1].point - mPoints[0].point);
      
      mPoints[0].normal = GetNormal(firstDirection);
      mPoints[0].pointType = Type::Start;
      mPoints[0].miterUp = halfWidth;
      mPoints[0].miterDown = -halfWidth;

      const auto lastDirection = PreciseNormalized(
         mPoints[pointsCount - 1].point - mPoints[pointsCount - 2].point);
      
      mPoints[pointsCount - 1].normal = GetNormal(lastDirection);
      mPoints[pointsCount - 1].pointType = Type::End;
      mPoints[pointsCount - 1].miterUp = halfWidth;
      mPoints[pointsCount - 1].miterDown = -halfWidth;
   }

   for (size_t pointIndex = !closed ? 1 : 0;
        pointIndex < (!closed ? pointsCount - 1 : pointsCount); ++pointIndex)
   {
      const auto points = GetPoints(pointIndex);

      const bool pointsCCWOriented =
         points.orientation > std::numeric_limits<double>::epsilon();

      const auto nextToPrevDirection = points.nextPoint - points.prevPoint;
      const auto nextToPrevDirectionNorm = PreciseNorm(nextToPrevDirection);

      // Rough estimate to check that points are collinear

      const bool collinear =
         nextToPrevDirectionNorm < halfWidth * 2.0f ||
          // Orientation is the signed area of parallelogram area defined by the (prev - next) and (current - next) vectors
          // It is used here to estimate that height of the triangle is less than half pixel.
         std::abs(points.orientation) / nextToPrevDirectionNorm < 0.5f;

      if (!collinear)
      {
         const auto nextToCurrentDirection =
            points.nextPoint - points.currentPoint;

         const auto nextToCurrentNormal =
            halfWidth * PreciseNormalized(GetNormal(nextToCurrentDirection));

         const auto currentToPrevDirection = points.currentPoint - points.prevPoint;

         const auto currentToPrevNormal =
            halfWidth * PreciseNormalized(GetNormal(currentToPrevDirection));

         const Line topLeftLine = GetLine(
            points.currentPoint + nextToCurrentNormal,
            points.nextPoint + nextToCurrentNormal);
            
         const Line topRightLine = GetLine(
            points.currentPoint + currentToPrevNormal,
            points.prevPoint + currentToPrevNormal);
         
         const Line bottomLeftLine = GetLine(
            points.currentPoint - nextToCurrentNormal,
            points.nextPoint - nextToCurrentNormal);
         
         const Line bottomRightLine = GetLine(
            points.currentPoint - currentToPrevNormal,
            points.prevPoint - currentToPrevNormal);

         const auto top = GetIntersection(topLeftLine, topRightLine);
         const auto bottom = GetIntersection(bottomLeftLine, bottomRightLine);

         const auto normal = top - points.currentPoint;

         const auto topMiter = PreciseNorm(normal);
         const auto botMiter = PreciseNorm(bottom - points.currentPoint);

         auto& strokePoint = mPoints[pointIndex];

         strokePoint.normal = PreciseNormalized(normal);
         strokePoint.pointType = pointsCCWOriented ? Type::InnerCCW : Type::InnerCW;
         strokePoint.miterUp = topMiter;
         strokePoint.miterDown = -botMiter;
      }
      else
      {
         const auto currentToPrevDirection =
            points.currentPoint - points.prevPoint;

         const auto proj = DotProduct(currentToPrevDirection, nextToPrevDirection);

         const bool currentIsBetween =
            proj > -std::numeric_limits<double>::epsilon() &&
            proj < (DotProduct(nextToPrevDirection, nextToPrevDirection) +
                    std::numeric_limits<double>::epsilon());

         if (currentIsBetween)
         {
            const auto normal =
               PreciseNormalized(GetNormal(currentToPrevDirection));

            auto& strokePoint = mPoints[pointIndex];

            strokePoint.normal = normal;
            strokePoint.pointType =
               pointsCCWOriented ? Type::InnerCCW : Type::InnerCW;
            strokePoint.miterUp = halfWidth;
            strokePoint.miterDown = -halfWidth;
         }
         else
         {
            const auto normal = points.currentPoint - (points.prevPoint + points.nextPoint) / 2.0;
            
            const auto normalLength = PreciseNorm(normal);

            auto& strokePoint = mPoints[pointIndex];

            strokePoint.normal = PreciseNormalized(normal);
            strokePoint.pointType =
               pointsCCWOriented ? Type::DegeneratedCCW : Type::DegeneratedCW;
            strokePoint.miterUp = normalLength;
            strokePoint.miterDown = -normalLength;
         }
      }
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
      0.0f
   };

   const auto ac = result.prevPoint - result.nextPoint;
   const auto bc = result.currentPoint - result.nextPoint;

   result.orientation = ac.x * bc.y - ac.y * bc.x;

   return result;
}

void StrokeGenerator::EmitVertex(const Pen& pen, StrokePoint point)
{
   mVertices.emplace_back(Vertex {
      point_cast<float>(point), {}, Colors::Transparent, pen.GetColor() });
}

void StrokeGenerator::EmitJoin(const Pen& pen, size_t pointIndex)
{
   const size_t pointsCount = mVertices.size();
   const auto penWidth = pen.GetWidth();
   
   const auto& currentPointData = mPoints[pointIndex];
   const auto pointType = currentPointData.pointType;
   const bool ccw = pointType == Type::InnerCCW;
   
   const auto points = GetPoints(pointIndex);

   const auto miterDown = currentPointData.miterDown;
   const auto miterUp = currentPointData.miterUp;

   const auto miterLength = miterUp - miterDown;

   const auto normal = currentPointData.normal;
   const auto center = currentPointData.point;

   const auto topPoint = center + normal * miterUp;
   const auto bottomPoint = center + normal * miterDown;

   const auto dir1 = PreciseNormalized(points.prevPoint - points.currentPoint);
   const auto dir2 = PreciseNormalized(points.nextPoint - points.currentPoint);

   const auto cosHalfAngle =
      std::sqrt(std::max(0.0, 1.0 - DotProduct(dir1, dir2)) / 2.0);
   
   const auto bevelHeight = cosHalfAngle * penWidth / 2.0;

   const auto minMitter =
      bevelHeight + (currentPointData.pointType == Type::InnerCCW ?
                        -currentPointData.miterDown :
                        currentPointData.miterUp);

   const auto miterLimit = std::max(minMitter, 2.0 * penWidth);

   if (miterLength < (miterLimit + 0.5))
   {
      mIndices.push_back(uint16_t(pointsCount));
      mIndices.push_back(uint16_t(pointsCount + 1));

      EmitVertex(pen, bottomPoint);
      EmitVertex(pen, topPoint);
   }
   else // Miter length is way too long, generate bevel join instead
   {
      const auto leftLineDirection = center - points.prevPoint;
      const auto leftLineNormal = GetNormal(leftLineDirection);

      const auto rightLineDirection = points.nextPoint - center;
      const auto rightLineNormal = GetNormal(rightLineDirection);

      const auto basePoint = ccw ? bottomPoint : topPoint;
      const auto oppositePoint = !ccw ? bottomPoint : topPoint;

      const Line miterLine {
         normal, DotProduct(normal, basePoint + normal * miterLimit)
      };

      const Line leftLine {
         leftLineNormal, DotProduct(leftLineNormal, oppositePoint)
      };
      const Line rightLine {
         rightLineNormal, DotProduct(rightLineNormal, oppositePoint)
      };

      const auto leftPoint = GetIntersection(miterLine, leftLine);
      const auto rightPoint = GetIntersection(miterLine, rightLine);

      if (IsFinite(leftPoint) && IsFinite(rightPoint))
      {
         if (ccw)
         {
            mIndices.push_back(uint16_t(pointsCount));
            mIndices.push_back(uint16_t(pointsCount + 1));
            mIndices.push_back(uint16_t(pointsCount));
            mIndices.push_back(uint16_t(pointsCount + 2));

            EmitVertex(pen, basePoint);
            EmitVertex(pen, leftPoint);
            EmitVertex(pen, rightPoint);
         }
         else
         {
            mIndices.push_back(uint16_t(pointsCount));
            mIndices.push_back(uint16_t(pointsCount + 1));
            mIndices.push_back(uint16_t(pointsCount + 2));
            mIndices.push_back(uint16_t(pointsCount + 1));

            EmitVertex(pen, leftPoint);
            EmitVertex(pen, basePoint);
            EmitVertex(pen, rightPoint);
         }
      }
      else
      {
         mIndices.push_back(uint16_t(pointsCount));
         mIndices.push_back(uint16_t(pointsCount + 1));

         EmitVertex(pen, bottomPoint);
         EmitVertex(pen, topPoint);
      }
   }
}

void StrokeGenerator::EmitDegeneratedJoin(const Pen& pen, size_t pointIndex)
{
}


} // namespace graphics::gl
