/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  Path.h

  Dmitry Vedenko

**********************************************************************/
#include "Path.h"

#include "GLPainter.h"

#include <cmath>

namespace graphics::gl
{
extern const RendererID OpenGLRendererID;

Path::Path()
    : PainterPath(OpenGLRendererID)
{
}

void Path::EndFigure(bool closed)
{
   if (closed && mFigurePoints > 2)
      LineTo(mFirstFigurePoint);
}

void Path::DoLineTo(Point pt)
{
   if (pt == mLastFigurePoint)
      return;
   
   mTriangualtion.LineTo(pt);

   mLastFigurePoint = pt;
   ++mFigurePoints;

   AppendPointToMesh(pt);
}

void Path::DoMoveTo(Point pt)
{
   mTriangualtion.MoveTo(pt);

   mFirstFigurePoint = pt;
   mLastFigurePoint = pt;
   
   mFigurePoints = 1;

   AppendPointToMesh(pt);
}

void Path::DoAddRect(const Rect& rect)
{
   MoveTo(rect.Origin);
   LineTo(rect.Origin.x + rect.Size.width, rect.Origin.y);
   LineTo(rect.Origin.x + rect.Size.width, rect.Origin.y + rect.Size.height);
   LineTo(rect.Origin.x, rect.Origin.y + rect.Size.height);
   EndFigure(true);
}

void Path::Draw(GLPainter& painter, PaintTarget& target) const
{
   if (mPoints.empty())
      return;
   
   if (mBatches.empty())
      const_cast<Path*>(this)->FillBatches();

   std::vector<uint16_t> outlineIndexes;

   for (const auto& batch : mBatches)
   {
      if (batch.mode == GLenum::LINE)
      {
         auto pen = painter.GetCurrentPen();

         if (pen.GetStyle() == PenStyle::None)
            continue;

         SetColor(batch.firstVertex, batch.vertexCount, pen.GetColor());

         const uint16_t indices[] = { 0, 1 };

         target.Append(
            GLenum::LINE, &mPoints[batch.firstVertex], 2, indices, 2);
      }
      else
      {
         auto brush = painter.GetCurrentBrush();

         if (brush.GetStyle() != BrushStyle::None)
         {
            SetColor(batch.firstVertex, batch.vertexCount, brush.GetColor());

            const auto& indices = mTriangualtion.GetPolygon(batch.polygonIndex);

            target.Append(
               batch.mode, &mPoints[batch.firstVertex], batch.vertexCount,
               indices.data(), indices.size());
         }

         auto pen = painter.GetCurrentPen();

         if (pen.GetStyle() != PenStyle::None)
         {
            SetColor(batch.firstVertex, batch.vertexCount, pen.GetColor());

            outlineIndexes.clear();
            outlineIndexes.reserve(batch.vertexCount);

            for (size_t i = 0; i < batch.vertexCount; ++i)
               outlineIndexes.push_back(i);

            target.Append(
               GLenum::LINE_STRIP, &mPoints[batch.firstVertex],
               batch.vertexCount, outlineIndexes.data(), outlineIndexes.size());
         }
      }
   }
}

namespace
{
float ClampAngle(float angle)
{
   constexpr float twoPi = static_cast<float>(M_PI * 2.0);

   while (angle < -twoPi)
      angle += twoPi;

   while (angle > twoPi)
      angle -= twoPi;

   return angle;
}

float EstimateArcLength(
   float horizontalRadius, float verticalRadius, float startAngle,
   float endAngle) noexcept
{
   return static_cast<float>(
      3.0 * (horizontalRadius + verticalRadius) *
      (endAngle - startAngle) / (M_PI * 2.0));
}

constexpr int maxArcLength = 2;
} // namespace

void Path::AddEllipseArc(
   Point center, float horizontalRadius, float verticalRadius, float startAngle,
   float endAngle)
{
   if (verticalRadius < 1 || horizontalRadius < 1)
      return;

   startAngle = ClampAngle(startAngle);
   endAngle = ClampAngle(endAngle);

   if (startAngle > endAngle)
      std::swap(startAngle, endAngle);

   const auto lengthEstimation =
      EstimateArcLength(horizontalRadius, verticalRadius, startAngle, endAngle);

   const auto subdivisions =
      std::max(static_cast<int>(lengthEstimation / maxArcLength), 3);

   const float angleStep = (endAngle - startAngle) / subdivisions;

   for (int i = 0; i < subdivisions; ++i)
   {
      const float angle = startAngle + angleStep * i;

      LineTo(
         horizontalRadius * std::cos(angle) + center.x,
         center.y - verticalRadius * std::sin(angle));
   }

   LineTo(
      horizontalRadius * std::cos(endAngle) + center.x,
      center.y - verticalRadius * std::sin(endAngle));
}

void Path::AddEllipse(
   Point center, float horizontalRadius, float verticalRadius, float startAngle,
   float endAngle)
{
   if (verticalRadius < 1 || horizontalRadius < 1)
      return;

   startAngle = ClampAngle(startAngle);
   endAngle = ClampAngle(endAngle);

   if (startAngle > endAngle)
      std::swap(startAngle, endAngle);

   MoveTo(
      horizontalRadius * std::cos(startAngle) + center.x,
      center.y - verticalRadius * std::sin(startAngle));
}

void Path::AddRoundedRect(const Rect& rect, float radius)
{
   if (radius < 1)
   {
      AddRect(rect);
      return;
   }

   const float roundedWidth = 2.0f * radius;

   const auto size = rect.Size;

   if (roundedWidth >= size.width)
      radius = size.width / 2.0f;

   const float roundedHeight = 2.0f * radius;

   if (roundedHeight >= size.height)
      radius = size.height / 2.0f;

   const float left = rect.Origin.x;
   const float top = rect.Origin.y;
   const float right = left + rect.Size.width;
   const float bottom = top + rect.Size.height;

   MoveTo(left + radius, top);

   AddEllipseArc(
      Point { left + radius, top + radius }, radius, radius, float(M_PI_2),
      float(M_PI));

   LineTo(left, bottom - radius);

   AddEllipseArc(
      Point { left + radius, bottom - radius }, radius, radius, float(M_PI),
      float(M_PI * 3.0 / 2.0));

   LineTo(right - radius, bottom);

   AddEllipseArc(
      Point { right - radius, bottom - radius }, radius, radius,
      float(M_PI * 3.0 / 2.0), float(2.0 * M_PI));

   LineTo(right, top + radius);

   AddEllipseArc(
      Point { right - radius, top + radius }, radius, radius, 0.0f,
      float(M_PI / 2.0));

   EndFigure(true);
}

void Path::AppendPointToMesh(Point pt)
{
   mPoints.emplace_back(
      Vertex { pt, PointType<int16_t> {}, Colors::Transparent, Colors::White });
}

void Path::FillBatches()
{
   size_t processedVertices = 0;

   for (size_t i = 0; i < mTriangualtion.GetPolygonsCount(); ++i)
   {
      const auto vertexCount = mTriangualtion.GetPolygonVertexCount(i);
      

      if (vertexCount < 2)
      {
         processedVertices += vertexCount;
         continue;
      }

      if (vertexCount == 2)
      {
         mBatches.emplace_back(Batch { GLenum::LINES, processedVertices, 2, i });
      }
      else
      {
         mBatches.emplace_back(
            Batch { GLenum::TRIANGLES, processedVertices, vertexCount, i });
      }

      processedVertices += vertexCount;
   }
}

void Path::SetColor(size_t firstIndex, size_t count, Color color) const
{
   for (size_t i = firstIndex; i < (firstIndex + count); ++i)
      mPoints[i].addColor = color;
}

}
