/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  Path.h

  Dmitry Vedenko

**********************************************************************/
#include "Path.h"

#include "GLPainter.h"
#include "StrokeGenerator.h"

#include <cmath>

namespace graphics::gl
{
extern const RendererID OpenGLRendererID;

Path::Path()
    : PainterPath(OpenGLRendererID)
{
   mPolygons.emplace_back();
}

void Path::EndFigure(bool closed)
{
   if (closed && mPolygons.back().PointsCount > 2)
      mPolygons.back().Closed = true;

   mPolygons.emplace_back();
}

void Path::DoLineTo(Point pt)
{
   if (mPolygons.back().PointsCount == 0)
   {
      DoMoveTo(pt);
      return;
   }
   
   if (pt == mPolygons.back().LastPoint)
      return;
   
   mTriangualtion.LineTo(pt);

   mPolygons.back().LastPoint = pt;
   ++mPolygons.back().PointsCount;

   AppendPointToMesh(pt);
}

void Path::DoMoveTo(Point pt)
{
   if (mPolygons.back().PointsCount != 0)
      EndFigure(false);
   
   mTriangualtion.MoveTo(pt);

   mPolygons.back().LastPoint = pt;
   mPolygons.back().PointsCount = 1;

   AppendPointToMesh(pt);
}

void Path::DoAddRect(const Rect& rect)
{
   MoveTo(rect.origin);
   LineTo(rect.origin.x + rect.size.width, rect.origin.y);
   LineTo(rect.origin.x + rect.size.width, rect.origin.y + rect.size.height);
   LineTo(rect.origin.x, rect.origin.y + rect.size.height);
   EndFigure(true);
}

void Path::Draw(GLPainter& painter, PaintTarget& target) const
{
   if (mVertices.empty())
      return;
   
   if (mBatches.empty())
      const_cast<Path*>(this)->FillBatches();

   auto pen = painter.GetCurrentPen();
   auto& stroker = painter.GetStrokeGenerator();

   for (const auto& batch : mBatches)
   {
      if (batch.vertexCount == 2)
      {
         stroker.StartStroke();
         stroker.AddPoint(mVertices[batch.firstVertex].pos);
         stroker.AddPoint(mVertices[batch.firstVertex + 1].pos);
         stroker.EndStroke(target, pen, false);
      }
      else
      {
         auto brush = painter.GetCurrentBrush();

         if (brush.GetStyle() != BrushStyle::None)
         {
            target.SetupShadersForBrush(brush);
            
            SetColor(batch.firstVertex, batch.vertexCount, brush.GetColor());

            const auto& indices = mTriangualtion.GetPolygon(batch.polygonIndex);

            target.Append(
               GLenum::TRIANGLES, &mVertices[batch.firstVertex], batch.vertexCount,
               indices.data(), indices.size());
         }

         if (pen.GetStyle() != PenStyle::None)
         {            
            stroker.StartStroke();
            
            for (size_t i = 0; i < batch.vertexCount; ++i)
               stroker.AddPoint(mVertices[batch.firstVertex + i].pos);

            if (mVertices.size() > 15)
               int a = 1;
            
            stroker.EndStroke(target, pen, mPolygons[batch.polygonIndex].Closed);
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

   const auto size = rect.size;

   if (roundedWidth >= size.width)
      radius = size.width / 2.0f;

   const float roundedHeight = 2.0f * radius;

   if (roundedHeight >= size.height)
      radius = size.height / 2.0f;

   const float left = rect.origin.x;
   const float top = rect.origin.y;
   const float right = left + rect.size.width;
   const float bottom = top + rect.size.height;

   MoveTo(left + radius, top);

   AddEllipseArc(
      Point { left + radius, top + radius }, radius, radius, float(M_PI_2),
      float(M_PI));

   LineTo(left, top + radius);
   LineTo(left, bottom - radius);

   AddEllipseArc(
      Point { left + radius, bottom - radius }, radius, radius, float(M_PI),
      float(M_PI * 3.0 / 2.0));

   LineTo(left + radius, bottom);
   LineTo(right - radius, bottom);

   AddEllipseArc(
      Point { right - radius, bottom - radius }, radius, radius,
      float(M_PI * 3.0 / 2.0), float(2.0 * M_PI));

   LineTo(right, bottom - radius);
   LineTo(right, top + radius);

   AddEllipseArc(
      Point { right - radius, top + radius }, radius, radius, 0.0f,
      float(M_PI / 2.0));

   LineTo(right - radius, top);

   EndFigure(true);
}

void Path::AppendPointToMesh(Point pt)
{
   mVertices.emplace_back(
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
         mBatches.emplace_back(Batch { processedVertices, 2, i });
      }
      else
      {
         mBatches.emplace_back(
            Batch { processedVertices, vertexCount, i });
      }

      processedVertices += vertexCount;
   }
}

void Path::SetColor(size_t firstIndex, size_t count, Color color) const
{
   for (size_t i = firstIndex; i < (firstIndex + count); ++i)
      mVertices[i].addColor = color;
}

}
