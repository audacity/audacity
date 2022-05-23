/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  Path.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include "graphics/Painter.h"
#include "graphics/PathTriangulation.h"

#include "PaintTarget.h"

namespace graphics::gl
{
class GLPainter;

class Path final : public PainterPath
{
public:
   Path();
   
   Path(const Path&) = delete;
   Path(Path&&) = default;
   Path& operator=(const Path&) = delete;
   Path& operator=(Path&&) = default;
   
   void EndFigure(bool closed) override;
   
   void DoLineTo(Point pt) override;
   void DoMoveTo(Point pt) override;
   void DoAddRect(const Rect& rect) override;

   void Draw(GLPainter& painter, PaintTarget& target) const;

   void AddEllipseArc(
      Point center, float horizontalRadius, float verticalRadius,
      float startAngle, float endAngle);

   void AddEllipse(
      Point center, float horizontalRadius, float verticalRadius,
      float startAngle, float endAngle);

   void AddRoundedRect(const Rect& rect, float radius);

private:
   void AppendPointToMesh(Point pt);
   void FillBatches();

   void SetColor(size_t firstIndex, size_t count, Color color) const;

   struct Batch final
   {
      GLenum mode;

      size_t firstVertex;
      size_t vertexCount;

      size_t polygonIndex;
   };
   
   PathTriangulation mTriangualtion;
   
   mutable std::vector<Vertex> mPoints;
   std::vector<Batch> mBatches;

   Point mFirstFigurePoint;
   Point mLastFigurePoint;
   
   size_t mFigurePoints { 0 };
};
}
