/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  StrokeGenerator.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include "graphics/Pen.h"
#include "graphics/Point.h"

#include "PaintTarget.h"

namespace graphics::gl
{

class StrokeGenerator final
{
public:
   void StartStroke();
   
   void AddPoint(const Point& point);
   void AddPoints(const Point* points, size_t count);
   
   void EndStroke(PaintTarget& paintTarget, const Pen& pen, bool closed);

private:
   using StrokePoint = PointType<double>;

   enum class Type
   {
      Start,
      End,
      Inner
   };

   struct PointData final
   {
      Type pointType { Type::Start };

      StrokePoint point;
   };

   struct PointsRequestResult final
   {
      StrokePoint prevPoint;
      StrokePoint currentPoint;
      StrokePoint nextPoint;
   };

   void ProcessPoints(float halfWidth, bool closed);
   void ProcessDashes(const float* dashPattern, size_t dashPatternSize);

   PointsRequestResult GetPoints(size_t index) const noexcept;

   void EmitVertex(const Pen& pen, StrokePoint point);

   std::vector<PointData> mPoints;

   std::vector<Vertex> mVertices;
   std::vector<uint16_t> mIndices;
}; // class StrokeGenerator
} // namespace graphics::gl
