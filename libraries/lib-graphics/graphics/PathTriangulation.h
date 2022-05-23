/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  PathTriangulation.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <array>
#include <cstdint>
#include <vector>

#include "Point.h"

namespace graphics
{
class GRAPHICS_API PathTriangulation final
{
public:   
   using IndexType = uint16_t;
   using IndexList = std::vector<IndexType>;

   PathTriangulation() = default;
   PathTriangulation(const PathTriangulation&) = delete;
   PathTriangulation(PathTriangulation&&) = default;
   PathTriangulation& operator=(const PathTriangulation&) = delete;
   PathTriangulation& operator=(PathTriangulation&&) = default;
   
   void MoveTo(Point pt);
   void LineTo(Point pt);

   size_t GetPolygonsCount() const noexcept;
   const IndexList& GetPolygon(size_t index) const;
   size_t GetPolygonVertexCount(size_t index) const noexcept;

private:
   void UpdateTriangulation();
   
   // Triangulation is very sensitive to the precision
   using PolygonPoint = std::array<double, 2>;
   using Outline = std::vector<PolygonPoint>;
   using Polygon = std::vector<Outline>;
   using Polygons = std::vector<Polygon>;
   using TriangulatedPolygons = std::vector<IndexList>;

   Polygons mInputPolygons;
   TriangulatedPolygons mOutputPolygons;
   
};
}
