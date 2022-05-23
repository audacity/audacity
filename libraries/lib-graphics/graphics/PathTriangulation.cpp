/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  PathTriangulation.cpp

  Dmitry Vedenko

**********************************************************************/
#include "PathTriangulation.h"

#include <algorithm>

#include "3party/earcut/earcut.hpp"

namespace graphics
{
   
void PathTriangulation::MoveTo(Point pt)
{
   mInputPolygons.emplace_back();
   mInputPolygons.back().emplace_back();
   mInputPolygons.back().back().emplace_back(
      PolygonPoint { static_cast<double>(pt.x), static_cast<double>(pt.y) });
}

void PathTriangulation::LineTo(Point pt)
{
   if (mInputPolygons.empty())
      MoveTo(pt);
   else
      mInputPolygons.back().back().emplace_back(
         PolygonPoint { static_cast<double>(pt.x), static_cast<double>(pt.y) });
}

size_t PathTriangulation::GetPolygonsCount() const noexcept
{
   return mInputPolygons.size();
}

const PathTriangulation::IndexList& PathTriangulation::GetPolygon(size_t index) const
{
   static IndexList emptyList;

   if (index >= mInputPolygons.size())
      return emptyList;

   if (mOutputPolygons.size() <= index)
      const_cast<PathTriangulation*>(this)->UpdateTriangulation();

   return mOutputPolygons[index];
}

size_t PathTriangulation::GetPolygonVertexCount(size_t index) const noexcept
{
   if (index >= mInputPolygons.size())
      return 0;

   return std::accumulate(
      mInputPolygons[index].begin(), mInputPolygons[index].end(), size_t {}, 
      [](size_t sum, const Outline& outline) { return sum + outline.size(); });
}

void PathTriangulation::UpdateTriangulation()
{
   for (auto polygonIndex = mOutputPolygons.size(); polygonIndex < mInputPolygons.size(); ++polygonIndex)
   {
      const auto& polygon = mInputPolygons[polygonIndex];

      const size_t firstPolygonSize = polygon.front().size();

      if (firstPolygonSize >= 3)
      {
         mOutputPolygons.emplace_back(mapbox::earcut<IndexType>(polygon));
      }
      else if (firstPolygonSize == 2)
      {
         mOutputPolygons.emplace_back();
         
         mOutputPolygons.back().emplace_back(0);
         mOutputPolygons.back().emplace_back(1);
      }
      else
      {
         mOutputPolygons.emplace_back();
      }
         
   }
}

} // namespace graphics
