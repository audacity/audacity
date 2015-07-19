/**********************************************************************

Audacity: A Digital Audio Editor

ViewInfo.cpp

Paul Licameli

**********************************************************************/

#include "ViewInfo.h"

#include <algorithm>

#include "Internat.h"
#include "xml/XMLWriter.h"

namespace {
static const double gMaxZoom = 6000000;
static const double gMinZoom = 0.001;
}

ZoomInfo::ZoomInfo(double start, double screenDuration, double pixelsPerSecond)
   : vpos(0)
   , h(start)
   , screen(screenDuration)
   , zoom(pixelsPerSecond)
{
}

ZoomInfo::~ZoomInfo()
{
}

/// Converts a position (mouse X coordinate) to
/// project time, in seconds.  Needs the left edge of
/// the track as an additional parameter.
double ZoomInfo::PositionToTime(wxInt64 position,
   wxInt64 origin
   , bool // ignoreFisheye
) const
{
   return h + (position - origin) / zoom;
}


/// STM: Converts a project time to screen x position.
wxInt64 ZoomInfo::TimeToPosition(double projectTime,
   wxInt64 origin
   , bool // ignoreFisheye
) const
{
   return floor(0.5 +
      zoom * (projectTime - h) + origin
   );
}

bool ZoomInfo::ZoomInAvailable() const
{
   return zoom < gMaxZoom;
}

bool ZoomInfo::ZoomOutAvailable() const
{
   return zoom > gMinZoom;
}

void ZoomInfo::SetZoom(double pixelsPerSecond)
{
   zoom = std::max(gMinZoom, std::min(gMaxZoom, pixelsPerSecond));
}

void ZoomInfo::ZoomBy(double multiplier)
{
   SetZoom(zoom * multiplier);
}

void ZoomInfo::FindIntervals
   (double /*rate*/, Intervals &results, wxInt64 origin) const
{
   results.clear();
   results.reserve(2);

   const wxInt64 rightmost(origin + (0.5 + screen * zoom));
   wxASSERT(origin <= rightmost);
   {
      results.push_back(Interval(origin, zoom, false));
   }

   if (origin < rightmost)
      results.push_back(Interval(rightmost, 0, false));
   wxASSERT(!results.empty() && results[0].position == origin);
}

ViewInfo::ViewInfo(double start, double screenDuration, double pixelsPerSecond)
   : ZoomInfo(start, screenDuration, pixelsPerSecond)
   , selectedRegion()
   , track(NULL)
   , total(screen)
   , sbarH(0)
   , sbarScreen(1)
   , sbarTotal(1)
   , sbarScale(1.0)
   , scrollStep(16)
   , bUpdateTrackIndicator(true)
{
}

void ViewInfo::SetBeforeScreenWidth(wxInt64 width, double lowerBoundTime)
{
   h =
      std::max(lowerBoundTime,
         std::min(total - screen,
            width / zoom));
}

void ViewInfo::WriteXMLAttributes(XMLWriter &xmlFile)
{
   selectedRegion.WriteXMLAttributes(xmlFile, wxT("sel0"), wxT("sel1"));
   xmlFile.WriteAttr(wxT("vpos"), vpos);
   xmlFile.WriteAttr(wxT("h"), h, 10);
   xmlFile.WriteAttr(wxT("zoom"), zoom, 10);
}

bool ViewInfo::ReadXMLAttribute(const wxChar *attr, const wxChar *value)
{
   if (selectedRegion.HandleXMLAttribute(attr, value, wxT("sel0"), wxT("sel1")))
      return true;

   if (!wxStrcmp(attr, wxT("vpos"))) {
      long longVpos;
      wxString(value).ToLong(&longVpos);
      vpos = int(longVpos);
      return true;
   }

   if (!wxStrcmp(attr, wxT("h"))) {
      Internat::CompatibleToDouble(value, &h);
      return true;
   }

   if (!wxStrcmp(attr, wxT("zoom"))) {
      Internat::CompatibleToDouble(value, &zoom);
      return true;
   }

   return false;
}
