/**********************************************************************

Audacity: A Digital Audio Editor

ViewInfo.cpp

Paul Licameli

**********************************************************************/

#include "ViewInfo.h"
#include "Experimental.h"

#include <algorithm>

#include "AudioIO.h"
#include "Internat.h"
#include "Project.h"
#include "prefs/GUISettings.h"
#include "Prefs.h"
#include "xml/XMLWriter.h"
#include "prefs/TracksPrefs.h"

namespace {
static const double gMaxZoom = 6000000;
static const double gMinZoom = 0.001;
}

ZoomInfo::ZoomInfo(double start, double pixelsPerSecond)
   : vpos(0)
   , h(start)
   , zoom(pixelsPerSecond)
{
   UpdatePrefs();
}

ZoomInfo::~ZoomInfo()
{
}

void ZoomInfo::UpdatePrefs()
{
   dBr = gPrefs->Read(ENV_DB_KEY, ENV_DB_RANGE);
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
   double t = 0.5 + zoom * (projectTime - h) + origin ;
   if( t < wxINT64_MIN )
      return wxINT64_MIN;
   if( t > wxINT64_MAX )
      return wxINT64_MAX;
   t = floor( t );
   return t;
}

// This always ignores the fisheye.  Use with caution!
// You should prefer to call TimeToPosition twice, for endpoints, and take the difference!
double ZoomInfo::TimeRangeToPixelWidth(double timeRange) const
{
   return timeRange * zoom;
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
#ifdef EXPERIMENTAL_DA
   // Disable snapping if user zooms in a long way.
   // Helps stop users be trapped in snap-to.
   // The level chosen is in sample viewing range with samples
   // still quite close together.
   if( zoom > (gMaxZoom * 0.06  ))
   {
      AudacityProject * project = GetActiveProject();
      if( project )
         project->OnSnapToOff();
   }
#endif
}

void ZoomInfo::ZoomBy(double multiplier)
{
   SetZoom(zoom * multiplier);
}

void ZoomInfo::FindIntervals
   (double /*rate*/, Intervals &results, wxInt64 width, wxInt64 origin) const
{
   results.clear();
   results.reserve(2);

   const wxInt64 rightmost(origin + (0.5 + width));
   wxASSERT(origin <= rightmost);
   {
      results.push_back(Interval(origin, zoom, false));
   }

   if (origin < rightmost)
      results.push_back(Interval(rightmost, 0, false));
   wxASSERT(!results.empty() && results[0].position == origin);
}

ViewInfo::ViewInfo(double start, double screenDuration, double pixelsPerSecond)
   : ZoomInfo(start, pixelsPerSecond)
   , selectedRegion()
   , track(NULL)
   , total(screenDuration)
   , sbarH(0)
   , sbarScreen(1)
   , sbarTotal(1)
   , sbarScale(1.0)
   , scrollStep(16)
   , bUpdateTrackIndicator(true)
   , bScrollBeyondZero(false)
   , mRecentStreamTime(-1.0)
{
   UpdatePrefs();
}

void ViewInfo::UpdatePrefs()
{
   ZoomInfo::UpdatePrefs();
#ifdef EXPERIMENTAL_SCROLLING_LIMITS
   gPrefs->Read(TracksPrefs::ScrollingPreferenceKey(), &bScrollBeyondZero,
                TracksPrefs::ScrollingPreferenceDefault());
#endif

}

void ViewInfo::SetBeforeScreenWidth(wxInt64 beforeWidth, wxInt64 screenWidth, double lowerBoundTime)
{
   h =
      std::max(lowerBoundTime,
         std::min(total - screenWidth / zoom,
         beforeWidth / zoom));
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
      vpos = (int)(longVpos);
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

void ViewInfo::OnTimer(wxCommandEvent &event)
{
   mRecentStreamTime = gAudioIO->GetStreamTime();
   event.Skip();
}
