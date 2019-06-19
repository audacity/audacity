/**********************************************************************

Audacity: A Digital Audio Editor

ViewInfo.cpp

Paul Licameli

**********************************************************************/

#include "ViewInfo.h"

#include "Experimental.h"

#include <algorithm>

#include "AudioIOBase.h"
#include "Prefs.h"
#include "Project.h"
#include "xml/XMLWriter.h"
#include "prefs/TracksBehaviorsPrefs.h"
#include "xml/XMLWriter.h"

static const AudacityProject::AttachedObjects::RegisteredFactory key{
   []( AudacityProject &project ) {
      auto result =
         std::make_unique<ViewInfo>(0.0, 1.0, ZoomInfo::GetDefaultZoom());
      project.Bind(EVT_TRACK_PANEL_TIMER,
         &ViewInfo::OnTimer,
         result.get());
      return std::move( result );
   }
};

ViewInfo &ViewInfo::Get( AudacityProject &project )
{
   return project.AttachedObjects::Get< ViewInfo >( key );
}

const ViewInfo &ViewInfo::Get( const AudacityProject &project )
{
   return Get( const_cast< AudacityProject & >( project ) );
}

ViewInfo::ViewInfo(double start, double screenDuration, double pixelsPerSecond)
   : ZoomInfo(start, pixelsPerSecond)
   , selectedRegion()
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

void ViewInfo::UpdateSelectedPrefs( int id )
{
   if (id == UpdateScrollPrefsID())
      gPrefs->Read(wxT("/GUI/AutoScroll"), &bUpdateTrackIndicator,
                   true);
   ZoomInfo::UpdateSelectedPrefs( id );
}

void ViewInfo::UpdatePrefs()
{
   ZoomInfo::UpdatePrefs();
#ifdef EXPERIMENTAL_SCROLLING_LIMITS
   gPrefs->Read(TracksBehaviorsPrefs::ScrollingPreferenceKey(), &bScrollBeyondZero,
                TracksBehaviorsPrefs::ScrollingPreferenceDefault());
#endif
   gPrefs->Read(wxT("/GUI/AdjustSelectionEdges"), &bAdjustSelectionEdges,
      true);

   UpdateSelectedPrefs( UpdateScrollPrefsID() );
}

void ViewInfo::SetBeforeScreenWidth(wxInt64 beforeWidth, wxInt64 screenWidth, double lowerBoundTime)
{
   h =
      std::max(lowerBoundTime,
         std::min(total - screenWidth / zoom,
         beforeWidth / zoom));
}

void ViewInfo::WriteXMLAttributes(XMLWriter &xmlFile) const
// may throw
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
   auto gAudioIO = AudioIOBase::Get();
   mRecentStreamTime = gAudioIO->GetStreamTime();
   event.Skip();
   // Propagate the message to other listeners bound to this
   this->ProcessEvent( event );
}

int ViewInfo::UpdateScrollPrefsID()
{
   static int value = wxNewId();
   return value;
}
