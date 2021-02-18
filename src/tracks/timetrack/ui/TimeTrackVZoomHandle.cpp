/**********************************************************************

Audacity: A Digital Audio Editor

TimeTrackVZoomHandle.cpp

Paul Licameli split from TimeTrackVZoomHandle.cpp

**********************************************************************/


#include "TimeTrackVZoomHandle.h"
#include "TimeTrackVRulerControls.h"
#include "TimeTrackControls.h"

#include "../../../HitTestResult.h"
#include "../../../NumberScale.h"
#include "Prefs.h"
#include "../../../ProjectHistory.h"
#include "../../../RefreshCode.h"
#include "../../../TrackPanelMouseEvent.h"
#include "../../../TimeTrack.h"

TimeTrackVZoomHandle::TimeTrackVZoomHandle(
   const std::shared_ptr<TimeTrack> &pTrack, const wxRect &rect, int y)
      : mpTrack{ pTrack }
{
}

TimeTrackVZoomHandle::~TimeTrackVZoomHandle() = default;

void TimeTrackVZoomHandle::Enter( bool, AudacityProject* )
{
#ifdef EXPERIMENTAL_TRACK_PANEL_HIGHLIGHTING
   mChangeHighlight = RefreshCode::RefreshCell;
#endif
}

UIHandle::Result TimeTrackVZoomHandle::Click
(const TrackPanelMouseEvent &, AudacityProject *)
{
   return RefreshCode::RefreshNone;
}

UIHandle::Result TimeTrackVZoomHandle::Drag
(const TrackPanelMouseEvent &evt, AudacityProject *pProject)
{
   using namespace RefreshCode;
   auto pTrack = TrackList::Get( *pProject ).Lock(mpTrack);
   if (!pTrack)
      return Cancelled;
   return RefreshNone;
}

HitTestPreview TimeTrackVZoomHandle::Preview
(const TrackPanelMouseState &st, AudacityProject *)
{
   static  wxCursor arrowCursor{ wxCURSOR_ARROW };

   return {
      XO("Right-click for menu."),
      &arrowCursor
      // , message
   };
}

UIHandle::Result TimeTrackVZoomHandle::Release
(const TrackPanelMouseEvent &evt, AudacityProject *pProject,
 wxWindow *pParent)
{
   auto pTrack = TrackList::Get( *pProject ).Lock(mpTrack);
   using namespace RefreshCode;
   if (!pTrack)
      return RefreshNone;

   const wxMouseEvent &event = evt.event;
   const bool shiftDown = event.ShiftDown();
   const bool rightUp = event.RightUp();

   // Popup menu...
   if (
       rightUp &&
       !(event.ShiftDown() || event.CmdDown()))
   {
      CommonTrackControls::InitMenuData data {
         *pProject, pTrack.get(), pParent, RefreshNone
      };

      auto pMenu = PopupMenuTable::BuildMenu(pParent, &TimeTrackMenuTable::Instance(), &data);
      pParent->PopupMenu(pMenu.get(), event.m_x, event.m_y);
   }

   return UpdateVRuler | RefreshAll;
}

UIHandle::Result TimeTrackVZoomHandle::Cancel(AudacityProject*)
{
   // Cancel is implemented!  And there is no initial state to restore,
   // so just return a code.
   return RefreshCode::RefreshAll;
}
