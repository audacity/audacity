/**********************************************************************

Audacity: A Digital Audio Editor

NoteTrackVZoomHandle.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../../../Audacity.h"
#include "NoteTrackVZoomHandle.h"
#include "../../../../Experimental.h"
#include "NoteTrackVRulerControls.h"

#include "../../../../HitTestResult.h"
#include "../../../../NoteTrack.h"
#include "../../../../Project.h"
#include "../../../../RefreshCode.h"
#include "../../../../TrackPanelMouseEvent.h"
#include "../../../../../images/Cursors.h"

#include <wx/event.h>

namespace
{

   bool IsDragZooming(int zoomStart, int zoomEnd)
   {
      const int DragThreshold = 3;// Anything over 3 pixels is a drag, else a click.
      return (abs(zoomEnd - zoomStart) > DragThreshold);
   }

}

///////////////////////////////////////////////////////////////////////////////

NoteTrackVZoomHandle::NoteTrackVZoomHandle
(const std::shared_ptr<NoteTrack> &pTrack, const wxRect &rect, int y)
   : mZoomStart(y), mZoomEnd(y), mRect(rect)
   , mpTrack{ pTrack }
{
}

void NoteTrackVZoomHandle::Enter(bool)
{
#ifdef EXPERIMENTAL_TRACK_PANEL_HIGHLIGHTING
   mChangeHighlight = RefreshCode::RefreshCell;
#endif
}

HitTestPreview NoteTrackVZoomHandle::HitPreview(const wxMouseState &state)
{
   static auto zoomInCursor =
      ::MakeCursor(wxCURSOR_MAGNIFIER, ZoomInCursorXpm, 19, 15);
   static auto zoomOutCursor =
      ::MakeCursor(wxCURSOR_MAGNIFIER, ZoomOutCursorXpm, 19, 15);
   const auto message =
_("Click to verticaly zoom in, Shift-click to zoom out, Drag to create a particular zoom region.");
   return {
      message,
      (state.ShiftDown() ? &*zoomOutCursor : &*zoomInCursor)
      // , message
   };
}

UIHandlePtr NoteTrackVZoomHandle::HitTest
(std::weak_ptr<NoteTrackVZoomHandle> &holder,
 const wxMouseState &state,
 const std::shared_ptr<NoteTrack> &pTrack, const wxRect &rect)
{
   if (pTrack) {
      auto result = std::make_shared<NoteTrackVZoomHandle>(
         pTrack, rect, state.m_y);
      result = AssignUIHandlePtr(holder, result);
      return result;
   }
   return {};
}

NoteTrackVZoomHandle::~NoteTrackVZoomHandle()
{
}

UIHandle::Result NoteTrackVZoomHandle::Click
(const TrackPanelMouseEvent &, AudacityProject *)
{
   // change note track to zoom like audio track
   //          mpTrack->StartVScroll();

   return RefreshCode::RefreshNone;
}

UIHandle::Result NoteTrackVZoomHandle::Drag
(const TrackPanelMouseEvent &evt, AudacityProject *pProject)
{
   using namespace RefreshCode;
   auto pTrack = pProject->GetTracks()->Lock(mpTrack);
   if (!pTrack)
      return Cancelled;

   const wxMouseEvent &event = evt.event;
   mZoomEnd = event.m_y;
   if (IsDragZooming(mZoomStart, mZoomEnd)) {
      // changed Note track to work like audio track
      //         pTrack->VScroll(mZoomStart, mZoomEnd);
      return RefreshAll;
   }
   return RefreshNone;
}

HitTestPreview NoteTrackVZoomHandle::Preview
(const TrackPanelMouseState &st, const AudacityProject *)
{
   return HitPreview(st.state);
}

UIHandle::Result NoteTrackVZoomHandle::Release
(const TrackPanelMouseEvent &evt, AudacityProject *pProject,
 wxWindow *pParent)
{
   using namespace RefreshCode;
   auto pTrack = pProject->GetTracks()->Lock(mpTrack);
   if (!pTrack)
      return RefreshNone;

   const wxMouseEvent &event = evt.event;
   if (IsDragZooming(mZoomStart, mZoomEnd)) {
      pTrack->ZoomTo(evt.rect, mZoomStart, mZoomEnd);
   }
   else if (event.ShiftDown() || event.RightUp()) {
      if (event.ShiftDown() && event.RightUp()) {
         // Zoom out completely
         pTrack->SetBottomNote(0);
         pTrack->SetPitchHeight(evt.rect.height, 1);
      } else {
         // Zoom out
         pTrack->ZoomOut(evt.rect, mZoomEnd);
      }
   }
   else {
      pTrack->ZoomIn(evt.rect, mZoomEnd);
   }

   mZoomEnd = mZoomStart = 0;
   pProject->ModifyState(true);

   return RefreshAll;
}

UIHandle::Result NoteTrackVZoomHandle::Cancel(AudacityProject *pProject)
{
   // Cancel is implemented!  And there is no initial state to restore,
   // so just return a code.
   return RefreshCode::RefreshAll;
}

void NoteTrackVZoomHandle::DrawExtras
(DrawingPass pass, wxDC * dc, const wxRegion &, const wxRect &panelRect)
{
   if (!mpTrack.lock()) //? TrackList::Lock()
      return;

   if ( pass == UIHandle::Cells &&
        IsDragZooming( mZoomStart, mZoomEnd ) )
      TrackVRulerControls::DrawZooming
         ( dc, mRect, panelRect, mZoomStart, mZoomEnd );
}
