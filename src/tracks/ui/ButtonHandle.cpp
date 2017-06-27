/**********************************************************************

Audacity: A Digital Audio Editor

ButtonHandle.cpp

Paul Licameli

**********************************************************************/

#include "../../Audacity.h"
#include "ButtonHandle.h"

#include "../../MemoryX.h"

#include "../../HitTestResult.h"
#include "../../Project.h"
#include "../../RefreshCode.h"
#include "../../Track.h"
#include "../../TrackPanelMouseEvent.h"
#include "../ui/TrackControls.h"

ButtonHandle::ButtonHandle(int dragCode)
   : mDragCode(dragCode)
{
}

ButtonHandle::~ButtonHandle()
{
}

HitTestPreview ButtonHandle::HitPreview()
{
   static wxCursor arrowCursor{ wxCURSOR_ARROW };
   return { {}, &arrowCursor };
}

UIHandle::Result ButtonHandle::Click
(const TrackPanelMouseEvent &evt, AudacityProject *)
{
   using namespace RefreshCode;
   auto pTrack = static_cast<TrackControls*>(evt.pCell.get())->FindTrack();
   if ( !pTrack )
      return Cancelled;

   const wxMouseEvent &event = evt.event;
   if (!event.Button(wxMOUSE_BTN_LEFT))
      return Cancelled;

   // Come here for left click or double click
   if (mRect.Contains(event.m_x, event.m_y)) {
      mpTrack = pTrack;
      TrackControls::gCaptureState = mDragCode;
      // Toggle visible button state
      return RefreshCell;
   }
   else
      return Cancelled;
}

UIHandle::Result ButtonHandle::Drag
(const TrackPanelMouseEvent &evt, AudacityProject *)
{
   const wxMouseEvent &event = evt.event;
   using namespace RefreshCode;
   if (!mpTrack.lock())
      return Cancelled;

   const int newState =
      mRect.Contains(event.m_x, event.m_y) ? mDragCode : 0;
   if (TrackControls::gCaptureState == newState)
      return RefreshNone;
   else {
      TrackControls::gCaptureState = newState;
      return RefreshCell;
   }
}

HitTestPreview ButtonHandle::Preview
(const TrackPanelMouseEvent &, const AudacityProject *)
{
   // No special message or cursor
   return {};
}

UIHandle::Result ButtonHandle::Release
(const TrackPanelMouseEvent &evt, AudacityProject *pProject,
 wxWindow *pParent)
{
   using namespace RefreshCode;
   auto pTrack = mpTrack.lock();
   if (!pTrack)
      return Cancelled;

   Result result = RefreshNone;
   const wxMouseEvent &event = evt.event;
   if (TrackControls::gCaptureState) {
      TrackControls::gCaptureState = 0;
      result = RefreshCell;
   }
   if (pTrack && mRect.Contains(event.m_x, event.m_y))
      result |= CommitChanges(event, pProject, pParent);
   return result;
}

UIHandle::Result ButtonHandle::Cancel(AudacityProject *pProject)
{
   using namespace RefreshCode;
   if (TrackControls::gCaptureState) {
      TrackControls::gCaptureState = 0;
      return RefreshCell;
   }
   else
      return RefreshNone;
}
