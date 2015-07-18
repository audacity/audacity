/**********************************************************************

Audacity: A Digital Audio Editor

TrackButtonHandles.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "TrackButtonHandles.h"

#include "../../HitTestResult.h"
#include "../../Project.h"
#include "../../RefreshCode.h"
#include "../../Track.h"
#include "../../TrackPanel.h"

MinimizeButtonHandle::MinimizeButtonHandle()
   : ButtonHandle{ TrackPanel::IsMinimizing }
{
}

MinimizeButtonHandle::~MinimizeButtonHandle()
{
}

MinimizeButtonHandle &MinimizeButtonHandle::Instance()
{
   static MinimizeButtonHandle instance;
   return instance;
}

UIHandle::Result MinimizeButtonHandle::CommitChanges
(const wxMouseEvent &, AudacityProject *pProject, wxWindow*)
{
   using namespace RefreshCode;

   if (mpTrack)
   {
      mpTrack->SetMinimized(!mpTrack->GetMinimized());
      if (mpTrack->GetLink())
         mpTrack->GetLink()->SetMinimized(mpTrack->GetMinimized());
      pProject->ModifyState(true);

      // Redraw all tracks when any one of them expands or contracts
      // (Could we invent a return code that draws only those at or below
      // the affected track?)
      return RefreshAll | FixScrollbars;
   }

   return RefreshNone;
}

HitTestResult MinimizeButtonHandle::HitTest
(const wxMouseEvent &event, const wxRect &rect)
{
   wxRect buttonRect;
   TrackInfo::GetMinimizeRect(rect, buttonRect);

   if (buttonRect.Contains(event.m_x, event.m_y)) {
      Instance().mRect = buttonRect;
      return {
         HitPreview(),
         &Instance()
      };
   }
   else
      return {};
}

////////////////////////////////////////////////////////////////////////////////

CloseButtonHandle::CloseButtonHandle()
   : ButtonHandle{ TrackPanel::IsClosing }
{
}

CloseButtonHandle::~CloseButtonHandle()
{
}

CloseButtonHandle &CloseButtonHandle::Instance()
{
   static CloseButtonHandle instance;
   return instance;
}

UIHandle::Result CloseButtonHandle::CommitChanges
(const wxMouseEvent &, AudacityProject *pProject, wxWindow*)
{
   using namespace RefreshCode;
   Result result = RefreshNone;

   if (mpTrack)
   {
      pProject->StopIfPaused();
      if (!pProject->IsAudioActive()) {
         // This pushes an undo item:
         pProject->RemoveTrack(mpTrack);
         // Redraw all tracks when any one of them closes
         // (Could we invent a return code that draws only those at or below
         // the affected track?)
         result |= Resize | RefreshAll | FixScrollbars | DestroyedCell;
      }
   }

   return result;
}

HitTestResult CloseButtonHandle::HitTest
(const wxMouseEvent &event, const wxRect &rect)
{
   wxRect buttonRect;
   TrackInfo::GetCloseBoxRect(rect, buttonRect);

   if (buttonRect.Contains(event.m_x, event.m_y)) {
      Instance().mRect = buttonRect;
      return {
         HitPreview(),
         &Instance()
      };
   }
   else
      return {};
}
