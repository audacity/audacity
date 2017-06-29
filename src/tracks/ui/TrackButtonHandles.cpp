/**********************************************************************

Audacity: A Digital Audio Editor

TrackButtonHandles.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../Audacity.h"
#include "TrackButtonHandles.h"

#include "../../HitTestResult.h"
#include "../../Project.h"
#include "../../RefreshCode.h"
#include "../../Track.h"
#include "../../TrackPanel.h"

MinimizeButtonHandle::MinimizeButtonHandle
( const std::shared_ptr<Track> &pTrack, const wxRect &rect )
   : ButtonHandle{ pTrack, rect, TrackPanel::IsMinimizing }
{}

MinimizeButtonHandle::~MinimizeButtonHandle()
{
}

UIHandle::Result MinimizeButtonHandle::CommitChanges
(const wxMouseEvent &, AudacityProject *pProject, wxWindow*)
{
   using namespace RefreshCode;

   auto pTrack = mpTrack.lock();
   if (pTrack)
   {
      pTrack->SetMinimized(!pTrack->GetMinimized());
      if (pTrack->GetLink())
         pTrack->GetLink()->SetMinimized(pTrack->GetMinimized());
      pProject->ModifyState(true);

      // Redraw all tracks when any one of them expands or contracts
      // (Could we invent a return code that draws only those at or below
      // the affected track?)
      return RefreshAll | FixScrollbars;
   }

   return RefreshNone;
}

HitTestResult MinimizeButtonHandle::HitTest
(std::weak_ptr<MinimizeButtonHandle> &holder,
 const wxMouseState &state, const wxRect &rect, TrackPanelCell *pCell)
{
   wxRect buttonRect;
   TrackInfo::GetMinimizeRect(rect, buttonRect);

   if (buttonRect.Contains(state.m_x, state.m_y)) {
      auto pTrack = static_cast<CommonTrackPanelCell*>(pCell)->FindTrack();
      auto result = std::make_shared<MinimizeButtonHandle>( pTrack, buttonRect );
      result = AssignUIHandlePtr(holder, result);
      return {
         HitTestPreview{},
         result
      };
   }
   else
      return {};
}

////////////////////////////////////////////////////////////////////////////////

CloseButtonHandle::CloseButtonHandle
( const std::shared_ptr<Track> &pTrack, const wxRect &rect )
   : ButtonHandle{ pTrack, rect, TrackPanel::IsClosing }
{}

CloseButtonHandle::~CloseButtonHandle()
{
}

UIHandle::Result CloseButtonHandle::CommitChanges
(const wxMouseEvent &, AudacityProject *pProject, wxWindow*)
{
   using namespace RefreshCode;
   Result result = RefreshNone;

   auto pTrack = mpTrack.lock();
   if (pTrack)
   {
      pProject->StopIfPaused();
      if (!pProject->IsAudioActive()) {
         // This pushes an undo item:
         pProject->RemoveTrack(pTrack.get());
         // Redraw all tracks when any one of them closes
         // (Could we invent a return code that draws only those at or below
         // the affected track?)
         result |= Resize | RefreshAll | FixScrollbars | DestroyedCell;
      }
   }

   return result;
}

HitTestResult CloseButtonHandle::HitTest
(std::weak_ptr<CloseButtonHandle> &holder,
 const wxMouseState &state, const wxRect &rect, TrackPanelCell *pCell)
{
   wxRect buttonRect;
   TrackInfo::GetCloseBoxRect(rect, buttonRect);

   if (buttonRect.Contains(state.m_x, state.m_y)) {
      auto pTrack = static_cast<CommonTrackPanelCell*>(pCell)->FindTrack();
      auto result = std::make_shared<CloseButtonHandle>( pTrack, buttonRect );
      result = AssignUIHandlePtr(holder, result);
      return {
         HitTestPreview{},
         result
      };
   }
   else
      return {};
}

////////////////////////////////////////////////////////////////////////////////

MenuButtonHandle::MenuButtonHandle
( const std::shared_ptr<TrackPanelCell> &pCell,
  const std::shared_ptr<Track> &pTrack, const wxRect &rect )
   : ButtonHandle{ pTrack, rect, TrackPanel::IsPopping }
   , mpCell{ pCell }
{}

MenuButtonHandle::~MenuButtonHandle()
{
}

UIHandle::Result MenuButtonHandle::CommitChanges
(const wxMouseEvent &, AudacityProject *, wxWindow *pParent)
{
   auto pCell = mpCell.lock();
   if (!pCell)
      return RefreshCode::Cancelled;
   return pCell->DoContextMenu(mRect, pParent, NULL);
}

HitTestResult MenuButtonHandle::HitTest
(std::weak_ptr<MenuButtonHandle> &holder,
 const wxMouseState &state, const wxRect &rect,
 const std::shared_ptr<TrackPanelCell> &pCell)
{
   wxRect buttonRect;
   TrackInfo::GetTitleBarRect(rect, buttonRect);

   if (buttonRect.Contains(state.m_x, state.m_y)) {
      auto pTrack = static_cast<CommonTrackPanelCell*>(pCell.get())->FindTrack();
      auto result = std::make_shared<MenuButtonHandle>( pCell, pTrack, buttonRect );
      result = AssignUIHandlePtr(holder, result);
      return {
         HitTestPreview{},
         result
      };
   }
   else
      return {};
}
