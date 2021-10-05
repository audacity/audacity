/**********************************************************************

Audacity: A Digital Audio Editor

TrackButtonHandles.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/


#include "TrackButtonHandles.h"

#include "Project.h"
#include "../../ProjectAudioIO.h"
#include "../../ProjectAudioManager.h"
#include "../../ProjectHistory.h"
#include "../../SelectUtilities.h"
#include "../../RefreshCode.h"
#include "../../Track.h"
#include "../../TrackPanelAx.h"
#include "../../TrackInfo.h"
#include "../../TrackPanel.h"
#include "../../TrackUtilities.h"
#include "../../commands/CommandManager.h"
#include "../../tracks/ui/TrackView.h"

MinimizeButtonHandle::MinimizeButtonHandle
( const std::shared_ptr<Track> &pTrack, const wxRect &rect )
   : ButtonHandle{ pTrack, rect }
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
      auto channels = TrackList::Channels(pTrack.get());
      bool wasMinimized = TrackView::Get( **channels.begin() ).GetMinimized();
      for (auto channel : channels)
         TrackView::Get( *channel ).SetMinimized( !wasMinimized );
      ProjectHistory::Get( *pProject ).ModifyState(true);

      // Redraw all tracks when any one of them expands or contracts
      // (Could we invent a return code that draws only those at or below
      // the affected track?)
      return RefreshAll | FixScrollbars;
   }

   return RefreshNone;
}

TranslatableString MinimizeButtonHandle::Tip(
   const wxMouseState &, AudacityProject &) const
{
   auto pTrack = GetTrack();
   return TrackView::Get( *pTrack ).GetMinimized()
      ? XO("Expand") : XO("Collapse");
}

UIHandlePtr MinimizeButtonHandle::HitTest
(std::weak_ptr<MinimizeButtonHandle> &holder,
 const wxMouseState &state, const wxRect &rect, TrackPanelCell *pCell)
{
   wxRect buttonRect;
   TrackInfo::GetMinimizeRect(rect, buttonRect);

   if (buttonRect.Contains(state.m_x, state.m_y)) {
      auto pTrack = static_cast<CommonTrackPanelCell*>(pCell)->FindTrack();
      auto result = std::make_shared<MinimizeButtonHandle>( pTrack, buttonRect );
      result = AssignUIHandlePtr(holder, result);
      return result;
   }
   else
      return {};
}

////////////////////////////////////////////////////////////////////////////////
SelectButtonHandle::SelectButtonHandle
( const std::shared_ptr<Track> &pTrack, const wxRect &rect )
   : ButtonHandle{ pTrack, rect }
{}

SelectButtonHandle::~SelectButtonHandle()
{
}

UIHandle::Result SelectButtonHandle::CommitChanges
(const wxMouseEvent &event, AudacityProject *pProject, wxWindow*)
{
   using namespace RefreshCode;

   auto pTrack = mpTrack.lock();
   if (pTrack)
   {
      const bool unsafe = ProjectAudioIO::Get( *pProject ).IsAudioActive();
      SelectUtilities::DoListSelection(*pProject,
         pTrack.get(), event.ShiftDown(), event.ControlDown(), !unsafe);
//    return RefreshAll ;
   }

   return RefreshNone;
}

TranslatableString SelectButtonHandle::Tip(
   const wxMouseState &, AudacityProject &) const
{
   auto pTrack = GetTrack();
#if defined(__WXMAC__)
   return pTrack->GetSelected() ? XO("Command+Click to deselect") : XO("Select track");
#else
   return pTrack->GetSelected() ? XO("Ctrl+Click to deselect") : XO("Select track");
#endif
}

UIHandlePtr SelectButtonHandle::HitTest
(std::weak_ptr<SelectButtonHandle> &holder,
 const wxMouseState &state, const wxRect &rect, TrackPanelCell *pCell)
{
   wxRect buttonRect;
   TrackInfo::GetSelectButtonRect(rect, buttonRect);

   if (buttonRect.Contains(state.m_x, state.m_y)) {
      auto pTrack = static_cast<CommonTrackPanelCell*>(pCell)->FindTrack();
      auto result = std::make_shared<SelectButtonHandle>( pTrack, buttonRect );
      result = AssignUIHandlePtr(holder, result);
      return result;
   }
   else
      return {};
}

////////////////////////////////////////////////////////////////////////////////

CloseButtonHandle::CloseButtonHandle
( const std::shared_ptr<Track> &pTrack, const wxRect &rect )
   : ButtonHandle{ pTrack, rect }
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
      auto toRemove = pTrack->SubstitutePendingChangedTrack();
      ProjectAudioManager::Get( *pProject ).StopIfPaused();
      if (!ProjectAudioIO::Get( *pProject ).IsAudioActive()) {
         // This pushes an undo item:
         TrackUtilities::DoRemoveTrack(*pProject, toRemove.get());
         // Redraw all tracks when any one of them closes
         // (Could we invent a return code that draws only those at or below
         // the affected track?)
         result |= Resize | RefreshAll | FixScrollbars | DestroyedCell;
      }
   }

   return result;
}

TranslatableString CloseButtonHandle::Tip(
   const wxMouseState &, AudacityProject &project) const
{
   auto name = XO("Close");
   auto focused =
      TrackFocus::Get( project ).Get() == GetTrack().get();
   if (!focused)
      return name;

   auto &commandManager = CommandManager::Get( project );
   ComponentInterfaceSymbol command{ wxT("TrackClose"), name };
   return commandManager.DescribeCommandsAndShortcuts( &command, 1u );
}

UIHandlePtr CloseButtonHandle::HitTest
(std::weak_ptr<CloseButtonHandle> &holder,
 const wxMouseState &state, const wxRect &rect, TrackPanelCell *pCell)
{
   wxRect buttonRect;
   TrackInfo::GetCloseBoxRect(rect, buttonRect);

   if (buttonRect.Contains(state.m_x, state.m_y)) {
      auto pTrack = static_cast<CommonTrackPanelCell*>(pCell)->FindTrack();
      auto result = std::make_shared<CloseButtonHandle>( pTrack, buttonRect );
      result = AssignUIHandlePtr(holder, result);
      return result;
   }
   else
      return {};
}

////////////////////////////////////////////////////////////////////////////////

MenuButtonHandle::MenuButtonHandle
( const std::shared_ptr<TrackPanelCell> &pCell,
  const std::shared_ptr<Track> &pTrack, const wxRect &rect )
   : ButtonHandle{ pTrack, rect }
   , mpCell{ pCell }
{}

MenuButtonHandle::~MenuButtonHandle()
{
}

UIHandle::Result MenuButtonHandle::CommitChanges
(const wxMouseEvent &, AudacityProject *pProject, wxWindow *WXUNUSED(pParent))
{
   auto &trackPanel = TrackPanel::Get( *pProject );
   auto pCell = mpCell.lock();
   if (!pCell)
      return RefreshCode::Cancelled;
   auto pTrack =
      static_cast<CommonTrackPanelCell*>(pCell.get())->FindTrack();
   if (!pTrack)
      return RefreshCode::Cancelled;
   trackPanel.CallAfter(
      [&trackPanel,pTrack]{ trackPanel.OnTrackMenu( pTrack.get() ); } );
   return RefreshCode::RefreshNone;
}

TranslatableString MenuButtonHandle::Tip(
   const wxMouseState &, AudacityProject &project) const
{
   auto name = XO("Open menu...");
   auto focused =
      TrackFocus::Get( project ).Get() == GetTrack().get();
   if (!focused)
      return name;

   auto &commandManager = CommandManager::Get( project );
   ComponentInterfaceSymbol command{ wxT("TrackMenu"), name };
   return commandManager.DescribeCommandsAndShortcuts( &command, 1u );
}

UIHandlePtr MenuButtonHandle::HitTest
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
      return result;
   }
   else
      return {};
}
