/**********************************************************************

Audacity: A Digital Audio Editor

TrackButtonHandles.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../Audacity.h"
#include "TrackButtonHandles.h"

#include "../../HitTestResult.h"
#include "../../Menus.h"
#include "../../Project.h"
#include "../../RefreshCode.h"
#include "../../Track.h"
#include "../../TrackPanel.h"
#include "../../commands/CommandManager.h"

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
      bool wasMinimized = pTrack->GetMinimized();
      for (auto channel : TrackList::Channels(pTrack.get()))
         channel->SetMinimized(!wasMinimized);
      pProject->ModifyState(true);

      // Redraw all tracks when any one of them expands or contracts
      // (Could we invent a return code that draws only those at or below
      // the affected track?)
      return RefreshAll | FixScrollbars;
   }

   return RefreshNone;
}

wxString MinimizeButtonHandle::Tip(const wxMouseState &) const
{
   auto pTrack = GetTrack();
   return pTrack->GetMinimized() ? _("Expand") : _("Collapse");
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
      const bool unsafe = pProject->IsAudioActive();
      SelectActions::DoListSelection(*pProject,
         pTrack.get(), event.ShiftDown(), event.ControlDown(), !unsafe);
//    return RefreshAll ;
   }

   return RefreshNone;
}

wxString SelectButtonHandle::Tip(const wxMouseState &) const
{
   auto pTrack = GetTrack();
#if defined(__WXMAC__)
   return pTrack->GetSelected() ? _("Command+Click to Unselect") : _("Select track");
#else
   return pTrack->GetSelected() ? _("Ctrl+Click to Unselect") : _("Select track");
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
      pProject->StopIfPaused();
      if (!pProject->IsAudioActive()) {
         // This pushes an undo item:
         TrackActions::DoRemoveTrack(*pProject, pTrack.get());
         // Redraw all tracks when any one of them closes
         // (Could we invent a return code that draws only those at or below
         // the affected track?)
         result |= Resize | RefreshAll | FixScrollbars | DestroyedCell;
      }
   }

   return result;
}

wxString CloseButtonHandle::Tip(const wxMouseState &) const
{
   auto name = _("Close");
   auto project = ::GetActiveProject();
   auto focused =
      project->GetTrackPanel()->GetFocusedTrack() == GetTrack().get();
   if (!focused)
      return name;

   auto commandManager = project->GetCommandManager();
   TranslatedInternalString command{ wxT("TrackClose"), name };
   return commandManager->DescribeCommandsAndShortcuts( &command, 1u );
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
   auto pPanel = pProject->GetTrackPanel();
   auto pCell = mpCell.lock();
   if (!pCell)
      return RefreshCode::Cancelled;
   auto pTrack =
      static_cast<CommonTrackPanelCell*>(pCell.get())->FindTrack();
   if (!pTrack)
      return RefreshCode::Cancelled;
   pPanel->CallAfter( [=]{ pPanel->OnTrackMenu( pTrack.get() ); } );
   return RefreshCode::RefreshNone;
}

wxString MenuButtonHandle::Tip(const wxMouseState &) const
{
   auto name = _("Open menu...");
   auto project = ::GetActiveProject();
   auto focused =
      project->GetTrackPanel()->GetFocusedTrack() == GetTrack().get();
   if (!focused)
      return name;

   auto commandManager = project->GetCommandManager();
   TranslatedInternalString command{ wxT("TrackMenu"), name };
   return commandManager->DescribeCommandsAndShortcuts( &command, 1u );
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
