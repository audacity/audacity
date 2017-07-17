/**********************************************************************

Audacity: A Digital Audio Editor

PlayableTrackButtonHandles.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../../Audacity.h"
#include "PlayableTrackButtonHandles.h"

#include "../../../commands/CommandManager.h"
#include "../../../HitTestResult.h"
#include "../../../Project.h"
#include "../../../RefreshCode.h"
#include "../../../Track.h"
#include "../../../TrackPanel.h"
#include "../../../TrackPanelMouseEvent.h"

MuteButtonHandle::MuteButtonHandle
( const std::shared_ptr<Track> &pTrack, const wxRect &rect )
   : ButtonHandle{ pTrack, rect }
{}

MuteButtonHandle::~MuteButtonHandle()
{
}

UIHandle::Result MuteButtonHandle::CommitChanges
   (const wxMouseEvent &event, AudacityProject *pProject, wxWindow *)
{
   auto pTrack = mpTrack.lock();
   if ( dynamic_cast< PlayableTrack* >( pTrack.get() ) )
      pProject->DoTrackMute(pTrack.get(), event.ShiftDown());

   return RefreshCode::RefreshNone;
}

wxString MuteButtonHandle::Tip(const wxMouseState &) const
{
   auto name = _("Mute");
   auto project = ::GetActiveProject();
   auto focused =
      project->GetTrackPanel()->GetFocusedTrack() == GetTrack().get();
   if (!focused)
      return name;

   auto commandManager = project->GetCommandManager();
   std::vector<wxString> commands;
   commands.push_back(name);
   commands.push_back(wxT("TrackMute"));
   return commandManager->DescribeCommandsAndShortcuts(commands);
}

UIHandlePtr MuteButtonHandle::HitTest
(std::weak_ptr<MuteButtonHandle> &holder,
 const wxMouseState &state, const wxRect &rect,
 const AudacityProject *pProject, const std::shared_ptr<Track> &pTrack)
{
   wxRect buttonRect;
   if ( pTrack )
      TrackInfo::GetMuteSoloRect(rect, buttonRect, false,
         !pProject->IsSoloNone(), pTrack.get());
   if ( TrackInfo::HideTopItem( rect, buttonRect ) )
      return {};

   if ( pTrack && buttonRect.Contains(state.m_x, state.m_y) ) {
      auto result = std::make_shared<MuteButtonHandle>(pTrack, buttonRect);
      result = AssignUIHandlePtr(holder, result);
      return result;
   }
   else
      return {};
}

////////////////////////////////////////////////////////////////////////////////

SoloButtonHandle::SoloButtonHandle
( const std::shared_ptr<Track> &pTrack, const wxRect &rect )
   : ButtonHandle{ pTrack, rect }
{}

SoloButtonHandle::~SoloButtonHandle()
{
}

UIHandle::Result SoloButtonHandle::CommitChanges
(const wxMouseEvent &event, AudacityProject *pProject, wxWindow *pParent)
{
   auto pTrack = mpTrack.lock();
   if ( dynamic_cast< PlayableTrack* >( pTrack.get() ) )
      pProject->DoTrackSolo(pTrack.get(), event.ShiftDown());

   return RefreshCode::RefreshNone;
}

wxString SoloButtonHandle::Tip(const wxMouseState &) const
{
   auto name = _("Solo");
   auto project = ::GetActiveProject();
   auto focused =
      project->GetTrackPanel()->GetFocusedTrack() == GetTrack().get();
   if (!focused)
      return name;

   auto commandManager = project->GetCommandManager();
   std::vector<wxString> commands;
   commands.push_back(name);
   commands.push_back(wxT("TrackSolo"));
   return commandManager->DescribeCommandsAndShortcuts(commands);
}

UIHandlePtr SoloButtonHandle::HitTest
(std::weak_ptr<SoloButtonHandle> &holder,
 const wxMouseState &state, const wxRect &rect,
 const AudacityProject *pProject, const std::shared_ptr<Track> &pTrack)
{
   wxRect buttonRect;
   if ( pTrack )
      TrackInfo::GetMuteSoloRect(rect, buttonRect, true,
         !pProject->IsSoloNone(), pTrack.get());

   if ( TrackInfo::HideTopItem( rect, buttonRect ) )
      return {};

   if ( pTrack && buttonRect.Contains(state.m_x, state.m_y) ) {
      auto result = std::make_shared<SoloButtonHandle>( pTrack, buttonRect );
      result = AssignUIHandlePtr(holder, result);
      return result;
   }
   else
      return {};
}
