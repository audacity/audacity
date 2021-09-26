/**********************************************************************

Audacity: A Digital Audio Editor

PlayableTrackButtonHandles.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/


#include "PlayableTrackButtonHandles.h"

#include "PlayableTrackControls.h"
#include "../../../commands/CommandManager.h"
#include "Project.h"
#include "../../../ProjectSettings.h"
#include "../../../RefreshCode.h"
#include "../../../Track.h"
#include "../../../TrackPanelAx.h"
#include "../../../TrackInfo.h"
#include "../../../TrackPanelMouseEvent.h"
#include "../../../TrackUtilities.h"

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
      TrackUtilities::DoTrackMute(*pProject, pTrack.get(), event.ShiftDown());

   return RefreshCode::RefreshNone;
}

TranslatableString MuteButtonHandle::Tip(
   const wxMouseState &, AudacityProject &project) const
{
   auto name = XO("Mute");
   auto focused =
      TrackFocus::Get( project ).Get() == GetTrack().get();
   if (!focused)
      return name;

   auto &commandManager = CommandManager::Get( project );
   ComponentInterfaceSymbol command{ wxT("TrackMute"), name };
   return commandManager.DescribeCommandsAndShortcuts(&command, 1u);
}

UIHandlePtr MuteButtonHandle::HitTest
(std::weak_ptr<MuteButtonHandle> &holder,
 const wxMouseState &state, const wxRect &rect,
 const AudacityProject *pProject, const std::shared_ptr<Track> &pTrack)
{
   wxRect buttonRect;
   if ( pTrack )
      PlayableTrackControls::GetMuteSoloRect(rect, buttonRect, false,
         !ProjectSettings::Get( *pProject ).IsSoloNone(), pTrack.get());
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
(const wxMouseEvent &event, AudacityProject *pProject, wxWindow *WXUNUSED(pParent))
{
   auto pTrack = mpTrack.lock();
   if ( dynamic_cast< PlayableTrack* >( pTrack.get() ) )
      TrackUtilities::DoTrackSolo(*pProject, pTrack.get(), event.ShiftDown());

   return RefreshCode::RefreshNone;
}

TranslatableString SoloButtonHandle::Tip(
   const wxMouseState &, AudacityProject &project) const
{
   auto name = XO("Solo");
   auto focused =
      TrackFocus::Get( project ).Get() == GetTrack().get();
   if (!focused)
      return name;

   auto &commandManager = CommandManager::Get( project );
   ComponentInterfaceSymbol command{ wxT("TrackSolo"), name };
   return commandManager.DescribeCommandsAndShortcuts( &command, 1u );
}

UIHandlePtr SoloButtonHandle::HitTest
(std::weak_ptr<SoloButtonHandle> &holder,
 const wxMouseState &state, const wxRect &rect,
 const AudacityProject *pProject, const std::shared_ptr<Track> &pTrack)
{
   wxRect buttonRect;
   if ( pTrack )
      PlayableTrackControls::GetMuteSoloRect(rect, buttonRect, true,
         !ProjectSettings::Get( *pProject ).IsSoloNone(), pTrack.get());

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
