/**********************************************************************

Audacity: A Digital Audio Editor

PlayableTrackButtonHandles.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../../Audacity.h"
#include "PlayableTrackButtonHandles.h"

#include "../../../HitTestResult.h"
#include "../../../Project.h"
#include "../../../RefreshCode.h"
#include "../../../Track.h"
#include "../../../TrackPanel.h"
#include "../../../TrackPanelMouseEvent.h"

MuteButtonHandle::MuteButtonHandle()
   : ButtonHandle{ TrackPanel::IsMuting }
{
}

MuteButtonHandle::~MuteButtonHandle()
{
}

MuteButtonHandle &MuteButtonHandle::Instance()
{
   static MuteButtonHandle instance;
   return instance;
}

UIHandle::Result MuteButtonHandle::CommitChanges
   (const wxMouseEvent &event, AudacityProject *pProject, wxWindow *)
{
   auto pTrack = mpTrack.lock();
   if ( dynamic_cast< PlayableTrack* >( pTrack.get() ) )
      pProject->DoTrackMute(pTrack.get(), event.ShiftDown());

   return RefreshCode::RefreshNone;
}

HitTestResult MuteButtonHandle::HitTest
(const wxMouseState &state, const wxRect &rect,
 const AudacityProject *pProject, const std::shared_ptr<Track> &pTrack)
{
   wxRect buttonRect;
   if ( pTrack )
      TrackInfo::GetMuteSoloRect(rect, buttonRect, false,
         !pProject->IsSoloNone(), pTrack.get());
   if ( TrackInfo::HideTopItem( rect, buttonRect ) )
      return {};

   if ( pTrack && buttonRect.Contains(state.m_x, state.m_y) ) {
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

SoloButtonHandle::SoloButtonHandle()
   : ButtonHandle{ TrackPanel::IsSoloing }
{
}

SoloButtonHandle::~SoloButtonHandle()
{
}

SoloButtonHandle &SoloButtonHandle::Instance()
{
   static SoloButtonHandle instance;
   return instance;
}

UIHandle::Result SoloButtonHandle::CommitChanges
(const wxMouseEvent &event, AudacityProject *pProject, wxWindow *pParent)
{
   auto pTrack = mpTrack.lock();
   if ( dynamic_cast< PlayableTrack* >( pTrack.get() ) )
      pProject->DoTrackSolo(pTrack.get(), event.ShiftDown());

   return RefreshCode::RefreshNone;
}

HitTestResult SoloButtonHandle::HitTest
(const wxMouseState &state, const wxRect &rect,
 const AudacityProject *pProject, const std::shared_ptr<Track> &pTrack)
{
   wxRect buttonRect;
   if ( pTrack )
      TrackInfo::GetMuteSoloRect(rect, buttonRect, true,
         !pProject->IsSoloNone(), pTrack.get());

   if ( TrackInfo::HideTopItem( rect, buttonRect ) )
      return {};

   if ( pTrack && buttonRect.Contains(state.m_x, state.m_y) ) {
      Instance().mRect = buttonRect;
      return HitTestResult(
         HitPreview(),
         &Instance()
      );
   }
   else
      return HitTestResult();
}
