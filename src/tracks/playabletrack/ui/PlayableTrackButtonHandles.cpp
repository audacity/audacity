/**********************************************************************

Audacity: A Digital Audio Editor

PlayableTrackButtonHandles.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

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
   if ( dynamic_cast< PlayableTrack* >( mpTrack ) )
      pProject->DoTrackMute(mpTrack, event.ShiftDown());

   return RefreshCode::RefreshNone;
}

HitTestResult MuteButtonHandle::HitTest
(const wxMouseEvent &event, const wxRect &rect,
 const AudacityProject *pProject, const Track *pTrack)
{
   wxRect buttonRect;
   if ( pTrack )
      TrackInfo::GetMuteSoloRect(rect, buttonRect, false,
         !pProject->IsSoloNone(), pTrack);
   if ( TrackInfo::HideTopItem( rect, buttonRect ) )
      return {};

   if ( pTrack && buttonRect.Contains(event.m_x, event.m_y) ) {
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
   if ( dynamic_cast< PlayableTrack* >( mpTrack ) )
      pProject->DoTrackSolo(mpTrack, event.ShiftDown());

   return RefreshCode::RefreshNone;
}

HitTestResult SoloButtonHandle::HitTest
(const wxMouseEvent &event, const wxRect &rect,
 const AudacityProject *pProject, const Track *pTrack)
{
   wxRect buttonRect;
   if ( pTrack )
      TrackInfo::GetMuteSoloRect(rect, buttonRect, true,
         !pProject->IsSoloNone(), pTrack);

   if ( TrackInfo::HideTopItem( rect, buttonRect ) )
      return {};

   if ( pTrack && buttonRect.Contains(event.m_x, event.m_y) ) {
      Instance().mRect = buttonRect;
      return HitTestResult(
         HitPreview(),
         &Instance()
      );
   }
   else
      return HitTestResult();
}
