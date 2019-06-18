/**********************************************************************

Audacity: A Digital Audio Editor

TimeTrackControls.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_TIME_TRACK_CONTROLS__
#define __AUDACITY_TIME_TRACK_CONTROLS__

#include "../../ui/CommonTrackControls.h" // to inherit

class TimeTrackControls final : public CommonTrackControls
{
   TimeTrackControls(const TimeTrackControls&) = delete;
   TimeTrackControls &operator=(const TimeTrackControls&) = delete;

public:
   explicit
   TimeTrackControls( std::shared_ptr<Track> pTrack )
      : CommonTrackControls( pTrack ) {}
   ~TimeTrackControls();

   std::vector<UIHandlePtr> HitTest
      (const TrackPanelMouseState &state,
       const AudacityProject *pProject) override;

   PopupMenuTable *GetMenuExtension(Track *pTrack) override;
};

#endif
