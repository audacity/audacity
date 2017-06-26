/**********************************************************************

Audacity: A Digital Audio Editor

TimeTrackVRulerControls.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_TIME_TRACK_VRULER_CONTROLS__
#define __AUDACITY_TIME_TRACK_VRULER_CONTROLS__

#include "../../ui/TrackVRulerControls.h"

// This class is here for completeness, by analogy with other track
// types, but it does nothing.
class TimeTrackVRulerControls final : public TrackVRulerControls
{
   TimeTrackVRulerControls(const TimeTrackVRulerControls&) = delete;
   TimeTrackVRulerControls &operator=(const TimeTrackVRulerControls&) = delete;

public:
   explicit
   TimeTrackVRulerControls( std::shared_ptr<Track> pTrack )
      : TrackVRulerControls( pTrack ) {}
   ~TimeTrackVRulerControls();
};

#endif
