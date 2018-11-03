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
   TimeTrackVRulerControls( const std::shared_ptr<TrackView> &pTrackView )
      : TrackVRulerControls( pTrackView ) {}
   ~TimeTrackVRulerControls();

private:

   // TrackPanelDrawable implementation
   void Draw(
      TrackPanelDrawingContext &context,
      const wxRect &rect, unsigned iPass ) override;

   // TrackVRulerControls implementation
   void UpdateRuler( const wxRect &rect ) override;

};

#endif
