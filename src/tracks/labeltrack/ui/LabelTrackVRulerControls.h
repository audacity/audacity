/**********************************************************************

Audacity: A Digital Audio Editor

LabelTrackVRulerControls.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_LABEL_TRACK_VRULER_CONTROLS__
#define __AUDACITY_LABEL_TRACK_VRULER_CONTROLS__

#include "../../ui/TrackVRulerControls.h"

// This class is here for completeness, by analogy with other track
// types, but it does nothing.
class LabelTrackVRulerControls final : public TrackVRulerControls
{
   LabelTrackVRulerControls(const LabelTrackVRulerControls&) = delete;
   LabelTrackVRulerControls &operator=(const LabelTrackVRulerControls&)
      = delete;

public:
   explicit
   LabelTrackVRulerControls( std::shared_ptr<Track> pTrack )
      : TrackVRulerControls( pTrack ) {}
   ~LabelTrackVRulerControls();
};

#endif
