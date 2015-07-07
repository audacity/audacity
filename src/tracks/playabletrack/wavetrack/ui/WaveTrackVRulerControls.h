/**********************************************************************

Audacity: A Digital Audio Editor

WaveTrackVRulerControls.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_WAVE_TRACK_VRULER_CONTROLS__
#define __AUDACITY_WAVE_TRACK_VRULER_CONTROLS__

#include "../../../ui/TrackVRulerControls.h"

class WaveTrackVRulerControls final : public TrackVRulerControls
{
   WaveTrackVRulerControls();
   WaveTrackVRulerControls(const WaveTrackVRulerControls&) = delete;
   WaveTrackVRulerControls &operator=(const WaveTrackVRulerControls&) = delete;

public:
   static WaveTrackVRulerControls &Instance();
   ~WaveTrackVRulerControls();

   HitTestResult HitTest
      (const TrackPanelMouseEvent &event,
       const AudacityProject *) override;
};

#endif
