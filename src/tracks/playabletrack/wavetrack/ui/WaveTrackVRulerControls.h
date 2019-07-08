/**********************************************************************

Audacity: A Digital Audio Editor

WaveTrackVRulerControls.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_WAVE_TRACK_VRULER_CONTROLS__
#define __AUDACITY_WAVE_TRACK_VRULER_CONTROLS__

#include "../../../ui/TrackVRulerControls.h"

class Ruler;
class WaveTrack;

namespace WaveTrackVRulerControls
{
   Ruler &ScratchRuler();

   void DoDraw( TrackVRulerControls &controls,
      TrackPanelDrawingContext &context,
      const wxRect &rect, unsigned iPass );
};

#endif
