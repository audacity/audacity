/**********************************************************************

Sneedacity: A Digital Audio Editor

WaveTrackVRulerControls.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __SNEEDACITY_WAVE_TRACK_VRULER_CONTROLS__
#define __SNEEDACITY_WAVE_TRACK_VRULER_CONTROLS__

#include "../../../ui/TrackVRulerControls.h"

class Ruler;
class WaveTrack;

namespace WaveTrackVRulerControls
{
   SNEEDACITY_DLL_API Ruler &ScratchRuler();

   SNEEDACITY_DLL_API void DoDraw( TrackVRulerControls &controls,
      TrackPanelDrawingContext &context,
      const wxRect &rect, unsigned iPass );
};

#endif
