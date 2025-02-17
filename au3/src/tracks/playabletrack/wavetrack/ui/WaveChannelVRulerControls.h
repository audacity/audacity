/**********************************************************************

Audacity: A Digital Audio Editor

WaveChannelVRulerControls.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_WAVE_TRACK_VRULER_CONTROLS__
#define __AUDACITY_WAVE_TRACK_VRULER_CONTROLS__

#include "../../../ui/ChannelVRulerControls.h"

class Ruler;
class WaveTrack;

namespace WaveChannelVRulerControls {
AUDACITY_DLL_API Ruler& ScratchRuler();

AUDACITY_DLL_API void DoDraw(ChannelVRulerControls& controls, TrackPanelDrawingContext& context, const wxRect& rect, unsigned iPass);
}

#endif
