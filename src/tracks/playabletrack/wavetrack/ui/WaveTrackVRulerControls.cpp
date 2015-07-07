/**********************************************************************

Audacity: A Digital Audio Editor

WaveTrackVRulerControls.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../../../Audacity.h"
#include "WaveTrackVRulerControls.h"
#include "../../../../HitTestResult.h"

WaveTrackVRulerControls::WaveTrackVRulerControls()
   : TrackVRulerControls()
{
}

WaveTrackVRulerControls &WaveTrackVRulerControls::Instance()
{
   static WaveTrackVRulerControls instance;
   return instance;
}

WaveTrackVRulerControls::~WaveTrackVRulerControls()
{
}

HitTestResult WaveTrackVRulerControls::HitTest
(const TrackPanelMouseEvent &,
 const AudacityProject *)
{
   return {};
}
