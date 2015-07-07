/**********************************************************************

Audacity: A Digital Audio Editor

WaveTrackControls.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../../../Audacity.h"
#include "WaveTrackControls.h"
#include "../../../../HitTestResult.h"

WaveTrackControls::WaveTrackControls()
{
}

WaveTrackControls &WaveTrackControls::Instance()
{
   static WaveTrackControls instance;
   return instance;
}

WaveTrackControls::~WaveTrackControls()
{
}


HitTestResult WaveTrackControls::HitTest
(const TrackPanelMouseEvent & event,
 const AudacityProject *pProject)
{
   return TrackControls::HitTest(event, pProject);
}
