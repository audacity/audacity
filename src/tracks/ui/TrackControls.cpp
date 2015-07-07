/**********************************************************************

Audacity: A Digital Audio Editor

TrackControls.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../Audacity.h"
#include "TrackControls.h"
#include "../../HitTestResult.h"

TrackControls::~TrackControls()
{
}

HitTestResult TrackControls::HitTest
(const TrackPanelMouseEvent &,
 const AudacityProject *)
{
   return {};
}

Track *TrackControls::FindTrack()
{
   return GetTrack();
}
