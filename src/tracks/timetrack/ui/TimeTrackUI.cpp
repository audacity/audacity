/**********************************************************************

Audacity: A Digital Audio Editor

TimeTrackUI.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../../TimeTrack.h"
#include "TimeTrackControls.h"
#include "TimeTrackVRulerControls.h"

#include "../../../HitTestResult.h"

HitTestResult TimeTrack::HitTest
(const TrackPanelMouseEvent &event,
 const AudacityProject *pProject)
{
   return Track::HitTest(event, pProject);
}

TrackControls *TimeTrack::GetControls()
{
   return &TimeTrackControls::Instance();
}

TrackVRulerControls *TimeTrack::GetVRulerControls()
{
   return &TimeTrackVRulerControls::Instance();
}
