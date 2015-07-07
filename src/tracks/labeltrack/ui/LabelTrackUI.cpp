/**********************************************************************

Audacity: A Digital Audio Editor

LabelTrackUI.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../../LabelTrack.h"
#include "LabelTrackControls.h"
#include "LabelTrackVRulerControls.h"

#include "../../../HitTestResult.h"

HitTestResult LabelTrack::HitTest
(const TrackPanelMouseEvent &event,
 const AudacityProject *pProject)
{
   return Track::HitTest(event, pProject);
}

TrackControls *LabelTrack::GetControls()
{
   return &LabelTrackControls::Instance();
}

TrackVRulerControls *LabelTrack::GetVRulerControls()
{
   return &LabelTrackVRulerControls::Instance();
}
