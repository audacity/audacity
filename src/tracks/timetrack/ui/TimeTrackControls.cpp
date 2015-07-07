/**********************************************************************

Audacity: A Digital Audio Editor

TimeTrackControls.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../../Audacity.h"
#include "TimeTrackControls.h"
#include "../../../HitTestResult.h"

TimeTrackControls::TimeTrackControls()
{
}

TimeTrackControls &TimeTrackControls::Instance()
{
   static TimeTrackControls instance;
   return instance;
}

TimeTrackControls::~TimeTrackControls()
{
}

HitTestResult TimeTrackControls::HitTest
(const TrackPanelMouseEvent & event,
 const AudacityProject *pProject)
{
   return TrackControls::HitTest(event, pProject);
}
