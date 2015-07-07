/**********************************************************************

Audacity: A Digital Audio Editor

TimeTrackVRulerControls.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../../Audacity.h"
#include "TimeTrackVRulerControls.h"
#include "../../../HitTestResult.h"

TimeTrackVRulerControls::TimeTrackVRulerControls()
   : TrackVRulerControls()
{
}

TimeTrackVRulerControls &TimeTrackVRulerControls::Instance()
{
   static TimeTrackVRulerControls instance;
   return instance;
}

TimeTrackVRulerControls::~TimeTrackVRulerControls()
{
}
