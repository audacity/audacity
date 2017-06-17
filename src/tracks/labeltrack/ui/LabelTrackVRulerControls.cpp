/**********************************************************************

Audacity: A Digital Audio Editor

LabelTrackVRulerControls.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../../Audacity.h"
#include "LabelTrackVRulerControls.h"
#include "../../../HitTestResult.h"

LabelTrackVRulerControls::LabelTrackVRulerControls()
   : TrackVRulerControls()
{
}

LabelTrackVRulerControls &LabelTrackVRulerControls::Instance()
{
   static LabelTrackVRulerControls instance;
   return instance;
}

LabelTrackVRulerControls::~LabelTrackVRulerControls()
{
}
