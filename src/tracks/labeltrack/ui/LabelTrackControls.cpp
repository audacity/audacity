/**********************************************************************

Audacity: A Digital Audio Editor

LabelTrackControls.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../../Audacity.h"
#include "LabelTrackControls.h"
#include "../../../HitTestResult.h"

LabelTrackControls::LabelTrackControls()
{
}

LabelTrackControls &LabelTrackControls::Instance()
{
   static LabelTrackControls instance;
   return instance;
}

LabelTrackControls::~LabelTrackControls()
{
}

HitTestResult LabelTrackControls::HitTest
(const TrackPanelMouseEvent & event,
 const AudacityProject *pProject)
{
   return TrackControls::HitTest(event, pProject);
}
