/**********************************************************************

Audacity: A Digital Audio Editor

TimeTrackUI.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../../TimeTrack.h"
#include "TimeTrackControls.h"
#include "TimeTrackVRulerControls.h"

#include "../../../HitTestResult.h"
#include "../../../Project.h"
#include "../../../toolbars/ToolsToolBar.h"

#include "../../ui/EnvelopeHandle.h"

HitTestResult TimeTrack::HitTest
(const TrackPanelMouseEvent &event,
 const AudacityProject *pProject)
{
   HitTestResult result = Track::HitTest(event, pProject);
   if (result.preview.cursor)
      return result;

   const ToolsToolBar *const pTtb = pProject->GetToolsToolBar();
   if (pTtb->IsDown(multiTool))
      // No hit test --unconditional availability.
      result = EnvelopeHandle::HitAnywhere(pProject);

   return result;
}

TrackControls *TimeTrack::GetControls()
{
   return &TimeTrackControls::Instance();
}

TrackVRulerControls *TimeTrack::GetVRulerControls()
{
   return &TimeTrackVRulerControls::Instance();
}
