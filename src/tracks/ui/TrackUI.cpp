/**********************************************************************

Audacity: A Digital Audio Editor

TrackUI.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../Track.h"
#include "../../TrackPanelMouseEvent.h"
#include "TrackControls.h"
#include "TrackVRulerControls.h"

#include "../../HitTestResult.h"
#include "../../Project.h"
#include "../../toolbars/ToolsToolBar.h"

#include "ZoomHandle.h"

HitTestResult Track::HitTest
(const TrackPanelMouseEvent &event,
 const AudacityProject *pProject)
{
   const ToolsToolBar * pTtb = pProject->GetToolsToolBar();
   // Unless in Multimode keep using the current tool.
   const bool isMultiTool = pTtb->IsDown(multiTool);
   if (!isMultiTool) {
      switch (pTtb->GetCurrentTool()) {
      case zoomTool:
         return ZoomHandle::HitAnywhere(event.event, pProject);

      case selectTool:
      case envelopeTool:
      case drawTool:
      case slideTool:
      default:
         // cases not yet implemented
         // fallthru
         ;
      }
   }

   // Replicate some of the logic of TrackPanel::DetermineToolToUse
   HitTestResult result;

   if (isMultiTool)
      result = ZoomHandle::HitTest(event.event, pProject);

   return result;
}

TrackPanelCell *Track::GetTrackControl()
{
   TrackControls *const result = GetControls();
   result->mpTrack = this;
   return result;
}

TrackPanelCell *Track::GetVRulerControl()
{
   TrackVRulerControls *const result = GetVRulerControls();
   result->mpTrack = this;
   return result;
}
