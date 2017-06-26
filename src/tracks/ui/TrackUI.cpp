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

#include "../ui/SelectHandle.h"
#include "EnvelopeHandle.h"
#include "../playabletrack/wavetrack/ui/SampleHandle.h"
#include "ZoomHandle.h"
#include "TimeShiftHandle.h"

HitTestResult Track::HitTest
(const TrackPanelMouseEvent &event,
 const AudacityProject *pProject)
{
   const ToolsToolBar * pTtb = pProject->GetToolsToolBar();
   // Unless in Multimode keep using the current tool.
   const bool isMultiTool = pTtb->IsDown(multiTool);
   if (!isMultiTool) {
      switch (pTtb->GetCurrentTool()) {
      case envelopeTool:
         // Pass "false" for unsafe -- let the tool decide to cancel itself
         return EnvelopeHandle::HitAnywhere(pProject);
      case drawTool:
         return SampleHandle::HitAnywhere(event.event, pProject);
      case zoomTool:
         return ZoomHandle::HitAnywhere(event.event, pProject);
      case slideTool:
         return TimeShiftHandle::HitAnywhere(pProject);
      case selectTool:
         return SelectHandle::HitTest(event, pProject, this);

      default:
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

std::shared_ptr<TrackPanelCell> Track::GetTrackControl()
{
   if (!mpControls)
      // create on demand
      mpControls = GetControls();
   return mpControls;
}

std::shared_ptr<TrackPanelCell> Track::GetVRulerControl()
{
   if (!mpVRulerContols)
      // create on demand
      mpVRulerContols = GetVRulerControls();
   return mpVRulerContols;
}

#include "../../TrackPanelResizeHandle.h"
std::shared_ptr<TrackPanelCell> Track::GetResizer()
{
   if (!mpResizer)
      // create on demand
      mpResizer = std::make_shared<TrackPanelResizerCell>( Pointer( this ) );
   return mpResizer;
}
