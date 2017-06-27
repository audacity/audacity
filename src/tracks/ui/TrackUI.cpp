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
   const bool isMultiTool = pTtb->IsDown(multiTool);
   const auto currentTool = pTtb->GetCurrentTool();

   if ( !isMultiTool && currentTool == zoomTool )
      // Zoom tool is a non-selecting tool that takes precedence in all tracks
      // over all other tools, no matter what detail you point at.
      return ZoomHandle::HitAnywhere(event.event, pProject);

   // In other tools, let subclasses determine detailed hits.
   HitTestResult result =
      DetailedHitTest( event, pProject, currentTool, isMultiTool );

   // If there is no detailed hit for the subclass, there are still some
   // general cases.

   // Sliding applies in more than one track type.
   if ( !result.handle && !isMultiTool && currentTool == slideTool )
      result = TimeShiftHandle::HitAnywhere(pProject);

   // Let the multi-tool right-click handler apply only in default of all
   // other detailed hits.
   if ( !result.handle && isMultiTool )
      result = ZoomHandle::HitTest(event.event, pProject);

   // Finally, default of all is adjustment of the selection box.
   if ( !result.handle && ( isMultiTool || currentTool == selectTool) )
      result = SelectHandle::HitTest(event, pProject, this);

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
