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
#include "../../TrackPanelResizerCell.h"
#include "BackgroundCell.h"

HitTestResult Track::HitTest
(const TrackPanelMouseState &st,
 const AudacityProject *pProject)
{
   const ToolsToolBar * pTtb = pProject->GetToolsToolBar();
   const bool isMultiTool = pTtb->IsDown(multiTool);
   const auto currentTool = pTtb->GetCurrentTool();

   if ( !isMultiTool && currentTool == zoomTool )
      // Zoom tool is a non-selecting tool that takes precedence in all tracks
      // over all other tools, no matter what detail you point at.
      return ZoomHandle::HitAnywhere(
         pProject->GetBackgroundCell()->mZoomHandle, st.state, pProject);

   // In other tools, let subclasses determine detailed hits.
   HitTestResult result =
      DetailedHitTest( st, pProject, currentTool, isMultiTool );

   // If there is no detailed hit for the subclass, there are still some
   // general cases.

   // Sliding applies in more than one track type.
   if ( !result.handle && !isMultiTool && currentTool == slideTool )
      result = TimeShiftHandle::HitAnywhere(
         mTimeShiftHandle, pProject, Pointer(this), false);

   // Let the multi-tool right-click handler apply only in default of all
   // other detailed hits.
   if ( !result.handle && isMultiTool )
      result = ZoomHandle::HitTest(
         pProject->GetBackgroundCell()->mZoomHandle, st.state, pProject);

   // Finally, default of all is adjustment of the selection box.
   if ( !result.handle && ( isMultiTool || currentTool == selectTool) )
      result = SelectHandle::HitTest(
         mSelectHandle, st, pProject, Pointer(this));

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
