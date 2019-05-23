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

std::vector<UIHandlePtr> Track::HitTest
(const TrackPanelMouseState &st,
 const AudacityProject *pProject)
{
   UIHandlePtr result;
   std::vector<UIHandlePtr> results;
   const ToolsToolBar * pTtb = pProject->GetToolsToolBar();
   const bool isMultiTool = pTtb->IsDown(multiTool);
   const auto currentTool = pTtb->GetCurrentTool();

   if ( !isMultiTool && currentTool == zoomTool ) {
      // Zoom tool is a non-selecting tool that takes precedence in all tracks
      // over all other tools, no matter what detail you point at.
      result = ZoomHandle::HitAnywhere(
         BackgroundCell::Get( *pProject ).mZoomHandle );
      results.push_back(result);
      return results;
   }

   // In other tools, let subclasses determine detailed hits.
   results =
      DetailedHitTest( st, pProject, currentTool, isMultiTool );

   // There are still some general cases.

   // Sliding applies in more than one track type.
   if ( !isMultiTool && currentTool == slideTool ) {
      result = TimeShiftHandle::HitAnywhere(
         mTimeShiftHandle, SharedPointer(), false);
      if (result)
         results.push_back(result);
   }

   // Let the multi-tool right-click handler apply only in default of all
   // other detailed hits.
   if ( isMultiTool ) {
      result = ZoomHandle::HitTest(
         BackgroundCell::Get( *pProject ).mZoomHandle, st.state);
      if (result)
         results.push_back(result);
   }

   // Finally, default of all is adjustment of the selection box.
   if ( isMultiTool || currentTool == selectTool ) {
      result = SelectHandle::HitTest(
         mSelectHandle, st, pProject, SharedPointer());
      if (result)
         results.push_back(result);
   }

   return results;
}

std::shared_ptr<TrackPanelCell> Track::ContextMenuDelegate()
{
   return FindTrack()->GetTrackControl();
}

std::shared_ptr<TrackControls> Track::GetTrackControl()
{
   if (!mpControls)
      // create on demand
      mpControls = DoGetControls();
   return mpControls;
}

std::shared_ptr<const TrackControls> Track::GetTrackControl() const
{
   return const_cast< Track* >( this )->GetTrackControl();
}

std::shared_ptr<TrackVRulerControls> Track::GetVRulerControl()
{
   if (!mpVRulerContols)
      // create on demand
      mpVRulerContols = DoGetVRulerControls();
   return mpVRulerContols;
}

std::shared_ptr<const TrackVRulerControls> Track::GetVRulerControl() const
{
   return const_cast< Track* >( this )->GetVRulerControl();
}

#include "../../TrackPanelResizeHandle.h"
std::shared_ptr<TrackPanelCell> Track::GetResizer()
{
   if (!mpResizer)
      // create on demand
      mpResizer = std::make_shared<TrackPanelResizerCell>( SharedPointer() );
   return mpResizer;
}
