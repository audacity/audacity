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

std::shared_ptr<TrackControls> TimeTrack::GetControls()
{
   return std::make_shared<TimeTrackControls>( Pointer( this ) );
}

std::shared_ptr<TrackVRulerControls> TimeTrack::GetVRulerControls()
{
   return std::make_shared<TimeTrackVRulerControls>( Pointer( this ) );
}
