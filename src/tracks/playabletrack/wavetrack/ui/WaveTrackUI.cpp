/**********************************************************************

Audacity: A Digital Audio Editor

WaveTrackUI.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../../../WaveTrack.h"
#include "WaveTrackControls.h"
#include "WaveTrackVRulerControls.h"

#include "../../../../HitTestResult.h"
#include "../../../../Project.h"
#include "../../../../TrackPanelMouseEvent.h"
#include "../../../../toolbars/ToolsToolBar.h"

#include "../../../ui/EnvelopeHandle.h"
#include "SampleHandle.h"
#include "../../../ui/TimeShiftHandle.h"

HitTestResult WaveTrack::HitTest
(const TrackPanelMouseEvent &event,
 const AudacityProject *pProject)
{
   HitTestResult result = Track::HitTest(event, pProject);
   if (result.preview.cursor)
      return result;

   const ToolsToolBar *const pTtb = pProject->GetToolsToolBar();
   if (pTtb->IsDown(multiTool)) {
      // Replicate some of the logic of TrackPanel::DetermineToolToUse
      int currentTool = -1;
      if (event.event.CmdDown())
         result = TimeShiftHandle::HitAnywhere(pProject);
      else if (NULL !=
         (result = EnvelopeHandle::WaveTrackHitTest(event.event, event.rect, pProject, this))
         .preview.cursor)
         ;
      else if (NULL != (result =
         TimeShiftHandle::HitTest(event.event, event.rect, pProject)).preview.cursor)
         ;
      else if (NULL != (result =
         SampleHandle::HitTest(event.event, event.rect, pProject, this)).preview.cursor)
         ;
   }

   return result;
}

TrackControls *WaveTrack::GetControls()
{
   return &WaveTrackControls::Instance();
}

TrackVRulerControls *WaveTrack::GetVRulerControls()
{
   return &WaveTrackVRulerControls::Instance();
}
