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

#include "SampleHandle.h"

HitTestResult WaveTrack::HitTest
(const TrackPanelMouseEvent &event,
 const AudacityProject *pProject)
{
   HitTestResult result = Track::HitTest(event, pProject);
   if (result.preview.cursor)
      return result;

   const ToolsToolBar *const pTtb = pProject->GetToolsToolBar();
   if (pTtb->IsDown(multiTool)) {
      if (NULL != (result =
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
