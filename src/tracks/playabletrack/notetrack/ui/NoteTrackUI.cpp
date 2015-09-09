/**********************************************************************

Audacity: A Digital Audio Editor

NoteTrackUI.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../../../Audacity.h"

#ifdef USE_MIDI

#include "../../../../NoteTrack.h"
#include "NoteTrackControls.h"
#include "NoteTrackVRulerControls.h"

#include "../../../../HitTestResult.h"
#include "../../../../Project.h"
#include "../../../../TrackPanelMouseEvent.h"
#include "../../../ui/SelectHandle.h"
#include "StretchHandle.h"
#include "../../../../toolbars/ToolsToolBar.h"

HitTestResult NoteTrack::HitTest
(const TrackPanelMouseEvent &event,
 const AudacityProject *pProject)
{
   const ToolsToolBar *const pTtb = pProject->GetToolsToolBar();

   // If the cursor is in the hot zone for stretching, that takes precedence
   // over selection, but it didn't take precedence over other hits.
   // That was the old logic, and maybe I tried too hard to preserve it just so.
   // PRL.

   // Eligible for stretch?
   HitTestResult result1;
#ifdef USE_MIDI
   StretchHandle::StretchState state;
   result1 = StretchHandle::HitTest( event, pProject, this, state );
#endif

   // But some other non-select tool like zoom may take priority.
   HitTestResult result = Track::HitTest(event, pProject);
   if (result.preview.cursor &&
       !(result1.preview.cursor && pTtb->GetCurrentTool() == selectTool))
      return result;

   if (pTtb->IsDown(multiTool)) {
      // Default to selection
      if (!result1.preview.cursor &&
          NULL != (result =
         SelectHandle::HitTest(event, pProject, this)).preview.cursor)
         return result;
   }

   // Do stretch!
   if (result1.preview.cursor)
      return result1;

   return result;
}

TrackControls *NoteTrack::GetControls()
{
   return &NoteTrackControls::Instance();
}

TrackVRulerControls *NoteTrack::GetVRulerControls()
{
   return &NoteTrackVRulerControls::Instance();
}
#endif
