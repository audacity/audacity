/**********************************************************************

Audacity: A Digital Audio Editor

LabelTrackUI.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../../LabelTrack.h"
#include "LabelTrackControls.h"
#include "LabelDefaultClickHandle.h"
#include "LabelTrackVRulerControls.h"
#include "LabelGlyphHandle.h"
#include "LabelTextHandle.h"

#include "../../ui/SelectHandle.h"

#include "../../../HitTestResult.h"
#include "../../../Project.h"
#include "../../../TrackPanelMouseEvent.h"
#include "../../../toolbars/ToolsToolBar.h"

HitTestResult LabelTrack::HitTest
(const TrackPanelMouseEvent &evt,
 const AudacityProject *pProject)
{
   // PRL: Maybe I did too much work to preserve old behavior, but anyway,
   // this unusually combines parts of two or more hit test results.

   HitTestResult result;
   const wxMouseEvent &event = evt.event;

   // Try label movement handles first
   result = LabelGlyphHandle::HitTest(event, this);
   // Hit test may request refresh even if a miss
   auto refreshResult = result.preview.refreshCode;

   if ( !result.handle ) {
      // Missed glyph, try text box
      // This hit test does not define its own messages or cursor
      HitTestResult defaultResult = Track::HitTest(evt, pProject);
      if (!defaultResult.handle) {
         // In case of multi tool, default to selection.
         const ToolsToolBar *const pTtb = pProject->GetToolsToolBar();
         if (pTtb->IsDown(multiTool))
            defaultResult = SelectHandle::HitTest(evt, pProject, this);
      }
      result = LabelTextHandle::HitTest(event, this);
      if (result.handle)
         // Use any cursor or status message change from catchall,
         // But let the text ui handle pass
         result.preview = defaultResult.preview;
      else
         result = defaultResult;
   }

   // Now attach some common extra work to the click action
   LabelDefaultClickHandle::Instance().mpForward = result.handle;
   result.handle = &LabelDefaultClickHandle::Instance();

   // Don't lose the refresh result side effect of the glyph
   // hit test
   result.preview.refreshCode |= refreshResult;

   return result;
}

std::shared_ptr<TrackControls> LabelTrack::GetControls()
{
   return std::make_shared<LabelTrackControls>( Pointer( this ) );
}

std::shared_ptr<TrackVRulerControls> LabelTrack::GetVRulerControls()
{
   return std::make_shared<LabelTrackVRulerControls>( Pointer( this ) );
}
