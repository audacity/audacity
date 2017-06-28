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

HitTestResult LabelTrack::DetailedHitTest
(const TrackPanelMouseEvent &evt,
 const AudacityProject *pProject, int, bool)
{
   HitTestResult result;
   const wxMouseEvent &event = evt.event;

   // Try label movement handles first
   result = LabelGlyphHandle::HitTest(event, Pointer<LabelTrack>(this));

   if ( !result.handle )
      // Missed glyph, try text box
      result = LabelTextHandle::HitTest(event, Pointer<LabelTrack>(this));

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
