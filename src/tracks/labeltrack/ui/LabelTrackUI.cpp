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

UIHandlePtr LabelTrack::DetailedHitTest
(const TrackPanelMouseState &st,
 const AudacityProject *pProject, int, bool)
{
   const wxMouseState &state = st.state;

   // Try label movement handles first
   UIHandlePtr result;
   result = LabelGlyphHandle::HitTest(
      mGlyphHandle, state, Pointer<LabelTrack>(this), st.rect);

   if ( !result )
      // Missed glyph, try text box
      result = LabelTextHandle::HitTest(
         mTextHandle, state, Pointer<LabelTrack>(this));

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
