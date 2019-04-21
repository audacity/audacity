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

std::vector<UIHandlePtr> LabelTrack::DetailedHitTest
(const TrackPanelMouseState &st,
 const AudacityProject *WXUNUSED(pProject), int, bool)
{
   UIHandlePtr result;
   std::vector<UIHandlePtr> results;
   const wxMouseState &state = st.state;

   result = LabelGlyphHandle::HitTest(
      mGlyphHandle, state, SharedPointer<LabelTrack>(), st.rect);
   if (result)
      results.push_back(result);

   result = LabelTextHandle::HitTest(
      mTextHandle, state, SharedPointer<LabelTrack>());
   if (result)
      results.push_back(result);

   return results;
}

std::shared_ptr<TrackControls> LabelTrack::DoGetControls()
{
   return std::make_shared<LabelTrackControls>( SharedPointer() );
}

std::shared_ptr<TrackVRulerControls> LabelTrack::DoGetVRulerControls()
{
   return std::make_shared<LabelTrackVRulerControls>( SharedPointer() );
}
