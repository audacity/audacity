/**********************************************************************

Audacity: A Digital Audio Editor

TimeTrackUI.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../../TimeTrack.h"

#include "TimeTrackControls.h"
#include "TimeTrackVRulerControls.h"

#include "../../../HitTestResult.h"
#include "../../../TrackPanelMouseEvent.h"
#include "../../../Project.h"

#include "../../ui/EnvelopeHandle.h"

std::vector<UIHandlePtr> TimeTrack::DetailedHitTest
(const TrackPanelMouseState &st,
 const AudacityProject *pProject, int, bool)
{
   std::vector<UIHandlePtr> results;
   auto result = EnvelopeHandle::TimeTrackHitTest(
      mEnvelopeHandle, st.state, st.rect, pProject, SharedPointer<TimeTrack>() );
   if (result)
      results.push_back(result);
   return results;
}

std::shared_ptr<TrackControls> TimeTrack::DoGetControls()
{
   return std::make_shared<TimeTrackControls>( SharedPointer() );
}

std::shared_ptr<TrackVRulerControls> TimeTrack::DoGetVRulerControls()
{
   return std::make_shared<TimeTrackVRulerControls>( SharedPointer() );
}
