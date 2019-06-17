/**********************************************************************

Audacity: A Digital Audio Editor

TimeTrackView.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "TimeTrackView.h"
#include "../../../TimeTrack.h"

#include "TimeTrackControls.h"
#include "TimeTrackVRulerControls.h"

#include "TimeTrackVRulerControls.h"
#include "../../../HitTestResult.h"
#include "../../../TrackPanelMouseEvent.h"

#include "../../ui/EnvelopeHandle.h"

TimeTrackView::~TimeTrackView()
{
}

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

std::shared_ptr<TrackView> TimeTrack::DoGetView()
{
   return std::make_shared<TimeTrackView>( SharedPointer() );
}

std::shared_ptr<TrackControls> TimeTrack::DoGetControls()
{
   return std::make_shared<TimeTrackControls>( SharedPointer() );
}

std::shared_ptr<TrackVRulerControls> TimeTrackView::DoGetVRulerControls()
{
   return
      std::make_shared<TimeTrackVRulerControls>( shared_from_this() );
}
