/**********************************************************************

Sneedacity: A Digital Audio Editor

TimeTrackView.h

Paul Licameli split from class TimeTrack

**********************************************************************/

#ifndef __SNEEDACITY_TIME_TRACK_VIEW__
#define __SNEEDACITY_TIME_TRACK_VIEW__

#include "../../ui/CommonTrackView.h"

class EnvelopeHandle;

class TimeTrackView final : public CommonTrackView
{
   TimeTrackView( const TimeTrackView& ) = delete;
   TimeTrackView &operator=( const TimeTrackView& ) = delete;

public:
   explicit
   TimeTrackView( const std::shared_ptr<Track> &pTrack );
   ~TimeTrackView() override;

   std::shared_ptr<TrackVRulerControls> DoGetVRulerControls() override;

private:
   std::vector<UIHandlePtr> DetailedHitTest
      (const TrackPanelMouseState &state,
       const SneedacityProject *pProject, int currentTool, bool bMultiTool)
      override;

   std::weak_ptr<EnvelopeHandle> mEnvelopeHandle;

   // TrackPanelDrawable implementation
   void Draw(
      TrackPanelDrawingContext &context,
      const wxRect &rect, unsigned iPass ) override;
};

#endif
