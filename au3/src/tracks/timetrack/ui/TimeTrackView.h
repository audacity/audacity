/**********************************************************************

Audacity: A Digital Audio Editor

TimeTrackView.h

Paul Licameli split from class TimeTrack

**********************************************************************/

#ifndef __AUDACITY_TIME_TRACK_VIEW__
#define __AUDACITY_TIME_TRACK_VIEW__

#include "../../ui/CommonChannelView.h"

class EnvelopeHandle;

class TimeTrackView final : public CommonChannelView
{
   TimeTrackView( const TimeTrackView& ) = delete;
   TimeTrackView &operator=( const TimeTrackView& ) = delete;

public:
   explicit TimeTrackView(const std::shared_ptr<Channel> &pChannel);
   ~TimeTrackView() override;

   std::shared_ptr<ChannelVRulerControls> DoGetVRulerControls() override;

private:

   std::vector<UIHandlePtr> DetailedHitTest
      (const TrackPanelMouseState &state,
       const AudacityProject *pProject, int currentTool, bool bMultiTool)
      override;

   std::weak_ptr<EnvelopeHandle> mEnvelopeHandle;

   // TrackPanelDrawable implementation
   void Draw(
      TrackPanelDrawingContext &context,
      const wxRect &rect, unsigned iPass ) override;
};

#endif
