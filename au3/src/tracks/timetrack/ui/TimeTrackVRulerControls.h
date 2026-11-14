/**********************************************************************

Audacity: A Digital Audio Editor

TimeTrackVRulerControls.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_TIME_TRACK_VRULER_CONTROLS__
#define __AUDACITY_TIME_TRACK_VRULER_CONTROLS__

#include "../../ui/ChannelVRulerControls.h"

class TimeTrack;
class TimeTrackVZoomHandle;

// This class is here for completeness, by analogy with other track
// types, but it does nothing.
class TimeTrackVRulerControls final : public ChannelVRulerControls
{
    TimeTrackVRulerControls(const TimeTrackVRulerControls&) = delete;
    TimeTrackVRulerControls& operator=(const TimeTrackVRulerControls&) = delete;

public:
    explicit
    TimeTrackVRulerControls(const std::shared_ptr<ChannelView>& pChannelView)
        : ChannelVRulerControls{pChannelView} {}
    ~TimeTrackVRulerControls();

    std::shared_ptr<TimeTrack> FindTimeTrack();

    std::vector<UIHandlePtr> HitTest(
        const TrackPanelMouseState& state, const AudacityProject*) override;

private:

    // TrackPanelDrawable implementation
    void Draw(
        TrackPanelDrawingContext& context, const wxRect& rect, unsigned iPass) override;

    // ChannelVRulerControls implementation
    void UpdateRuler(const wxRect& rect) override;

    std::weak_ptr<TimeTrackVZoomHandle> mVZoomHandle;
};

#endif
