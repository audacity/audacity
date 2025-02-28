/**********************************************************************

Audacity: A Digital Audio Editor

LabelTrackVRulerControls.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_LABEL_TRACK_VRULER_CONTROLS__
#define __AUDACITY_LABEL_TRACK_VRULER_CONTROLS__

#include "../../ui/ChannelVRulerControls.h"

// This class is here for completeness, by analogy with other track
// types, but it does nothing.
class LabelTrackVRulerControls final : public ChannelVRulerControls
{
    LabelTrackVRulerControls(const LabelTrackVRulerControls&) = delete;
    LabelTrackVRulerControls& operator=(const LabelTrackVRulerControls&)
    = delete;

public:
    explicit
    LabelTrackVRulerControls(const std::shared_ptr<ChannelView>& pChannelView)
        : ChannelVRulerControls{pChannelView} {}
    ~LabelTrackVRulerControls();

private:

    // TrackPanelDrawable implementation
    void Draw(
        TrackPanelDrawingContext& context, const wxRect& rect, unsigned iPass) override;

    // ChannelVRulerControls implementation
    void UpdateRuler(const wxRect& rect) override;
};

#endif
