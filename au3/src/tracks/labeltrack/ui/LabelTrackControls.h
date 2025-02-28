/**********************************************************************

Audacity: A Digital Audio Editor

LabelTrackControls.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_LABEL_TRACK_CONTROLS__
#define __AUDACITY_LABEL_TRACK_CONTROLS__

#include "../../ui/CommonTrackControls.h" // to inherit

class LabelTrackControls final : public CommonTrackControls
{
    LabelTrackControls(const LabelTrackControls&) = delete;
    LabelTrackControls& operator=(const LabelTrackControls&) = delete;

public:
    explicit
    LabelTrackControls(std::shared_ptr<Track> pTrack)
        : CommonTrackControls(pTrack) {}
    ~LabelTrackControls();

    std::vector<UIHandlePtr> HitTest(const TrackPanelMouseState& state, const AudacityProject* pProject) override;

    PopupMenuTable* GetMenuExtension(Track* pTrack) override;
};

#endif
