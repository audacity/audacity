/*!********************************************************************
*
 Audacity: A Digital Audio Editor

 WaveClipTrimHandle.h

 Vitaly Sverchinsky

 **********************************************************************/

#pragma once 

#include "UIHandle.h"
#include "WaveClip.h"

class WaveTrackView;
class WaveTrack;

class WaveClipTrimHandle : public UIHandle
{
    static constexpr int BoundaryThreshold = 5;

    enum class Border {
        Left,
        Right
    };

    std::pair<double, double> mRange;
    std::vector<std::shared_ptr<WaveClip>> mClips;

    int     mDragStartX{};
    Border  mTargetBorder{ Border::Left };
    double  mInitialBorderPosition{};

    static HitTestPreview HitPreview(const AudacityProject*, bool unsafe);

public:
    WaveClipTrimHandle(const std::pair<double, double>& range, const std::vector<std::shared_ptr<WaveClip>>& clips, Border targetBorder);

    static UIHandlePtr HitAnywhere(std::weak_ptr<WaveClipTrimHandle>& holder,
        WaveTrack* waveTrack,
        const AudacityProject* pProject,
        const TrackPanelMouseState& state);

    static UIHandlePtr HitTest(std::weak_ptr<WaveClipTrimHandle>& holder,
        WaveTrackView& view, const AudacityProject* pProject,
        const TrackPanelMouseState& state);

    HitTestPreview Preview(const TrackPanelMouseState& mouseState, AudacityProject* pProject) override;

    virtual Result Click
    (const TrackPanelMouseEvent& event, AudacityProject* pProject) override;

    virtual Result Drag
    (const TrackPanelMouseEvent& event, AudacityProject* pProject) override;

    virtual Result Release
    (const TrackPanelMouseEvent& event, AudacityProject* pProject,
        wxWindow* pParent) override;

    virtual Result Cancel(AudacityProject* pProject) override;
};
