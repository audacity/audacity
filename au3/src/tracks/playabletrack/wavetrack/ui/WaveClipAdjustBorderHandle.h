/*!********************************************************************
*
 Audacity: A Digital Audio Editor

 WaveClipAdjustBorderHandle.h

 Vitaly Sverchinsky

 **********************************************************************/

#pragma once

#include "UIHandle.h"
#include "WaveClip.h"

class WaveChannelView;
class WaveTrack;

class WaveClipAdjustBorderHandle final : public UIHandle
{
public:
    class AdjustPolicy;
private:

    static constexpr int BoundaryThreshold = 5;

    static HitTestPreview HitPreviewTrim(const AudacityProject*, bool unsafe, bool isLeftBorder);
    static HitTestPreview HitPreviewStretch(const AudacityProject*, bool unsafe, bool isLeftBorder);

    std::unique_ptr<AdjustPolicy> mAdjustPolicy{};
    std::shared_ptr<const WaveTrack> mpTrack;
    bool mIsStretchMode;
    bool mIsLeftBorder;

public:
    WaveClipAdjustBorderHandle(std::unique_ptr<AdjustPolicy>& adjustPolicy, std::shared_ptr<const WaveTrack> pTrack, bool stretchMode,
                               bool leftBorder);
    ~WaveClipAdjustBorderHandle() override;

    std::shared_ptr<const Track> FindTrack() const override;

    WaveClipAdjustBorderHandle(WaveClipAdjustBorderHandle&&) noexcept;
    WaveClipAdjustBorderHandle& operator =(WaveClipAdjustBorderHandle&&) noexcept;

    static UIHandlePtr HitAnywhere(std::weak_ptr<WaveClipAdjustBorderHandle>& holder, const std::shared_ptr<WaveTrack>& waveTrack,
                                   const AudacityProject* pProject, const TrackPanelMouseState& state);

    static UIHandlePtr HitTest(std::weak_ptr<WaveClipAdjustBorderHandle>& holder, WaveChannelView& view, const AudacityProject* pProject,
                               const TrackPanelMouseState& state);

    HitTestPreview Preview(const TrackPanelMouseState& mouseState, AudacityProject* pProject) override;

    Result Click(const TrackPanelMouseEvent& event, AudacityProject* pProject) override;

    Result Drag(const TrackPanelMouseEvent& event, AudacityProject* pProject) override;

    Result Release(const TrackPanelMouseEvent& event, AudacityProject* pProject, wxWindow* pParent) override;

    Result Cancel(AudacityProject* pProject) override;

    // TrackPanelDrawable implementation

    void Draw(TrackPanelDrawingContext& context, const wxRect& rect, unsigned iPass) override;

    wxRect DrawingArea(TrackPanelDrawingContext&, const wxRect& rect, const wxRect& panelRect, unsigned iPass) override;
};
