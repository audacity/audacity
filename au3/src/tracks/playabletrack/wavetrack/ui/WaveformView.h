/**********************************************************************

Audacity: A Digital Audio Editor

WaveformView.h

Paul Licameli split from WaveChannelView.h

**********************************************************************/

#ifndef __AUDACITY_WAVEFORM_VIEW__
#define __AUDACITY_WAVEFORM_VIEW__

#include "WaveChannelView.h" // to inherit

class WaveTrack;
class SampleHandle;
class EnvelopeHandle;

class WaveformView final : public WaveChannelSubView
{
    WaveformView(const WaveformView&) = delete;
    WaveformView& operator=(const WaveformView&) = delete;

public:
    using WaveChannelSubView::WaveChannelSubView;
    ~WaveformView() override;

    const Type& SubViewType() const override;

    std::shared_ptr<ChannelVRulerControls> DoGetVRulerControls() override;

private:
    // TrackPanelDrawable implementation
    void Draw(
        TrackPanelDrawingContext& context, const wxRect& rect, unsigned iPass) override;
    static void DoDraw(TrackPanelDrawingContext& context, const WaveChannel& channel, const WaveTrack::Interval* selectedClip,
                       const wxRect& rect, bool muted);

    std::vector<UIHandlePtr> DetailedHitTest(
        const TrackPanelMouseState& state, const AudacityProject* pProject, int currentTool, bool bMultiTool)
    override;

protected:
    void DoSetMinimized(bool minimized) override;

    std::weak_ptr<SampleHandle> mSampleHandle;
    std::weak_ptr<EnvelopeHandle> mEnvelopeHandle;
};

#endif
