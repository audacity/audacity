/**********************************************************************

Audacity: A Digital Audio Editor

WaveformVRulerControls.h

Paul Licameli split from WaveChannelVRulerControls.h

**********************************************************************/

#ifndef __AUDACITY_WAVEFORM_VRULER_CONTROLS__
#define __AUDACITY_WAVEFORM_VRULER_CONTROLS__

#include "../../../ui/ChannelVRulerControls.h" // to inherit

class WaveChannel;
class WaveformVZoomHandle;

class WaveformVRulerControls final : public ChannelVRulerControls
{
    WaveformVRulerControls(const WaveformVRulerControls&) = delete;
    WaveformVRulerControls& operator=(const WaveformVRulerControls&) = delete;

public:
    explicit
    WaveformVRulerControls(const std::shared_ptr<ChannelView>& pChannelView)
        : ChannelVRulerControls{pChannelView} {}
    ~WaveformVRulerControls() override;

    std::vector<UIHandlePtr> HitTest(
        const TrackPanelMouseState& state, const AudacityProject*) override;

    unsigned HandleWheelRotation(
        const TrackPanelMouseEvent& event, AudacityProject* pProject) override;
    static unsigned DoHandleWheelRotation(
        const TrackPanelMouseEvent& event, AudacityProject* pProject, WaveChannel& wc);

    std::shared_ptr<WaveChannel> FindWaveChannel();

private:
    // TrackPanelDrawable implementation
    void Draw(
        TrackPanelDrawingContext& context, const wxRect& rect, unsigned iPass) override;

    // ChannelVRulerControls implementation
    void UpdateRuler(const wxRect& rect) override;

    static void DoUpdateVRuler(const wxRect& rect, const WaveChannel& wc);

    std::weak_ptr<WaveformVZoomHandle> mVZoomHandle;
};

#endif
