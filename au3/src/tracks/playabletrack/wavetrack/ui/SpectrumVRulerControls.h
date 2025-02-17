/**********************************************************************

Audacity: A Digital Audio Editor

SpectrumVRulerControls.h

Paul Licameli split from WaveChannelVRulerControls.h

**********************************************************************/

#ifndef __AUDACITY_SPECTRUM_VRULER_CONTROLS__
#define __AUDACITY_SPECTRUM_VRULER_CONTROLS__

#include "../../../ui/ChannelVRulerControls.h" // to inherit

class WaveChannel;
class SpectrumVZoomHandle;

class SpectrumVRulerControls final : public ChannelVRulerControls
{
    SpectrumVRulerControls(const SpectrumVRulerControls&) = delete;
    SpectrumVRulerControls& operator=(const SpectrumVRulerControls&) = delete;

public:
    explicit
    SpectrumVRulerControls(const std::shared_ptr<ChannelView>& pChannelView)
        : ChannelVRulerControls{pChannelView} {}
    ~SpectrumVRulerControls() override;

    std::vector<UIHandlePtr> HitTest(
        const TrackPanelMouseState& state, const AudacityProject*) override;

    unsigned HandleWheelRotation(
        const TrackPanelMouseEvent& event, AudacityProject* pProject) override;
    static unsigned DoHandleWheelRotation(
        const TrackPanelMouseEvent& evt, AudacityProject* pProject, WaveChannel& wc);

    std::shared_ptr<WaveChannel> FindWaveChannel();

private:
    // TrackPanelDrawable implementation
    void Draw(
        TrackPanelDrawingContext& context, const wxRect& rect, unsigned iPass) override;

    // ChannelVRulerControls implementation
    void UpdateRuler(const wxRect& rect) override;

    static void DoUpdateVRuler(const wxRect& rect, const WaveChannel& wc);

    std::weak_ptr<SpectrumVZoomHandle> mVZoomHandle;
};

#endif
