/**********************************************************************

Audacity: A Digital Audio Editor

WaveformVZoomHandle.h

Paul Licameli split from WaveChannelVZoomHandle.h

**********************************************************************/

#ifndef __AUDACITY_WAVEFORM_VZOOM_HANDLE__
#define __AUDACITY_WAVEFORM_VZOOM_HANDLE__

#include "../../../../UIHandle.h" // to inherit
#include "WaveChannelViewConstants.h"

class WaveChannel;
class WaveTrack;

class WaveformVZoomHandle final : public UIHandle
{
    WaveformVZoomHandle(const WaveformVZoomHandle&);

public:
    explicit WaveformVZoomHandle(
        const std::shared_ptr<WaveChannel>& pChannel, const wxRect& rect, int y);

    WaveformVZoomHandle& operator=(const WaveformVZoomHandle&) = default;

    static void DoZoom(
        AudacityProject* pProject, WaveChannel& wc, WaveChannelViewConstants::ZoomActions ZoomKind, const wxRect& rect, int zoomStart,
        int zoomEnd, bool fixedMousePoint);

    ~WaveformVZoomHandle() override;

    std::shared_ptr<const Track> FindTrack() const override;

    void Enter(bool forward, AudacityProject*) override;

    bool HandlesRightClick() override;

    Result Click(const TrackPanelMouseEvent& event, AudacityProject* pProject) override;

    Result Drag(const TrackPanelMouseEvent& event, AudacityProject* pProject) override;

    HitTestPreview Preview(const TrackPanelMouseState& state, AudacityProject* pProject)
    override;

    Result Release(const TrackPanelMouseEvent& event, AudacityProject* pProject, wxWindow* pParent) override;

    Result Cancel(AudacityProject* pProject) override;

private:

    // TrackPanelDrawable implementation
    void Draw(
        TrackPanelDrawingContext& context, const wxRect& rect, unsigned iPass) override;

    wxRect DrawingArea(
        TrackPanelDrawingContext&, const wxRect& rect, const wxRect& panelRect, unsigned iPass) override;

    std::weak_ptr<WaveChannel> mpChannel;

    int mZoomStart{}, mZoomEnd{};
    wxRect mRect{};
};

#include "WaveChannelVZoomHandle.h" // to inherit

class WaveformVRulerMenuTable : public WaveChannelVRulerMenuTable
{
    WaveformVRulerMenuTable()
        : WaveChannelVRulerMenuTable{"WaveFormVRuler"}
    {}
    virtual ~WaveformVRulerMenuTable() {}
    DECLARE_POPUP_MENU(WaveformVRulerMenuTable);

public:
    static PopupMenuTable& Instance();

private:
    void OnWaveformScaleType(wxCommandEvent& evt);
};

#endif
