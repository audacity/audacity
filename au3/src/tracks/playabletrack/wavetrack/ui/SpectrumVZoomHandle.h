/**********************************************************************

Audacity: A Digital Audio Editor

SpectrumVZoomHandle.h

Paul Licameli split from WaveChannelVZoomHandle.h

**********************************************************************/

#ifndef __AUDACITY_SPECTRUM_VZOOM_HANDLE__
#define __AUDACITY_SPECTRUM_VZOOM_HANDLE__

#include "../../../../UIHandle.h" // to inherit
#include "WaveChannelViewConstants.h"

class WaveChannel;
class WaveTrack;

class SpectrumVZoomHandle final : public UIHandle
{
    SpectrumVZoomHandle(const SpectrumVZoomHandle&);

public:
    SpectrumVZoomHandle(
        const std::shared_ptr<WaveChannel>& pChannel, const wxRect& rect, int y);

    SpectrumVZoomHandle& operator=(const SpectrumVZoomHandle&) = default;

    static void DoZoom(
        AudacityProject* pProject, WaveChannel& wc, WaveChannelViewConstants::ZoomActions ZoomKind, const wxRect& rect, int zoomStart,
        int zoomEnd, bool fixedMousePoint);

    ~SpectrumVZoomHandle() override;

    std::shared_ptr<const Track> FindTrack() const override;
    std::shared_ptr<WaveChannel> FindWaveChannel();

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

class SpectrumVRulerMenuTable : public WaveChannelVRulerMenuTable
{
    SpectrumVRulerMenuTable()
        : WaveChannelVRulerMenuTable{"SpectrumVRuler"}
    {}
    virtual ~SpectrumVRulerMenuTable() {}
    DECLARE_POPUP_MENU(SpectrumVRulerMenuTable);

public:
    static PopupMenuTable& Instance();

private:
    void OnSpectrumScaleType(wxCommandEvent& evt);
};

#endif
