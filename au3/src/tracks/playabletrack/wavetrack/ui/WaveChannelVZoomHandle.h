/**********************************************************************

Audacity: A Digital Audio Editor

WaveChannelVZoomHandle.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_WAVE_TRACK_VZOOM_HANDLE__
#define __AUDACITY_WAVE_TRACK_VZOOM_HANDLE__

class wxMouseState;
class PopupMenuTable;
class WaveChannel;
class WaveTrack;
#include "WaveChannelViewConstants.h"
#include "../../../../UIHandle.h"
#include "Prefs.h"

namespace WaveChannelVZoomHandle {
// See RefreshCode.h for bit flags:
using Result = unsigned;

AUDACITY_DLL_API
HitTestPreview HitPreview(const bool bVZoom);

AUDACITY_DLL_API
bool IsDragZooming(int zoomStart, int zoomEnd, bool hasDragZoom);

using DoZoomFunction = void (*)(AudacityProject* pProject,
                                WaveChannel& wc,
                                WaveChannelViewConstants::ZoomActions ZoomKind,
                                const wxRect& rect, int zoomStart, int zoomEnd,
                                bool fixedMousePoint);

AUDACITY_DLL_API
Result DoDrag(
    const TrackPanelMouseEvent& event, AudacityProject* pProject, int zoomStart, int& zoomEnd, bool hasDragZoom);

AUDACITY_DLL_API
Result DoRelease(
    const TrackPanelMouseEvent& event, AudacityProject* pProject, wxWindow* pParent, WaveChannel& wc, const wxRect& mRect,
    DoZoomFunction doZoom, PopupMenuTable& table, int zoomStart, int zoomEnd);

AUDACITY_DLL_API
void DoDraw(
    TrackPanelDrawingContext& context, const wxRect& rect, unsigned iPass, int zoomStart, int zoomEnd, bool hasDragZoom);

AUDACITY_DLL_API
wxRect DoDrawingArea(
    const wxRect& rect, const wxRect& panelRect, unsigned iPass);
}

#include "../../../../widgets/PopupMenuTable.h" // to inherit

class AUDACITY_DLL_API WaveChannelVRulerMenuTable : public PopupMenuTable, private PrefsListener
{
public:
    struct InitMenuData
    {
    public:
        AudacityProject& project;
        WaveChannel& wc;
        wxRect rect;
        unsigned result;
        int yy;
        WaveChannelVZoomHandle::DoZoomFunction doZoom;
    };

protected:
    WaveChannelVRulerMenuTable(const Identifier& id)
        : PopupMenuTable{id}
    {}

    void InitUserData(void* pUserData) override;

protected:
    InitMenuData* mpData {};

    void OnZoom(WaveChannelViewConstants::ZoomActions iZoomCode);
    void OnZoomFitVertical(wxCommandEvent&)
    { OnZoom(WaveChannelViewConstants::kZoom1to1); }
    void OnZoomReset(wxCommandEvent&)
    { OnZoom(WaveChannelViewConstants::kZoomReset); }
    void OnZoomHalfWave(wxCommandEvent&)
    { OnZoom(WaveChannelViewConstants::kZoomHalfWave); }
    void OnZoomInVertical(wxCommandEvent&)
    { OnZoom(WaveChannelViewConstants::kZoomIn); }
    void OnZoomOutVertical(wxCommandEvent&)
    { OnZoom(WaveChannelViewConstants::kZoomOut); }

    void UpdatePrefs() override;
};

enum {
    OnZoomFitVerticalID = 20000,
    OnZoomResetID,
    OnZoomHalfWaveID,
    OnZoomInVerticalID,
    OnZoomOutVerticalID,

    // Reserve an ample block of ids for waveform scale types
    OnFirstWaveformScaleID,
    OnLastWaveformScaleID = OnFirstWaveformScaleID + 9,

    // Reserve an ample block of ids for spectrum scale types
    OnFirstSpectrumScaleID,
    OnLastSpectrumScaleID = OnFirstSpectrumScaleID + 19,
};

#endif
