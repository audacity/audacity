/**********************************************************************

Audacity: A Digital Audio Editor

WaveChannelVZoomHandle.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "WaveChannelVZoomHandle.h"

#include "../../../ui/ChannelVRulerControls.h"

#include "../../../../HitTestResult.h"
#include "Project.h"
#include "../../../../RefreshCode.h"
#include "../../../../TrackArtist.h"
#include "../../../../TrackPanelMouseEvent.h"
#include "WaveTrack.h"
#include "../../../../../images/Cursors.h"

bool WaveChannelVZoomHandle::IsDragZooming(int zoomStart, int zoomEnd, bool hasDragZoom)
{
    const int DragThreshold = 3;// Anything over 3 pixels is a drag, else a click.
    return hasDragZoom && (abs(zoomEnd - zoomStart) > DragThreshold);
}

///////////////////////////////////////////////////////////////////////////////
// Table class

void WaveChannelVRulerMenuTable::InitUserData(void* pUserData)
{
    mpData = static_cast<InitMenuData*>(pUserData);
}

void WaveChannelVRulerMenuTable::OnZoom(
    WaveChannelViewConstants::ZoomActions iZoomCode)
{
    mpData->doZoom(
        &mpData->project, mpData->wc,
        iZoomCode, mpData->rect, mpData->yy, mpData->yy, false
        );

    using namespace RefreshCode;
    mpData->result = UpdateVRuler | RefreshAll;
}

void WaveChannelVRulerMenuTable::UpdatePrefs()
{
    // Because labels depend on advanced vertical zoom setting
    PopupMenuTable::Clear();
}

///////////////////////////////////////////////////////////////////////////////

HitTestPreview WaveChannelVZoomHandle::HitPreview(const bool bVZoom)
{
    static wxCursor crossCursor{ wxCURSOR_CROSS };
    static wxCursor arrowCursor{ wxCURSOR_ARROW };
    const auto message = bVZoom
                         ? XO("Drag to specify a zoom region. Right-click for menu. Ctrl+scroll to zoom.")
                         : XO("Right-click for menu. Ctrl+scroll to zoom.");

    return {
        message,
        bVZoom ? &crossCursor : &arrowCursor
        // , message
    };
}

UIHandle::Result WaveChannelVZoomHandle::DoDrag(
    const TrackPanelMouseEvent& evt, AudacityProject* pProject,
    const int zoomStart, int& zoomEnd, bool hasDragZooming)
{
    using namespace RefreshCode;

    const wxMouseEvent& event = evt.event;
    if (event.RightIsDown()) {
        return RefreshNone;
    }
    zoomEnd = event.m_y;
    if (IsDragZooming(zoomStart, zoomEnd, hasDragZooming)) {
        return RefreshAll;
    }
    return RefreshNone;
}

UIHandle::Result WaveChannelVZoomHandle::DoRelease(
    const TrackPanelMouseEvent& evt, AudacityProject* pProject,
    wxWindow* pParent, WaveChannel& wc, const wxRect& rect,
    DoZoomFunction doZoom, PopupMenuTable& table,
    int zoomStart, int zoomEnd)
{
    using namespace RefreshCode;
    const wxMouseEvent& event = evt.event;
    const bool shiftDown = event.ShiftDown();
    const bool rightUp = event.RightUp();

    // Popup menu...
    using namespace WaveChannelViewConstants;
    if (
        rightUp
        && !(event.ShiftDown() || event.CmdDown())) {
        WaveChannelVRulerMenuTable::InitMenuData data {
            *pProject,
            wc, rect, RefreshCode::RefreshNone, event.m_y, doZoom };

        auto pMenu = PopupMenuTable::BuildMenu(&table, &data);
        pMenu->Popup(*pParent, { event.m_x, event.m_y });

        return data.result;
    } else {
        // Ignore Capture Lost event
        bool notLost = event.GetId() != kCaptureLostEventId;
        // Shift+rightclick to reset zoom
        if (shiftDown && notLost) {
            zoomStart = zoomEnd;
        }
        doZoom(pProject, wc, kZoom1to1,
               rect, zoomStart, zoomEnd, !shiftDown);
    }

    return UpdateVRuler | RefreshAll;
}

void WaveChannelVZoomHandle::DoDraw(
    TrackPanelDrawingContext& context,
    const wxRect& rect, unsigned iPass, const int zoomStart, const int zoomEnd, bool hasDragZoom)
{
    if (iPass == TrackArtist::PassZooming) {
        if (IsDragZooming(zoomStart, zoomEnd, hasDragZoom)) {
            ChannelVRulerControls::DrawZooming(context, rect, zoomStart, zoomEnd);
        }
    }
}

wxRect WaveChannelVZoomHandle::DoDrawingArea(
    const wxRect& rect, const wxRect& panelRect, unsigned iPass)
{
    if (iPass == TrackArtist::PassZooming) {
        return ChannelVRulerControls::ZoomingArea(rect, panelRect);
    } else {
        return rect;
    }
}
