/**********************************************************************

Audacity: A Digital Audio Editor

ChannelVRulerControls.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "ChannelVRulerControls.h"

#include "ChannelView.h"

#include "AColor.h"
#include "Track.h"
#include "../../TrackArtist.h"
#include "../../TrackPanelDrawingContext.h"
#include "ViewInfo.h"

#include <wx/cursor.h>
#include <wx/dc.h>
#include <wx/translation.h>

ChannelVRulerControls::ChannelVRulerControls(
    const std::shared_ptr<ChannelView>& pChannelView)
    : CommonChannelCell{pChannelView->FindChannel()}
    , mwChannelView{pChannelView}
{
}

ChannelVRulerControls::~ChannelVRulerControls()
{
}

ChannelVRulerControls& ChannelVRulerControls::Get(ChannelView& trackView)
{
    return *trackView.GetVRulerControls();
}

const ChannelVRulerControls& ChannelVRulerControls::Get(
    const ChannelView& trackView)
{
    return *trackView.GetVRulerControls();
}

std::shared_ptr<Track> ChannelVRulerControls::DoFindTrack()
{
    // Just pass-through to related ChannelView object
    const auto pView = mwChannelView.lock();
    if (pView) {
        return pView->DoFindTrack();
    }
    return {};
}

std::vector<UIHandlePtr> ChannelVRulerControls::HitTest(
    const TrackPanelMouseState&, const AudacityProject*)
{
    return std::vector<UIHandlePtr> {};
}

void ChannelVRulerControls::DrawZooming(
    TrackPanelDrawingContext& context, const wxRect& rect_,
    int zoomStart, int zoomEnd)
{
    // Draw a dashed rectangle, its right side disappearing in the black right
    // border of the track area, which is not part of this cell but right of it.
    auto& dc = context.dc;

    dc.SetBrush(*wxTRANSPARENT_BRUSH);
    dc.SetPen(*wxBLACK_DASHED_PEN);

    wxRect rect {
        rect_.x,
        std::min(zoomStart, zoomEnd),
        rect_.width,
        1 + abs(zoomEnd - zoomStart)
    };

    dc.DrawRectangle(rect);
}

wxRect ChannelVRulerControls::ZoomingArea(
    const wxRect& rect, const wxRect& panelRect)
{
    // TODO: Don't use the constant kRightMargin, but somehow discover the
    // neighboring track rectangle
    return {
        // Left edge of the rectangle disappears in the vertical line at
        // left edge of the ruler
        rect.x,
        rect.y,
        // Extend the dashed rectangle right up to the track border
        (panelRect.width - kRightMargin + kBorderThickness) - rect.x,
        rect.height
    };
}

wxRect ChannelVRulerControls::DrawingArea(
    TrackPanelDrawingContext&,
    const wxRect& rect, const wxRect&, unsigned iPass)
{
    // Common area change for all subclasses when drawing the controls
    // A bevel extends below one pixel outside of the hit-test area
    if (iPass == TrackArtist::PassControls) {
        return { rect.x, rect.y, rect.width, rect.height + 1 }
    } else {
        return rect;
    }
}
