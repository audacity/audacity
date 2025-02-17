/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  ClipOverflowButtonHandle.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "ClipOverflowButtonHandle.h"
#include "AllThemeResources.h"
#include "HitTestResult.h"
#include "LowlitClipButton.h"
#include "ProjectHistory.h"
#include "RefreshCode.h"
#include "Theme.h"
#include "TrackArt.h"
#include "TrackPanelCell.h"
#include "TrackPanelMouseEvent.h"
#include "ViewInfo.h"
#include "WaveClip.h"
#include "WaveTrack.h"
#include <wx/dc.h>
#include <wx/event.h>

namespace {
void SelectInterval(
    AudacityProject& project, const WaveTrack::Interval& interval)
{
    auto& viewInfo = ViewInfo::Get(project);
    viewInfo.selectedRegion.setTimes(
        interval.GetPlayStartTime(), interval.GetPlayEndTime());
    ProjectHistory::Get(project).ModifyState(false);
}
} // namespace

ClipOverflowButtonHandle::ClipOverflowButtonHandle(
    const std::shared_ptr<WaveTrack>& track,
    const std::shared_ptr<WaveTrack::Interval>& clip,
    std::weak_ptr<TrackPanelCell> cell)
    : HighlitClipButtonHandle{ClipButtonId::Overflow, track, clip}
    , mCell{std::move(cell)}
{
}

void ClipOverflowButtonHandle::DoDraw(const wxRect& rect, wxDC& dc)
{
    const ClipInterface& clip = *mClip;
    ClipButtonDrawingArgs args { rect, clip, dc };
    ClipButtonSpecializations<ClipButtonId::Overflow>::DrawOnClip(args);
}

UIHandle::Result ClipOverflowButtonHandle::DoRelease(
    const TrackPanelMouseEvent& event, AudacityProject* pProject,
    wxWindow* pParent)
{
    if (const auto cell = mCell.lock()) {
        SelectInterval(*pProject, *mClip);
        const wxPoint point { event.event.GetPosition() };
        return cell->DoContextMenu(event.rect, pParent, &point, pProject);
    }
    return RefreshCode::RefreshNone;
}

HitTestPreview ClipOverflowButtonHandle::Preview(
    const TrackPanelMouseState& state, AudacityProject* pProject)
{
    return { XO("Click to open clip context menu."), nullptr };
}

int ClipButtonSpecializations<ClipButtonId::Overflow>::GetWidth(
    const ClipInterface&)
{
    return 30;
}

bool ClipButtonSpecializations<ClipButtonId::Overflow>::NeedsDrawing(
    const ClipInterface&)
{
    return true;
}

void ClipButtonSpecializations<ClipButtonId::Overflow>::DrawOnClip(
    ClipButtonDrawingArgs& args)
{
    auto& dc = args.dc;
    const auto& rect = args.rect;
    constexpr auto penWidth = 0;
    wxDCPenChanger pen { args.dc, { wxPenInfo { theTheme.Colour(clrClipNameText), penWidth } } };
    wxDCBrushChanger brush { dc, theTheme.Colour(clrClipNameText) };
    constexpr auto numDots = 3;
    constexpr auto radius = 1;
    constexpr auto spacing = 4;
    constexpr auto totalWidth = numDots * 2 * radius + 2 * spacing;
    const auto dotY = rect.y + (rect.height + 1 - radius) / 2;
    for (auto i = 0; i < numDots; ++i) {
        const auto dotX = rect.x + (rect.width + 1 - totalWidth) / 2
                          + i * (2 * radius + spacing);
        dc.DrawCircle(dotX, dotY, radius);
    }
}
