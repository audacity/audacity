/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  HighlitClipButtonHandle.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "HighlitClipButtonHandle.h"
#include "AllThemeResources.h"
#include "ClipParameters.h"
#include "HitTestResult.h"
#include "LowlitClipButton.h"
#include "ProjectHistory.h"
#include "RefreshCode.h"
#include "SelectionState.h"
#include "Theme.h"
#include "TrackArt.h"
#include "TrackArtist.h"
#include "TrackPanelDrawingContext.h"
#include "TrackPanelMouseEvent.h"
#include "ViewInfo.h"
#include "WaveClip.h"
#include "WaveTrack.h"
#include <wx/dc.h>
#include <wx/window.h>

HighlitClipButtonHandle::HighlitClipButtonHandle(
    ClipButtonId id, std::shared_ptr<WaveTrack> track,
    std::shared_ptr<WaveTrack::Interval> clip)
    : mButtonId{id}
    , mTrack{std::move(track)}
    , mClip{std::move(clip)}
{
}

void HighlitClipButtonHandle::Draw(
    TrackPanelDrawingContext& context, const wxRect& affordanceRect,
    unsigned iPass)
{
    if (iPass != TrackArtist::PassSnapping) {
        return;
    }
    const auto artist = TrackArtist::Get(context);
    const auto& zoomInfo = *artist->pZoomInfo;
    const auto rect = LowlitClipButton::Detail::GetButtonInnerRectangle(
        mButtonId, { *mClip, zoomInfo, affordanceRect });
    if (!rect) {
        return;
    }
    Highlight(*rect, context.dc);
    DoDraw(*rect, context.dc);
}

UIHandle::Result HighlitClipButtonHandle::Click(
    const TrackPanelMouseEvent& event, AudacityProject* pProject)
{
    mwCell = event.pCell;
    return 0;
}

UIHandle::Result HighlitClipButtonHandle::Drag(
    const TrackPanelMouseEvent& event, AudacityProject* pProject)
{
    const auto cancelCode = RefreshCode::RefreshCell | RefreshCode::Cancelled;
    if (event.pCell != mwCell.lock()) {
        return cancelCode;
    }

    // It's the right cell; check that the mouse is still over the button.
    const auto buttonRect = LowlitClipButton::Detail::GetButtonRectangle(
        mButtonId, { *mClip, ViewInfo::Get(*pProject), event.rect });
    if (!buttonRect.has_value()) {
        return cancelCode;
    }

    const auto mousePos = event.event.GetPosition();
    if (!buttonRect->Contains(mousePos)) {
        return cancelCode;
    }

    return 0;
}

UIHandle::Result HighlitClipButtonHandle::Release(
    const TrackPanelMouseEvent& event, AudacityProject* pProject,
    wxWindow* pParent)
{
    mChangeHighlight = RefreshCode::RefreshNone;
    // Select before calling `DoRelease`.
    const auto result = UpdateTrackSelection(event, pProject);
    return result | DoRelease(event, pProject, pParent);
}

UIHandle::Result HighlitClipButtonHandle::Cancel(AudacityProject* pProject)
{
    return 0;
}

UIHandle::Result HighlitClipButtonHandle::UpdateTrackSelection(
    const TrackPanelMouseEvent& event, AudacityProject* pProject)
{
    auto& trackList = TrackList::Get(*pProject);

    if (const auto track = trackList.Lock<Track>(mTrack)) {
        auto& selectionState = SelectionState::Get(*pProject);
        selectionState.SelectNone(trackList);
        selectionState.SelectTrack(*track, true, true);

        auto& viewInfo = ViewInfo::Get(*pProject);
        viewInfo.selectedRegion.setTimes(
            mClip->GetPlayStartTime(), mClip->GetPlayEndTime());

        ProjectHistory::Get(*pProject).ModifyState(false);

        return RefreshCode::RefreshAll | RefreshCode::Cancelled;
    }

    return RefreshCode::RefreshNone;
}

std::shared_ptr<const Track> HighlitClipButtonHandle::FindTrack() const
{
    return mTrack;
}

void HighlitClipButtonHandle::Enter(bool forward, AudacityProject* pProject)
{
    mChangeHighlight = RefreshCode::RefreshCell;
}

void HighlitClipButtonHandle::Highlight(const wxRect& rect, wxDC& dc)
{
    constexpr auto penWidth = 0;
    const auto col = theTheme.Colour(clrClipAffordanceButton);
    wxDCBrushChanger brush { dc, col };
    wxDCPenChanger pen { dc, { wxPenInfo { col, penWidth } } };
    constexpr auto radius = 3;
    dc.DrawRoundedRectangle(rect, radius);
}
