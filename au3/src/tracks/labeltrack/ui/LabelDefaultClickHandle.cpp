/**********************************************************************

Audacity: A Digital Audio Editor

LabelDefaultClickHandle.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "LabelDefaultClickHandle.h"

#include "LabelTrackView.h"
#include "../../../HitTestResult.h"
#include "LabelTrack.h"
#include "../../../RefreshCode.h"
#include "../../../TrackPanelMouseEvent.h"
#include <wx/event.h>

LabelDefaultClickHandle::LabelDefaultClickHandle()
{
}

LabelDefaultClickHandle::~LabelDefaultClickHandle()
{
}

struct LabelDefaultClickHandle::LabelState {
    std::vector<
        std::pair< std::weak_ptr<LabelTrack>, LabelTrackView::Flags >
        > mPairs;
};

void LabelDefaultClickHandle::SaveState(AudacityProject* pProject)
{
    mLabelState = std::make_shared<LabelState>();
    auto& pairs = mLabelState->mPairs;
    auto& tracks = TrackList::Get(*pProject);

    for (auto lt : tracks.Any<LabelTrack>()) {
        auto& view = LabelTrackView::Get(*lt);
        pairs.push_back(std::make_pair(
                            lt->SharedPointer<LabelTrack>(), view.SaveFlags()));
    }
}

void LabelDefaultClickHandle::RestoreState(AudacityProject* pProject)
{
    if (mLabelState) {
        for ( const auto& pair : mLabelState->mPairs ) {
            if (auto pLt = TrackList::Get(*pProject).Lock(pair.first)) {
                auto& view = LabelTrackView::Get(*pLt);
                view.RestoreFlags(pair.second);
            }
        }
        mLabelState.reset();
    }
}

UIHandle::Result LabelDefaultClickHandle::Click
    (const TrackPanelMouseEvent& evt, AudacityProject* pProject)
{
    using namespace RefreshCode;
    // Redraw to show the change of text box selection status
    UIHandle::Result result = RefreshAll;

    if (evt.event.LeftDown()) {
        SaveState(pProject);

        const auto pLT = evt.pCell.get();
        for (auto lt : TrackList::Get(*pProject).Any<LabelTrack>()) {
            if (pLT != &ChannelView::Get(*lt)) {
                auto& view = LabelTrackView::Get(*lt);
                view.ResetFlags();
            }
        }
    }

    return result;
}

UIHandle::Result LabelDefaultClickHandle::Drag
    (const TrackPanelMouseEvent& WXUNUSED(evt), AudacityProject* WXUNUSED(pProject))
{
    return RefreshCode::RefreshNone;
}

UIHandle::Result LabelDefaultClickHandle::Release
    (const TrackPanelMouseEvent& WXUNUSED(evt), AudacityProject* WXUNUSED(pProject),
    wxWindow* WXUNUSED(pParent))
{
    mLabelState.reset();
    return RefreshCode::RefreshNone;
}

UIHandle::Result LabelDefaultClickHandle::Cancel(AudacityProject* pProject)
{
    UIHandle::Result result = RefreshCode::RefreshNone;
    RestoreState(pProject);
    return result;
}
