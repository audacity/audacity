/*!********************************************************************
*
 Audacity: A Digital Audio Editor

 NoteTrackAffordanceControls.cpp

 Vitaly Sverchinsky

 **********************************************************************/

#include "NoteTrackAffordanceControls.h"

#include <wx/dc.h>

#include "../../../ui/AffordanceHandle.h"
#include "../../../ui/SelectHandle.h"
#include "../../../ui/TrackView.h"
#include "../../../../AllThemeResources.h"
#include "../../../../AColor.h"
#include "../../../../NoteTrack.h"
#include "ViewInfo.h"
#include "../../../../TrackArtist.h"
#include "../../../../TrackPanelMouseEvent.h"
#include "../../../../TrackPanelDrawingContext.h"

#include "../lib-src/header-substitutes/allegro.h"


#include "../../../../ProjectHistory.h"
#include "../../../../SelectionState.h"
#include "../../../../ProjectSettings.h"
#include "../../../../RefreshCode.h"
#include "../../../../Theme.h"

class NoteTrackAffordanceHandle final : public AffordanceHandle
{
public:
    NoteTrackAffordanceHandle(const std::shared_ptr<Track>& track) : AffordanceHandle(track) { }

    static UIHandlePtr HitAnywhere(std::weak_ptr<NoteTrackAffordanceHandle>& holder, const std::shared_ptr<Track>& pTrack)
    {
        return AssignUIHandlePtr(holder, std::make_shared<NoteTrackAffordanceHandle>(pTrack));
    }

    UIHandle::Result SelectAt(const TrackPanelMouseEvent& event, AudacityProject* pProject) override
    {
        auto& viewInfo = ViewInfo::Get(*pProject);
        const auto& settings = ProjectSettings::Get(*pProject);
        const auto track = TrackList::Get(*pProject).Lock<Track>(GetTrack());

        SelectionState::SelectTrackLength(viewInfo, *track, settings.IsSyncLocked());

        ProjectHistory::Get(*pProject).ModifyState(false);

        return RefreshCode::RefreshAll | RefreshCode::Cancelled;
    }
};

NoteTrackAffordanceControls::NoteTrackAffordanceControls(const std::shared_ptr<Track>& pTrack)
    : CommonTrackCell(pTrack)
{

}

std::vector<UIHandlePtr> NoteTrackAffordanceControls::HitTest(const TrackPanelMouseState& state, const AudacityProject* pProject)
{
    std::vector<UIHandlePtr> results;

    auto track = FindTrack();
    const auto nt = std::static_pointer_cast<const NoteTrack>(track->SubstitutePendingChangedTrack());

    const auto rect = state.rect;

    auto& zoomInfo = ViewInfo::Get(*pProject);
    auto left = zoomInfo.TimeToPosition(nt->GetOffset(), rect.x);
    auto right = zoomInfo.TimeToPosition(nt->GetOffset() + nt->GetSeq().get_real_dur(), rect.x);
    auto headerRect = wxRect(left, rect.y, right - left, rect.height);

    auto px = state.state.m_x;
    auto py = state.state.m_y;

    if (px >= headerRect.GetLeft() && px <= headerRect.GetRight() &&
        py >= headerRect.GetTop() && py <= headerRect.GetBottom())
    {
        results.push_back(NoteTrackAffordanceHandle::HitAnywhere(mAffordanceHandle, track));
    }

    const auto& settings = ProjectSettings::Get(*pProject);
    const auto currentTool = settings.GetTool();
    if (currentTool == ToolCodes::multiTool || currentTool == ToolCodes::selectTool)
    {
        results.push_back(
            SelectHandle::HitTest(
                mSelectHandle, state, pProject, std::static_pointer_cast<TrackView>(track->GetTrackView())
            )
        );
    }

    return results;
}

void NoteTrackAffordanceControls::Draw(TrackPanelDrawingContext& context, const wxRect& rect, unsigned iPass)
{
    if (iPass == TrackArtist::PassBackground) {
        const auto nt = std::static_pointer_cast<const NoteTrack>(FindTrack()->SubstitutePendingChangedTrack());
        const auto artist = TrackArtist::Get(context);

        TrackArt::DrawBackgroundWithSelection(context, rect, nt.get(), AColor::labelSelectedBrush, AColor::labelUnselectedBrush);

        const auto& zoomInfo = *artist->pZoomInfo;
        auto left = zoomInfo.TimeToPosition(nt->GetOffset(), rect.x);
        auto right = zoomInfo.TimeToPosition(nt->GetOffset() + nt->GetSeq().get_real_dur(), rect.x);
        auto clipRect = wxRect(left, rect.y, right - left + 1, rect.height);

        auto px = context.lastState.m_x;
        auto py = context.lastState.m_y;

        auto selected = IsSelected();
        auto highlight = selected ||
            (px >= clipRect.GetLeft() && px <= clipRect.GetRight() &&
                py >= clipRect.GetTop() && py <= clipRect.GetBottom());

        {
            wxDCClipper clipper(context.dc, rect);
            context.dc.SetTextBackground(wxTransparentColor);
            context.dc.SetTextForeground(theTheme.Colour(clrClipNameText));
            context.dc.SetFont(wxFont(wxFontInfo()));
            TrackArt::DrawClipAffordance(context.dc, clipRect, nt->GetName(), highlight, selected);
        }
    }
}

bool NoteTrackAffordanceControls::IsSelected() const
{
    if (auto handle = mAffordanceHandle.lock())
    {
        return handle->Clicked();
    }
    return false;
}
