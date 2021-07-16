/*!********************************************************************
*
 Audacity: A Digital Audio Editor

 AffordanceHandle.cpp

 Vitaly Sverchinsky

 **********************************************************************/

#include "AffordanceHandle.h"

#include "../../HitTestResult.h"
#include "../../ProjectAudioIO.h"
#include "../../RefreshCode.h"
#include "../../ViewInfo.h"
#include "../../SelectionState.h"
#include "../../ProjectSettings.h"
#include "../../TrackPanelMouseEvent.h"
#include "../../WaveClip.h"
#include "../../ProjectHistory.h"
#include "../../Track.h"
#include "../../WaveTrack.h"
#include "../../../images/Cursors.h"

UIHandlePtr AffordanceHandle::HitAnywhere(std::weak_ptr<AffordanceHandle>& holder, const std::shared_ptr<Track>& pTrack)
{
    auto result = std::make_shared<AffordanceHandle>(pTrack);
    result = AssignUIHandlePtr(holder, result);
    return result;
}

HitTestPreview AffordanceHandle::HitPreview(const AudacityProject*, bool unsafe, bool moving)
{
    static auto disabledCursor =
        MakeCursor(wxCURSOR_NO_ENTRY, DisabledCursorXpm, 16, 16);
    static auto handOpenCursor =
        MakeCursor(wxCURSOR_HAND, RearrangeCursorXpm, 16, 16);
    static auto handClosedCursor =
        MakeCursor(wxCURSOR_HAND, RearrangingCursorXpm, 16, 16);
    // i18n-hint Appears on hovering mouse over clip affordance
    auto message = XO("Click and drag to move a clip in time, or click to select");

    if (unsafe)
        return { message, &*disabledCursor };
    return {
        message,
        (moving
        ? &*handClosedCursor
        : &*handOpenCursor)
    };
}

HitTestPreview AffordanceHandle::Preview(const TrackPanelMouseState& mouseState, AudacityProject* pProject)
{
    const bool unsafe = ProjectAudioIO::Get(*pProject).IsAudioActive();
    return HitPreview(pProject, unsafe, Clicked());
}

AffordanceHandle::AffordanceHandle(const std::shared_ptr<Track>& track)
    : TimeShiftHandle(track, false)
{
    SetChangeHighlight(RefreshCode::RefreshCell | RefreshCode::RefreshLatestCell);
}

UIHandle::Result AffordanceHandle::Click(const TrackPanelMouseEvent& evt, AudacityProject* pProject)
{
    auto result = TimeShiftHandle::Click(evt, pProject);
    return result | RefreshCode::RefreshCell;
}

UIHandle::Result AffordanceHandle::Release(const TrackPanelMouseEvent& event, AudacityProject* pProject, wxWindow* pParent)
{
    auto result = TimeShiftHandle::Release(event, pProject, pParent);
    //Clip was not moved
    if (!TimeShiftHandle::WasMoved())
    {
        //almost the same behaviour as provided by SelectHandle
        auto& viewInfo = ViewInfo::Get(*pProject);
        const auto& settings = ProjectSettings::Get(*pProject);

        const auto sTrack = TrackList::Get(*pProject).Lock<Track>(GetTrack());
        const auto pTrack = sTrack.get();

        auto& selectionState = SelectionState::Get(*pProject);

        auto& trackList = TrackList::Get(*pProject);

        // Deselect all other tracks and select this one.
        selectionState.SelectNone(trackList);
        selectionState.SelectTrack(*pTrack, true, true);

        pTrack->TypeSwitch(
            [&](WaveTrack* wt)
            {
                auto time = viewInfo.PositionToTime(event.event.m_x, event.rect.x);
                WaveClip* const selectedClip = wt->GetClipAtTime(time);
                if (selectedClip) {
                    viewInfo.selectedRegion.setTimes(
                        selectedClip->GetOffset(), selectedClip->GetEndTime());
                }
            },
            [&](Track* track)
            {
                // Default behavior: select whole track
                SelectionState::SelectTrackLength(viewInfo, *track, settings.IsSyncLocked());
            }
        );

        ProjectHistory::Get(*pProject).ModifyState(false);

        // Do not start a drag
        result |= RefreshCode::RefreshAll | RefreshCode::Cancelled;
    }
    return result;
}
