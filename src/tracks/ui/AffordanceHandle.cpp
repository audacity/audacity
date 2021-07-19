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
#include "../../TrackPanelMouseEvent.h"
#include "../../WaveClip.h"
#include "../../Track.h"
#include "../../WaveTrack.h"
#include "../../../images/Cursors.h"

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

void AffordanceHandle::Enter(bool forward, AudacityProject* pProject)
{
    SetChangeHighlight(RefreshCode::RefreshCell | RefreshCode::RefreshLatestCell);
    TimeShiftHandle::Enter(forward, pProject);
}

HitTestPreview AffordanceHandle::Preview(const TrackPanelMouseState& mouseState, AudacityProject* pProject)
{
    const bool unsafe = ProjectAudioIO::Get(*pProject).IsAudioActive();
    return HitPreview(pProject, unsafe, Clicked());
}

AffordanceHandle::AffordanceHandle(const std::shared_ptr<Track>& track)
    : TimeShiftHandle(track, false)
{
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
        const auto track = TrackList::Get(*pProject).Lock<Track>(GetTrack());

        auto& selectionState = SelectionState::Get(*pProject);
        auto& trackList = TrackList::Get(*pProject);
        selectionState.SelectNone(trackList);
        selectionState.SelectTrack(*track, true, true);

        result |= SelectAt(event, pProject);
    }
    return result;
}
