/*!********************************************************************
*
 Audacity: A Digital Audio Editor

 AffordanceHandle.cpp

 Vitaly Sverchinsky

 **********************************************************************/

#include "AffordanceHandle.h"

#include "../../HitTestResult.h"
#include "ProjectAudioIO.h"
#include "../../RefreshCode.h"
#include "ViewInfo.h"
#include "SelectionState.h"
#include "../../TrackPanelMouseEvent.h"
#include "Track.h"
#include "../../../images/Cursors.h"

#include <wx/cursor.h>
#include <wx/event.h>

HitTestPreview AffordanceHandle::HitPreview(const AudacityProject*, bool unsafe, bool moving)
{
    static wxCursor arrowCursor{ wxCURSOR_ARROW };
    static auto handOpenCursor
        =MakeCursor(wxCURSOR_HAND, RearrangeCursorXpm, 16, 16);
    static auto handClosedCursor
        =MakeCursor(wxCURSOR_HAND, RearrangingCursorXpm, 16, 16);
    // i18n-hint Appears on hovering mouse over clip affordance
    auto message = XO("Drag clips to reposition them." \
                      " Hold Shift and drag to move all clips on the same track.");

    if (unsafe) {
        return { message, &arrowCursor }
    }
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
    mClickPosition = evt.event.GetPosition();
    return result | RefreshCode::RefreshCell;
}

UIHandle::Result AffordanceHandle::Drag(const TrackPanelMouseEvent& event, AudacityProject* pProject)
{
    if (!mMoving) {
        if (std::abs(mClickPosition.x - event.event.m_x) >= MoveThreshold
            || std::abs(mClickPosition.y - event.event.m_y) >= MoveThreshold) {
            mMoving = true;
        } else {
            return RefreshCode::RefreshNone;
        }
    }
    return TimeShiftHandle::Drag(event, pProject);
}

UIHandle::Result AffordanceHandle::Release(const TrackPanelMouseEvent& event, AudacityProject* pProject, wxWindow* pParent)
{
    auto result = TimeShiftHandle::Release(event, pProject, pParent);
    if (!WasMoved()) {
        result |= UpdateTrackSelection(event, pProject);
    }
    return result;
}

UIHandle::Result AffordanceHandle::UpdateTrackSelection(const TrackPanelMouseEvent& event, AudacityProject* pProject)
{
    auto& trackList = TrackList::Get(*pProject);

    if (const auto track = trackList.Lock<Track>(GetTrack())) {
        auto& selectionState = SelectionState::Get(*pProject);
        selectionState.SelectNone(trackList);
        selectionState.SelectTrack(*track, true, true);

        return SelectAt(event, pProject);
    }

    return RefreshCode::RefreshNone;
}
