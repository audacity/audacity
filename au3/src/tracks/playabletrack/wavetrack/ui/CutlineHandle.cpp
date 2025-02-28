/**********************************************************************

Audacity: A Digital Audio Editor

CutlineHandle.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "CutlineHandle.h"

#include <wx/cursor.h>
#include <wx/event.h>

#include "../../../../HitTestResult.h"
#include "ProjectAudioIO.h"
#include "ProjectHistory.h"
#include "../../../../RefreshCode.h"
#include "Snap.h" // for kPixelTolerance
#include "../../../../TrackPanelMouseEvent.h"
#include "UndoManager.h"
#include "ViewInfo.h"
#include "WaveTrack.h"
#include "WaveTrackUtilities.h"
#include "../../../../../images/Cursors.h"

CutlineHandle::CutlineHandle(
    const std::shared_ptr<WaveTrack>& pTrack,
    WaveTrackLocations locations, WaveTrackLocation location)
    : mpTrack{pTrack}
    , mLocations{move(locations)}
    , mLocation{location}
{
}

void CutlineHandle::Enter(bool, AudacityProject*)
{
}

HitTestPreview CutlineHandle::HitPreview(bool unsafe)
{
    static auto disabledCursor
        =::MakeCursor(wxCURSOR_NO_ENTRY, DisabledCursorXpm, 16, 16);
    static wxCursor arrowCursor{ wxCURSOR_ARROW };
    return { XO("Left-Click to expand, Right-Click to remove"),
             (unsafe ? &*disabledCursor : &arrowCursor) };
}

namespace {
bool IsOverCutline(const WaveTrackLocations& locations,
                   const ViewInfo& viewInfo,
                   const wxRect& rect, const wxMouseState& state,
                   WaveTrackLocation* pmLocation)
{
    for (auto loc: locations) {
        const double x = viewInfo.TimeToPosition(loc.pos);
        if (x >= 0 && x < rect.width) {
            wxRect locRect;
            locRect.width = 2 * kPixelTolerance - 1;
            locRect.x = (int)(rect.x + x) - locRect.width / 2;
            locRect.y = rect.y;
            locRect.height = rect.height;
            if (locRect.Contains(state.m_x, state.m_y)) {
                if (pmLocation) {
                    *pmLocation = loc;
                }
                return true;
            }
        }
    }

    return false;
}
}

UIHandlePtr CutlineHandle::HitTest(
    std::weak_ptr<CutlineHandle>& holder,
    const wxMouseState& state, const wxRect& rect,
    const AudacityProject* pProject,
    std::shared_ptr<WaveTrack> pTrack)
{
    auto& viewInfo = ViewInfo::Get(*pProject);
    /// method that tells us if the mouse event landed on an
    /// editable Cutline

    auto locations = FindWaveTrackLocations(*pTrack);
    WaveTrackLocation location;
    if (!IsOverCutline(locations, viewInfo, rect, state, &location)) {
        return {}
    }

    auto result
        =std::make_shared<CutlineHandle>(pTrack, move(locations), location);
    result = AssignUIHandlePtr(holder, result);
    return result;
}

CutlineHandle::~CutlineHandle()
{
}

std::shared_ptr<const Track> CutlineHandle::FindTrack() const
{
    return mpTrack;
}

bool CutlineHandle::HandlesRightClick()
{
    return true;
}

UIHandle::Result CutlineHandle::Click
    (const TrackPanelMouseEvent& evt, AudacityProject* pProject)
{
    using namespace RefreshCode;
    const bool unsafe = ProjectAudioIO::Get(*pProject).IsAudioActive();
    if (unsafe) {
        return Cancelled;
    }

    const wxMouseEvent& event = evt.event;
    auto& viewInfo = ViewInfo::Get(*pProject);

    // Can affect the track by merging clips, expanding a cutline, or
    // deleting a cutline.
    // All the change is done at button-down.  Button-up just commits the undo item.

    /// Someone has just clicked the mouse.  What do we do?

    // FIXME: Disable this and return true when CutLines aren't showing?
    // (Don't use gPrefs-> for the fix as registry access is slow).

    // Cutline data changed on either branch, so refresh the track display.
    UIHandle::Result result = RefreshCell;

    if (event.LeftDown()) {
        mOperation = Expand;
        mStartTime = viewInfo.selectedRegion.t0();
        mEndTime = viewInfo.selectedRegion.t1();

        // When user presses left button on cut line, expand the line again
        double cutlineStart = 0, cutlineEnd = 0;
        WaveTrackUtilities::ExpandCutLine(*mpTrack,
                                          mLocation.pos, &cutlineStart, &cutlineEnd);
        viewInfo.selectedRegion.setTimes(cutlineStart, cutlineEnd);
    } else if (event.RightDown()) {
        bool removed = WaveTrackUtilities::RemoveCutLine(*mpTrack, mLocation.pos);
        if (!removed) {
            // Nothing happened, make no Undo item
            return Cancelled;
        }
        mOperation = Remove;
    } else {
        result = RefreshNone;
    }

    return result;
}

UIHandle::Result CutlineHandle::Drag
    (const TrackPanelMouseEvent&, AudacityProject*)
{
    return RefreshCode::RefreshNone;
}

HitTestPreview CutlineHandle::Preview
    (const TrackPanelMouseState&, AudacityProject* pProject)
{
    const bool unsafe = ProjectAudioIO::Get(*pProject).IsAudioActive();
    return HitPreview(unsafe);
}

UIHandle::Result CutlineHandle::Release
    (const TrackPanelMouseEvent&, AudacityProject* pProject, wxWindow*)
{
    UIHandle::Result result = RefreshCode::RefreshNone;

    // Only now commit the result to the undo stack
    switch (mOperation) {
    default:
        wxASSERT(false);
    case Expand:
        ProjectHistory::Get(*pProject)
        .PushState(XO("Expanded Cut Line"), XO("Expand"));
        break;
    case Remove:
        ProjectHistory::Get(*pProject)
        .PushState(XO("Removed Cut Line"), XO("Remove"));
        break;
    }

    // Nothing to do for the display
    return result;
}

UIHandle::Result CutlineHandle::Cancel(AudacityProject* pProject)
{
    using namespace RefreshCode;
    UIHandle::Result result = RefreshCell;
    ProjectHistory::Get(*pProject).RollbackState();
    if (mOperation == Expand) {
        AudacityProject& project = *pProject;
        auto& selectedRegion = ViewInfo::Get(project).selectedRegion;
        selectedRegion.setTimes(mStartTime, mEndTime);
    }
    return result;
}
