/**********************************************************************

Audacity: A Digital Audio Editor

TrackSelectHandle.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "TrackSelectHandle.h"

#include "ChannelView.h"
#include "../../HitTestResult.h"
#include "Project.h"
#include "ProjectAudioIO.h"
#include "ProjectHistory.h"
#include "../../RefreshCode.h"
#include "../../SelectUtilities.h"
#include "../../TrackPanelMouseEvent.h"
#include "Track.h"

#include <wx/cursor.h>
#include <wx/translation.h>

#include "../../../images/Cursors.h"

#if defined(__WXMAC__)
/* i18n-hint: Command names a modifier key on Macintosh keyboards */
#define CTRL_CLICK XO("Command+Click")
#else
/* i18n-hint: Ctrl names a modifier key on Windows or Linux keyboards */
#define CTRL_CLICK XO("Ctrl+Click")
#endif

namespace {
TranslatableString Message(unsigned trackCount)
{
    if (trackCount > 1) {
        return XO(
// i18n-hint: %s is replaced by (translation of) 'Ctrl+Click' on windows, 'Command+Click' on Mac
            "%s to select or deselect track. Drag up or down to change track order.")
               .Format(CTRL_CLICK);
    } else {
        // i18n-hint: %s is replaced by (translation of) 'Ctrl+Click' on windows, 'Command+Click' on Mac
        return XO("%s to select or deselect track.")
               .Format(CTRL_CLICK);
    }
}
}

TrackSelectHandle::TrackSelectHandle(const std::shared_ptr<Track>& pTrack)
    : mpTrack(pTrack)
{}

UIHandlePtr TrackSelectHandle::HitAnywhere
    (std::weak_ptr<TrackSelectHandle>& holder,
    const std::shared_ptr<Track>& pTrack)
{
    auto result = std::make_shared<TrackSelectHandle>(pTrack);
    result = AssignUIHandlePtr(holder, result);
    return result;
}

TrackSelectHandle::~TrackSelectHandle()
{
}

std::shared_ptr<const Track> TrackSelectHandle::FindTrack() const
{
    return mpTrack;
}

UIHandle::Result TrackSelectHandle::Click
    (const TrackPanelMouseEvent& evt, AudacityProject* pProject)
{
    // If unsafe to drag, still, it does harmlessly change the selected track
    // set on button down.

    using namespace RefreshCode;
    Result result = RefreshNone;

    const wxMouseEvent& event = evt.event;

    // AS: If not a click, ignore the mouse event.
    if (!event.ButtonDown() && !event.ButtonDClick()) {
        return Cancelled;
    }
    if (!event.Button(wxMOUSE_BTN_LEFT)) {
        return Cancelled;
    }

    const auto pTrack = mpTrack;
    if (!pTrack) {
        return Cancelled;
    }
    const bool unsafe = ProjectAudioIO::Get(*pProject).IsAudioActive();

    // DM: If they weren't clicking on a particular part of a track label,
    //  deselect other tracks and select this one.

    // JH: also, capture the current track for rearranging, so the user
    //  can drag the track up or down to swap it with others
    if (unsafe) {
        result |= Cancelled;
    } else {
        mRearrangeCount = 0;
        CalculateRearrangingThresholds(event, pProject);
    }

    SelectUtilities::DoListSelection(*pProject,
                                     *pTrack, event.ShiftDown(), event.ControlDown(), !unsafe);

    mClicked = true;
    return result;
}

UIHandle::Result TrackSelectHandle::Drag
    (const TrackPanelMouseEvent& evt, AudacityProject* pProject)
{
    using namespace RefreshCode;
    Result result = RefreshNone;
    if (!mpTrack) {
        return result;
    }

    const wxMouseEvent& event = evt.event;

    auto& tracks = TrackList::Get(*pProject);

    // probably harmless during play?  However, we do disallow the click, so check this too.
    bool unsafe = ProjectAudioIO::Get(*pProject).IsAudioActive();
    if (unsafe) {
        return result;
    }

    if (event.m_y < mMoveUpThreshold || event.m_y < 0) {
        tracks.MoveUp(*mpTrack);
        --mRearrangeCount;
    } else if (event.m_y > mMoveDownThreshold
               || event.m_y > evt.whole.GetHeight()) {
        tracks.MoveDown(*mpTrack);
        ++mRearrangeCount;
    } else {
        return result;
    }

    // JH: if we moved up or down, recalculate the thresholds and make sure the
    // track is fully on-screen.
    CalculateRearrangingThresholds(event, pProject);

    result |= EnsureVisible | RefreshAll;
    return result;
}

HitTestPreview TrackSelectHandle::Preview
    (const TrackPanelMouseState&, AudacityProject* project)
{
    static auto disabledCursor
        =::MakeCursor(wxCURSOR_NO_ENTRY, DisabledCursorXpm, 16, 16);
    //static wxCursor rearrangeCursor{ wxCURSOR_HAND };
    static auto rearrangingCursor
        =::MakeCursor(wxCURSOR_HAND, RearrangingCursorXpm, 16, 16);
    static wxCursor arrowCursor{ wxCURSOR_ARROW };

    //static auto hoverCursor =
    //   ::MakeCursor(wxCURSOR_HAND, RearrangeCursorXpm, 16, 16);
    //static auto clickedCursor =
    //   ::MakeCursor(wxCURSOR_HAND, RearrangingCursorXpm, 16, 16);

    const auto trackCount = TrackList::Get(*project).Any().size();
    auto message = Message(trackCount);
    if (mClicked) {
        const bool unsafe
            =ProjectAudioIO::Get(*project).IsAudioActive();
        const bool canMove = TrackList::Get(*project).Any().size() > 1;
        return {
            message,
            (unsafe
             ? &*disabledCursor
             : canMove
             ? &*rearrangingCursor
             : &arrowCursor)
            // , message // Stop showing the tooltip after the click
        };
    } else {
        // Only mouse-over
        // Don't test safety, because the click to change selection is allowed
        return {
            message,
            &arrowCursor,
            message
        };
    }
}

UIHandle::Result TrackSelectHandle::Release
    (const TrackPanelMouseEvent&, AudacityProject* project, wxWindow*)
{
    // If we're releasing, surely we are dragging a track?
    wxASSERT(mpTrack);
    if (mRearrangeCount != 0) {
        ProjectHistory::Get(*project).PushState(
            /* i18n-hint: will substitute name of track for %s */
            (mRearrangeCount < 0 ? XO("Moved '%s' up") : XO("Moved '%s' down"))
            .Format(mpTrack->GetName()),
            XO("Move Track"));
    }
    // Bug 1677
    // Holding on to the reference to the track was causing it to be released far later
    // than necessary, on shutdown, and so causing a crash as a dialog about cleaning
    // out files could not show at that time.
    mpTrack.reset();
    // No need to redraw, that was done when drag moved the track
    return RefreshCode::RefreshNone;
}

UIHandle::Result TrackSelectHandle::Cancel(AudacityProject* pProject)
{
    ProjectHistory::Get(*pProject).RollbackState();
    // Bug 1677
    mpTrack.reset();
    return RefreshCode::RefreshAll;
}

/// Figure out how far the user must drag the mouse up or down
/// before the track will swap with the one above or below
void TrackSelectHandle::CalculateRearrangingThresholds(
    const wxMouseEvent& event, AudacityProject* project)
{
    // JH: this will probably need to be tweaked a bit, I'm just
    //   not sure what formula will have the best feel for the
    //   user.

    auto& tracks = TrackList::Get(*project);

    if (mpTrack && tracks.CanMoveUp(*mpTrack)) {
        mMoveUpThreshold
            =event.m_y
              - ChannelView::GetChannelGroupHeight(
                  *--tracks.Find(mpTrack.get()));
    } else {
        mMoveUpThreshold = INT_MIN;
    }

    if (mpTrack && tracks.CanMoveDown(*mpTrack)) {
        mMoveDownThreshold
            =event.m_y
              + ChannelView::GetChannelGroupHeight(
                  *++tracks.Find(mpTrack.get()));
    } else {
        mMoveDownThreshold = INT_MAX;
    }
}
