/**********************************************************************

Audacity: A Digital Audio Editor

NoteTrackButtonHandle.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifdef USE_MIDI
#include "NoteTrackButtonHandle.h"

#include "../../../../HitTestResult.h"
#include "NoteTrackControls.h"
#include "../../../../TrackPanelMouseEvent.h"
#include "NoteTrack.h"
#include "ProjectHistory.h"
#include "../../../../RefreshCode.h"
#include "../../../ui/CommonTrackInfo.h"

#include <wx/event.h>

NoteTrackButtonHandle::NoteTrackButtonHandle
    (const std::shared_ptr<NoteTrack>& pTrack,
    int channel, const wxRect& rect)
    : mpTrack{pTrack}
    , mChannel{channel}
    , mRect{rect}
{
}

void NoteTrackButtonHandle::Enter(bool, AudacityProject*)
{
    mChangeHighlight = RefreshCode::RefreshCell;
}

NoteTrackButtonHandle::~NoteTrackButtonHandle()
{
}

std::shared_ptr<const Track> NoteTrackButtonHandle::FindTrack() const
{
    return mpTrack.lock();
}

UIHandle::Result NoteTrackButtonHandle::NeedChangeHighlight
    (const NoteTrackButtonHandle& oldState, const NoteTrackButtonHandle& newState)
{
    if (oldState.GetChannel() != newState.GetChannel()) {
        // Repaint whenever the highlighted button is different
        return RefreshCode::RefreshCell;
    }
    return 0;
}

static int FindChannelNumber(const wxRect& rect, int mx, int my)
{
    wxASSERT_MSG(rect.width % 4 == 0, "Midi channel control rect width must be divisible by 4");
    wxASSERT_MSG(rect.height % 4 == 0, "Midi channel control rect height must be divisible by 4");

    auto cellWidth = rect.width / 4;
    auto cellHeight = rect.height / 4;

    int col = (mx - rect.x) / cellWidth;
    int row = (my - rect.y) / cellHeight;

    return row * 4 + col;
}

// Handles clicking within the midi controls rect (same as DrawLabelControls).
// This is somewhat oddly written, as these aren't real buttons - they act
// when the mouse goes down; you can't hold it pressed and move off of it.
// Left-clicking toggles a single channel; right-clicking turns off all other channels.
static bool LabelClick(
    NoteTrack& track, const wxRect& rect, int mx, int my, bool right)
{
    auto channel = FindChannelNumber(rect, mx, my);
    if (right) {
        track.SoloVisibleChan(channel);
    } else {
        track.ToggleVisibleChan(channel);
    }

    return true;
}

UIHandlePtr NoteTrackButtonHandle::HitTest
    (std::weak_ptr<NoteTrackButtonHandle>& holder,
    const wxMouseState& state, const wxRect& rect,
    const std::shared_ptr<NoteTrack>& pTrack)
{
    wxRect midiRect;
    NoteTrackControls::GetMidiControlsRect(rect, midiRect);
    if (CommonTrackInfo::HideTopItem(rect, midiRect)) {
        return {}
    }
    if (midiRect.Contains(state.m_x, state.m_y)) {
        auto channel = FindChannelNumber(midiRect, state.m_x, state.m_y);
        auto result = std::make_shared<NoteTrackButtonHandle>(
            pTrack, channel, midiRect);
        result = AssignUIHandlePtr(holder, result);
        return result;
    } else {
        return {}
    }
}

UIHandle::Result NoteTrackButtonHandle::Click
    (const TrackPanelMouseEvent&, AudacityProject*)
{
    return RefreshCode::RefreshNone;
}

UIHandle::Result NoteTrackButtonHandle::Drag
    (const TrackPanelMouseEvent&, AudacityProject*)
{
    return RefreshCode::RefreshNone;
}

HitTestPreview NoteTrackButtonHandle::Preview
    (const TrackPanelMouseState&, AudacityProject*)
{
    // auto pTrack = pProject->GetTracks()->Lock(mpTrack);
    auto pTrack = mpTrack.lock();
    if (!pTrack) {
        return {}
    }
    // No special message or cursor
    return {};
}

UIHandle::Result NoteTrackButtonHandle::Release
    (const TrackPanelMouseEvent& evt, AudacityProject* pProject, wxWindow*)
{
    using namespace RefreshCode;

    auto pTrack = TrackList::Get(*pProject).Lock(mpTrack);
    if (!pTrack) {
        return Cancelled;
    }

    const wxMouseEvent& event = evt.event;
    if (LabelClick(*pTrack, mRect, event.m_x, event.m_y,
                   event.Button(wxMOUSE_BTN_RIGHT))) {
        // No undo items needed??
        ProjectHistory::Get(*pProject).ModifyState(false);
        return RefreshAll;
    }
    return RefreshNone;
}

UIHandle::Result NoteTrackButtonHandle::Cancel(AudacityProject*)
{
    return RefreshCode::RefreshNone;
}

#endif
