/**********************************************************************

Audacity: A Digital Audio Editor

StretchHandle.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifdef USE_MIDI
#include "WrapAllegro.h"

#include "StretchHandle.h"

#include "../../../ui/CommonTrackPanelCell.h"
#include "../../../../HitTestResult.h"
#include "NoteTrack.h"
#include "ProjectAudioIO.h"
#include "ProjectHistory.h"
#include "../../../../RefreshCode.h"
#include "SyncLock.h"
#include "../../../../TrackPanelMouseEvent.h"
#include "UndoManager.h"
#include "ViewInfo.h"
#include "../../../../../images/Cursors.h"

#include <wx/event.h>
#include <algorithm>

StretchHandle::StretchHandle
    (const std::shared_ptr<NoteTrack>& pTrack, const StretchState& stretchState)
    : mpTrack{pTrack}
    , mStretchState{stretchState}
{}

HitTestPreview StretchHandle::HitPreview(StretchEnum stretchMode, bool unsafe)
{
    static auto disabledCursor
        =::MakeCursor(wxCURSOR_NO_ENTRY, DisabledCursorXpm, 16, 16);
    static auto stretchLeftCursor
        =::MakeCursor(wxCURSOR_BULLSEYE, StretchLeftCursorXpm, 16, 16);
    static auto stretchRightCursor
        =::MakeCursor(wxCURSOR_BULLSEYE, StretchRightCursorXpm, 16, 16);
    static auto stretchCursor
        =::MakeCursor(wxCURSOR_BULLSEYE, StretchCursorXpm, 16, 16);

    if (unsafe) {
        return { {}, &*disabledCursor };
    } else {
        wxCursor* pCursor = NULL;
        switch (stretchMode) {
        default:
            wxASSERT(false);
        case stretchLeft:
            pCursor = &*stretchLeftCursor;
            break;
        case stretchCenter:
            pCursor = &*stretchCursor;
            break;
        case stretchRight:
            pCursor = &*stretchRightCursor;
            break;
        }
        return {
            XO("Click and drag to stretch selected region."),
            pCursor
        };
    }
}

UIHandlePtr StretchHandle::HitTest
    (std::weak_ptr<StretchHandle>& holder,
    const TrackPanelMouseState& st, const AudacityProject* pProject,
    const std::shared_ptr<NoteTrack>& pTrack)
{
    StretchState stretchState;
    const wxMouseState& state = st.state;

    // later, we may want a different policy, but for now, stretch is
    // selected when the cursor is near the center of the track and
    // within the selection
    auto& viewInfo = ViewInfo::Get(*pProject);

    if (!pTrack || !pTrack->GetSelected()) {
        return {}
    }

    const wxRect& rect = st.rect;
    int center = rect.y + rect.height / 2;
    int distance = abs(state.m_y - center);
    const int yTolerance = 10;
    wxInt64 leftSel = viewInfo.TimeToPosition(viewInfo.selectedRegion.t0(), rect.x);
    wxInt64 rightSel = viewInfo.TimeToPosition(viewInfo.selectedRegion.t1(), rect.x);
    // Something is wrong if right edge comes before left edge
    wxASSERT(!(rightSel < leftSel));
    if (!(leftSel <= state.m_x && state.m_x <= rightSel
          && distance < yTolerance)) {
        return {}
    }

    // find nearest beat to sel0, sel1
    static const double minPeriod = 0.05; // minimum beat period
    stretchState.mBeatCenter = { 0, 0 };

    auto t0 = GetT0(*pTrack, viewInfo);
    auto t1 = GetT1(*pTrack, viewInfo);

    if (t0 >= t1) {
        return {}
    }

    stretchState.mBeat0 = pTrack->NearestBeatTime(t0);
    stretchState.mOrigSel0Quantized = stretchState.mBeat0.first;

    stretchState.mBeat1 = pTrack->NearestBeatTime(t1);
    stretchState.mOrigSel1Quantized = stretchState.mBeat1.first;

    // If there is not (almost) a beat to stretch that is slower
    // than 20 beats per second, don't stretch
    if (within(stretchState.mBeat0.second,
               stretchState.mBeat1.second, 0.9)
        || (stretchState.mBeat1.first - stretchState.mBeat0.first)
        / (stretchState.mBeat1.second - stretchState.mBeat0.second)
        < minPeriod) {
        return {}
    }

    auto selStart = viewInfo.PositionToTime(state.m_x, rect.x);
    selStart = std::max(t0, std::min(t1, selStart));
    stretchState.mBeatCenter = pTrack->NearestBeatTime(selStart);
    if (within(stretchState.mBeat0.second,
               stretchState.mBeatCenter.second, 0.1)) {
        stretchState.mMode = stretchLeft;
        stretchState.mLeftBeats = 0;
        stretchState.mRightBeats
            =stretchState.mBeat1.second - stretchState.mBeat0.second;
    } else if (within(stretchState.mBeat1.second,
                      stretchState.mBeatCenter.second, 0.1)) {
        stretchState.mMode = stretchRight;
        stretchState.mLeftBeats
            =stretchState.mBeat1.second - stretchState.mBeat0.second;
        stretchState.mRightBeats = 0;
    } else {
        stretchState.mMode = stretchCenter;
        stretchState.mLeftBeats
            =stretchState.mBeat1.second - stretchState.mBeatCenter.second;
        stretchState.mRightBeats
            =stretchState.mBeatCenter.second - stretchState.mBeat0.second;
    }

    auto result = std::make_shared<StretchHandle>(pTrack, stretchState);
    result = AssignUIHandlePtr(holder, result);
    return result;
}

StretchHandle::~StretchHandle()
{
}

std::shared_ptr<const Track> StretchHandle::FindTrack() const
{
    return mpTrack;
}

UIHandle::Result StretchHandle::Click
    (const TrackPanelMouseEvent& evt, AudacityProject* pProject)
{
    using namespace RefreshCode;
    const bool unsafe = ProjectAudioIO::Get(*pProject).IsAudioActive();
    if (unsafe) {
        return Cancelled;
    }

    const wxMouseEvent& event = evt.event;

    if (event.LeftDClick()
        || !event.LeftDown()
        || evt.pCell == NULL) {
        return Cancelled;
    }

    mLeftEdge = evt.rect.GetLeft();
    auto& viewInfo = ViewInfo::Get(*pProject);

    viewInfo.selectedRegion.setTimes
        (mStretchState.mBeat0.first, mStretchState.mBeat1.first);

    // Full refresh since the label area may need to indicate
    // newly selected tracks. (I'm really not sure if the label area
    // needs to be refreshed or how to just refresh non-label areas.-RBD)

    return RefreshAll;
}

UIHandle::Result StretchHandle::Drag
    (const TrackPanelMouseEvent& evt, AudacityProject* pProject)
{
    using namespace RefreshCode;
    const bool unsafe = ProjectAudioIO::Get(*pProject).IsAudioActive();
    if (unsafe) {
        this->Cancel(pProject);
        return RefreshAll | Cancelled;
    }

    const wxMouseEvent& event = evt.event;
    const int x = event.m_x;

    Channel* clickedChannel = nullptr;
    if (evt.pCell) {
        clickedChannel
            =static_cast<CommonChannelCell*>(evt.pCell.get())->FindChannel().get();
    }

    if (clickedChannel == nullptr && mpTrack != nullptr) {
        clickedChannel = mpTrack.get();
    }
    Stretch(pProject, x, mLeftEdge, clickedChannel);
    return RefreshAll;
}

HitTestPreview StretchHandle::Preview
    (const TrackPanelMouseState&, AudacityProject* pProject)
{
    const bool unsafe = ProjectAudioIO::Get(*pProject).IsAudioActive();
    return HitPreview(mStretchState.mMode, unsafe);
}

UIHandle::Result StretchHandle::Release
    (const TrackPanelMouseEvent&, AudacityProject* pProject,
    wxWindow*)
{
    using namespace RefreshCode;
    if (!mpTrack) {
        return RefreshNone;
    }

    const bool unsafe = ProjectAudioIO::Get(*pProject).IsAudioActive();
    if (unsafe) {
        this->Cancel(pProject);
        return RefreshAll | Cancelled;
    }

    bool left = mStretchState.mMode == stretchLeft;
    bool right = mStretchState.mMode == stretchRight;
    auto& viewInfo = ViewInfo::Get(*pProject);
    if (SyncLockState::Get(*pProject).IsSyncLocked() && (left || right)) {
        for (auto track : SyncLock::Group(*mpTrack)) {
            if (track != mpTrack.get()) {
                if (left) {
                    auto origT0 = mStretchState.mOrigSel0Quantized;
                    auto diff = viewInfo.selectedRegion.t0() - origT0;
                    if (diff > 0) {
                        track->SyncLockAdjust(origT0 + diff, origT0);
                    } else {
                        track->SyncLockAdjust(origT0, origT0 - diff);
                    }
                    track->ShiftBy(diff);
                } else {
                    auto origT1 = mStretchState.mOrigSel1Quantized;
                    auto diff = viewInfo.selectedRegion.t1() - origT1;
                    track->SyncLockAdjust(origT1, origT1 + diff);
                }
            }
        }
    }

    /* i18n-hint: (noun) The track that is used for MIDI notes which can be
    dragged to change their duration.*/
    ProjectHistory::Get(*pProject).PushState(XO("Stretch Note Track"),
                                             /* i18n-hint: In the history list, indicates a MIDI note has
                                             been dragged to change its duration (stretch it). Using either past
                                             or present tense is fine here.  If unsure, go for whichever is
                                             shorter.*/
                                             XO("Stretch"),
                                             UndoPush::CONSOLIDATE);
    return RefreshAll;
}

UIHandle::Result StretchHandle::Cancel(AudacityProject* pProject)
{
    ProjectHistory::Get(*pProject).RollbackState();
    return RefreshCode::RefreshNone;
}

double StretchHandle::GetT0(const Track& track, const ViewInfo& viewInfo)
{
    return std::max(track.GetStartTime(), viewInfo.selectedRegion.t0());
}

double StretchHandle::GetT1(const Track& track, const ViewInfo& viewInfo)
{
    return std::min(track.GetEndTime(), viewInfo.selectedRegion.t1());
}

void StretchHandle::Stretch(AudacityProject* pProject, int mouseXCoordinate, int trackLeftEdge,
                            Channel* pChannel)
{
    auto& viewInfo = ViewInfo::Get(*pProject);

    if (pChannel == nullptr && mpTrack != nullptr) {
        pChannel = mpTrack.get();
    }

    if (const auto pNt = dynamic_cast<NoteTrack*>(pChannel)) {
        auto& nt = *pNt;
        double moveto
            =std::max(0.0, viewInfo.PositionToTime(mouseXCoordinate, trackLeftEdge));

        double dur, left_dur, right_dur;

        // check to make sure tempo is not higher than 20 beats per second
        // (In principle, tempo can be higher, but not infinity.)
        double minPeriod = 0.05; // minimum beat period

        // make sure target duration is not too short
        // Take quick exit if so, without changing the selection.
        auto t0 = mStretchState.mBeat0.first;
        auto t1 = mStretchState.mBeat1.first;
        switch (mStretchState.mMode) {
        case stretchLeft: {
            dur = t1 - moveto;
            if (dur < mStretchState.mRightBeats * minPeriod) {
                return;
            }
            nt.StretchRegion
                (mStretchState.mBeat0, mStretchState.mBeat1, dur);
            nt.ChannelGroup::ShiftBy(moveto - t0);
            mStretchState.mBeat0.first = moveto;
            viewInfo.selectedRegion.setT0(moveto);
            break;
        }
        case stretchRight: {
            dur = moveto - t0;
            if (dur < mStretchState.mLeftBeats * minPeriod) {
                return;
            }
            nt.StretchRegion
                (mStretchState.mBeat0, mStretchState.mBeat1, dur);
            viewInfo.selectedRegion.setT1(moveto);
            mStretchState.mBeat1.first = moveto;
            break;
        }
        case stretchCenter: {
            moveto = std::max(t0, std::min(t1, moveto));
            left_dur = moveto - t0;
            right_dur = t1 - moveto;
            if (left_dur < mStretchState.mLeftBeats * minPeriod
                || right_dur < mStretchState.mRightBeats * minPeriod) {
                return;
            }
            nt.StretchRegion
                (mStretchState.mBeatCenter, mStretchState.mBeat1, right_dur);
            nt.StretchRegion
                (mStretchState.mBeat0, mStretchState.mBeatCenter, left_dur);
            mStretchState.mBeatCenter.first = moveto;
            break;
        }
        default:
            wxASSERT(false);
            break;
        }
    }
}

#endif
