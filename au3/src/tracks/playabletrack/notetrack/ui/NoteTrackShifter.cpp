/*!
 @file NoteTrackShifter.cpp
 @brief headerless file injects method definitions for time shifting of NoteTrack
 */

#include "../../../ui/TimeShiftHandle.h"
#include "NoteTrack.h"
#include "ViewInfo.h"

class NoteTrackShifter final : public TrackShifter
{
public:
    NoteTrackShifter(NoteTrack& track)
        : mpTrack{track.SharedPointer<NoteTrack>()}
    {
        InitIntervals();
    }

    ~NoteTrackShifter() override {}
    Track& GetTrack() const override { return *mpTrack; }

    HitTestResult HitTest(
        double time, const ViewInfo& viewInfo, HitTestParams*) override
    {
        UnfixAll();
        auto t0 = viewInfo.selectedRegion.t0();
        auto t1 = viewInfo.selectedRegion.t1();
        if (mpTrack->IsSelected() && time >= t0 && time < t1) {
            return HitTestResult::Selection;
        } else {
            return HitTestResult::Intervals;
        }
    }

    void SelectInterval(TimeInterval interval) override
    {
        CommonSelectInterval(interval);
    }

    bool SyncLocks() override { return true; }

    // Ensure that t0 is still within the data.
    // This corrects for any rounding errors.
    double AdjustT0(double t0) const override
    {
        auto& track = GetTrack();
        if (t0 < track.GetStartTime()) {
            t0 = track.GetStartTime();
        }
        if (t0 > track.GetEndTime()) {
            t0 = track.GetEndTime();
        }

        return t0;
    }

private:
    const std::shared_ptr<NoteTrack> mpTrack;
};

using MakeNoteTrackShifter = MakeTrackShifter::Override<NoteTrack>;
DEFINE_ATTACHED_VIRTUAL_OVERRIDE(MakeNoteTrackShifter) {
    return [](NoteTrack& track, AudacityProject&) {
        return std::make_unique<NoteTrackShifter>(track);
    };
}
