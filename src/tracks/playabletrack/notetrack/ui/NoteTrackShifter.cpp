/*!
 @file NoteTrackShifter.cpp
 @brief headerless file injects method definitions for time shifting of NoteTrack
 */

#include "../../../ui/TimeShiftHandle.h"
#include "../../../../NoteTrack.h"
#include "../../../../ViewInfo.h"

class NoteTrackShifter final : public TrackShifter {
public:
   NoteTrackShifter( NoteTrack &track )
      : mpTrack{ track.SharedPointer<NoteTrack>() }
   {
      InitIntervals();
   }
   ~NoteTrackShifter() override {}
   Track &GetTrack() const override { return *mpTrack; }
   
   HitTestResult HitTest(
      double time, const ViewInfo &viewInfo, HitTestParams* ) override
   {
      UnfixAll();
      auto t0 = viewInfo.selectedRegion.t0();
      auto t1 = viewInfo.selectedRegion.t1();
      if ( mpTrack->IsSelected() && time >= t0 && time < t1 )
         return HitTestResult::Selection;
      else
         return HitTestResult::Intervals;
   }

   void SelectInterval( const TrackInterval &interval ) override
   {
      CommonSelectInterval( interval );
   }

   bool SyncLocks() override { return true; }

private:
   std::shared_ptr<NoteTrack> mpTrack;
};

using MakeNoteTrackShifter = MakeTrackShifter::Override<NoteTrack>;
template<> template<> auto MakeNoteTrackShifter::Implementation() -> Function {
   return [](NoteTrack &track, AudacityProject&) {
      return std::make_unique<NoteTrackShifter>(track);
   };
}
static MakeNoteTrackShifter registerMakeNoteTrackShifter;
