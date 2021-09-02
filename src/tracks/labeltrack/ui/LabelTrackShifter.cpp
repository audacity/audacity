/*!
 @file LabelTrackShifter.cpp
 @brief headerless file injects method definitions for time shifting of LabelTrack
 */

#include "LabelTrackView.h"
#include "../../ui/TimeShiftHandle.h"
#include "../../../LabelTrack.h"
#include "ViewInfo.h"

class LabelTrackShifter final : public TrackShifter {
public:
   LabelTrackShifter( LabelTrack &track, AudacityProject &project )
      : mpTrack{ track.SharedPointer<LabelTrack>() }
      , mProject{ project }
   {
      InitIntervals();
      mpTrack->Bind(
         EVT_LABELTRACK_PERMUTED, &LabelTrackShifter::OnLabelPermuted, this );
      mpTrack->Bind(
         EVT_LABELTRACK_ADDITION, &LabelTrackShifter::OnLabelAdded, this );
      mpTrack->Bind(
         EVT_LABELTRACK_DELETION, &LabelTrackShifter::OnLabelDeleted, this );
   }
   ~LabelTrackShifter() override
   {
      mpTrack->Unbind(
         EVT_LABELTRACK_PERMUTED, &LabelTrackShifter::OnLabelPermuted, this );
      mpTrack->Unbind(
         EVT_LABELTRACK_ADDITION, &LabelTrackShifter::OnLabelAdded, this );
      mpTrack->Unbind(
         EVT_LABELTRACK_DELETION, &LabelTrackShifter::OnLabelDeleted, this );
   }
   Track &GetTrack() const override { return *mpTrack; }
   
   static inline size_t& GetIndex(TrackInterval &interval)
   {
      auto pExtra =
         static_cast<LabelTrack::IntervalData*>( interval.Extra() );
      return pExtra->index;
   }

   static inline size_t GetIndex(const TrackInterval &interval)
   {
      return GetIndex( const_cast<TrackInterval&>(interval) );
   }

   HitTestResult HitTest(
      double time, const ViewInfo &viewInfo, HitTestParams *pParams ) override
   {
      HitTestResult result = HitTestResult::Intervals;
      auto t0 = viewInfo.selectedRegion.t0();
      auto t1 = viewInfo.selectedRegion.t1();
      if ( mpTrack->IsSelected() && time >= t0 && time < t1 )
         result = HitTestResult::Selection;

      // Prefer the box that the mouse hovers over, else the selected one
      int iLabel = -1;
      if ( pParams )
         iLabel =
            LabelTrackView::OverATextBox(*mpTrack, pParams->xx, pParams->yy);
      if (iLabel == -1)
         iLabel = LabelTrackView::Get(*mpTrack).GetNavigationIndex(mProject);
      if (iLabel != -1) {
         UnfixIntervals([&](const auto &myInterval){
            return GetIndex( myInterval ) == iLabel;
         });
         return result;
      }
      else {
         // If the pick is within the selection, which overlaps some intervals,
         // then move those intervals only
         // Else move all labels (preserving the older beahvior of time shift)
         if ( result == HitTestResult::Selection )
            SelectInterval({ t0, t1 });
         if (mMoving.empty())
            return HitTestResult::Track;
         else
            return result;
      }
   }

   void SelectInterval( const TrackInterval &interval ) override
   {
      CommonSelectInterval(interval);
   }

   bool SyncLocks() override { return false; }

   bool MayMigrateTo( Track &otherTrack ) override
   {
      return CommonMayMigrateTo(otherTrack);
   }

   /* We need to copy a complete label when detaching it because LabelStruct
      is stored in a vector in LabelTrack without an extra indirection.
      So the detached intervals handed back to the caller are unlike those
      reported by LabelTrack, but carry the extra information. */
   struct IntervalData final: Track::IntervalData {
      SelectedRegion region;
      wxString title;
      IntervalData(const LabelStruct &label)
         : region{label.selectedRegion}
         , title{label.title}
      {}
   };

   Intervals Detach() override
   {
      auto pTrack = mpTrack.get();
      auto moveLabel = [pTrack](TrackInterval &interval) -> TrackInterval {
         auto &rindex = GetIndex(interval);
         auto index = rindex;
         rindex = -1;
         auto result = TrackInterval{
            interval.Start(), interval.End(),
            std::make_unique<IntervalData>( *pTrack->GetLabel(index) ) };
         pTrack->DeleteLabel(index);
         return result;
      };
      Intervals result;
      std::transform(
         // Reverse traversal may lessen the shifting-left in the label array
         mMoving.rbegin(), mMoving.rend(), std::back_inserter(result),
         moveLabel );
      mMoving = Intervals{};
      return result;
   }

   bool AdjustFit(
      const Track &, const Intervals &, double &, double ) override
   {
      // Labels have no overlapping constraints, so just...
      return true;
   }

   bool Attach( Intervals intervals ) override
   {
      auto pTrack = mpTrack.get();
      std::for_each( intervals.rbegin(), intervals.rend(),
         [this, pTrack](auto &interval){
            auto pData = static_cast<IntervalData*>( interval.Extra() );
            auto index = pTrack->AddLabel(pData->region, pData->title);
            // Recreate the simpler TrackInterval as would be reported by LabelTrack
            mMoving.emplace_back( pTrack->MakeInterval(index) );
         } );
      return true;
   }

   void DoHorizontalOffset( double offset ) override
   {
      auto &labels = mpTrack->GetLabels();
      for ( auto &interval : MovingIntervals() ) {
         auto index = GetIndex( interval );
         auto labelStruct = labels[index];
         labelStruct.selectedRegion.move(offset);
         mpTrack->SetLabel( index, labelStruct );
      }

      mpTrack->SortLabels(); // causes callback to OnLabelPermuted
   }

private:
   void OnLabelPermuted( LabelTrackEvent &e )
   {
      e.Skip();
      if ( e.mpTrack.lock() != mpTrack )
         return;

      auto former = e.mFormerPosition;
      auto present = e.mPresentPosition;

      // Avoid signed-unsigned comparison below!
      if (former < 0 || present < 0) {
         wxASSERT(false);
         return;
      }

      auto update = [=]( TrackInterval &interval ){
         auto &index = GetIndex( interval );
         if ( index == former )
            index = present;
         else if ( former < index && index <= present )
            -- index;
         else if ( former > index && index >= present )
            ++ index;
      };

      std::for_each(mFixed.begin(), mFixed.end(), update);
      std::for_each(mMoving.begin(), mMoving.end(), update);
   }

   void OnLabelAdded( LabelTrackEvent &e )
   {
      e.Skip();
      if ( e.mpTrack.lock() != mpTrack )
         return;

      auto present = e.mPresentPosition;

      // Avoid signed-unsigned comparison below!
      if (present < 0) {
         wxASSERT(false);
         return;
      }

      auto update = [=]( TrackInterval &interval ){
         auto pExtra = static_cast<LabelTrack::IntervalData*>(interval.Extra());
         auto &index = pExtra->index;
         if ( index >= present )
            ++ index;
      };

      std::for_each(mFixed.begin(), mFixed.end(), update);
      std::for_each(mMoving.begin(), mMoving.end(), update);
   }

   void OnLabelDeleted( LabelTrackEvent &e )
   {
      e.Skip();
      if ( e.mpTrack.lock() != mpTrack )
         return;

      auto former = e.mFormerPosition;

      // Avoid signed-unsigned comparison below!
      if (former < 0) {
         wxASSERT(false);
         return;
      }

      auto update = [=]( TrackInterval &interval ){
         auto pExtra = static_cast<LabelTrack::IntervalData*>(interval.Extra());
         auto &index = pExtra->index;
         if ( index > former )
            -- index;
         else if ( index == former )
            // It should have been deleted first!
            wxASSERT( false );
      };

      std::for_each(mFixed.begin(), mFixed.end(), update);
      std::for_each(mMoving.begin(), mMoving.end(), update);
   }

   std::shared_ptr<LabelTrack> mpTrack;
   AudacityProject &mProject;
};

using MakeLabelTrackShifter = MakeTrackShifter::Override<LabelTrack>;
DEFINE_ATTACHED_VIRTUAL_OVERRIDE(MakeLabelTrackShifter) {
   return [](LabelTrack &track, AudacityProject &project) {
      return std::make_unique<LabelTrackShifter>(track, project);
   };
}
