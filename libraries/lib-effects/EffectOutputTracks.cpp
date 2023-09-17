/**********************************************************************

  Audacity: A Digital Audio Editor

  EffectOutputTracks.cpp

  Paul Licameli split from Effect.cpp and EffectBase.cpp

**********************************************************************/
#include "EffectOutputTracks.h"
#include "BasicUI.h"
#include "SyncLock.h"
#include "UserException.h"
#include "WaveTrack.h"
#include "WaveTrackUtilities.h"

// Effect application counter
int EffectOutputTracks::nEffectsDone = 0;

EffectOutputTracks::EffectOutputTracks(
   TrackList& tracks, std::optional<TimeInterval> effectTimeInterval,
   bool allSyncLockSelected, bool stretchSyncLocked)
    : mTracks { tracks }
{
   assert(
      !effectTimeInterval.has_value() ||
      effectTimeInterval->first <= effectTimeInterval->second);
   // Reset map
   mIMap.clear();
   mOMap.clear();
   mOutputTracks = TrackList::Create(mTracks.GetOwner());

   auto trackRange = mTracks.Any() +
      [&] (const Track *pTrack) {
         return allSyncLockSelected
         ? SyncLock::IsSelectedOrSyncLockSelected(pTrack)
         : dynamic_cast<const WaveTrack*>(pTrack) && pTrack->GetSelected();
      };

   for (auto aTrack : trackRange) {
      auto list = aTrack->Duplicate();
      mIMap.push_back(aTrack);
      mOMap.push_back(*list->begin());
      mOutputTracks->Append(std::move(*list));
   }

   if (
      effectTimeInterval.has_value() &&
      effectTimeInterval->second > effectTimeInterval->first)
   {
      using namespace BasicUI;
      auto progress = MakeProgress(
         XO("Pre-processing"), XO("Rendering Time-Stretched Audio"),
         ProgressShowCancel);
      const auto waveTracks = (stretchSyncLocked
         ? mOutputTracks->Any<WaveTrack>()
         : mOutputTracks->Selected<WaveTrack>())
      + [&](const WaveTrack *pTrack){
         return WaveTrackUtilities::HasStretch(*pTrack,
            effectTimeInterval->first, effectTimeInterval->second);
      };
      const auto numTracks = waveTracks.size();
      auto count = 0;
      auto reportProgress = [&](double progressFraction) {
         const auto overallProgress = (count + progressFraction) / numTracks;
         const auto result = progress->Poll(overallProgress * 1000, 1000);
         if (result != ProgressResult::Success)
            throw UserException {};
      };
      for (const auto& track : waveTracks)
      {
         track->ApplyStretchRatio(effectTimeInterval, reportProgress);
         ++count;
      }
   }

   // Invariant is established
   assert(mIMap.size() == mOutputTracks->Size());
   assert(mIMap.size() == mOMap.size());
}

EffectOutputTracks::~EffectOutputTracks() = default;

Track *EffectOutputTracks::AddToOutputTracks(const std::shared_ptr<Track> &t)
{
   assert(t && t->IsLeader() && t->NChannels() == 1);
   mIMap.push_back(nullptr);
   mOMap.push_back(t.get());
   auto result = mOutputTracks->Add(t);
   // Invariant is maintained
   assert(mIMap.size() == mOutputTracks->Size());
   assert(mIMap.size() == mOMap.size());
   return result;
}

// Replace tracks with successfully processed mOutputTracks copies.
// Else clear and delete mOutputTracks copies.
void EffectOutputTracks::Commit()
{
   if (!mOutputTracks) {
      // Already committed, violating precondition.  Maybe wrong intent...
      assert(false);
      // ... but harmless
      return;
   }

   size_t cnt = mOMap.size();
   size_t i = 0;

   while (!mOutputTracks->empty()) {
      const auto pOutputTrack = *mOutputTracks->begin();

      // If tracks were removed from mOutputTracks, then there will be
      // tracks in the map that must be removed from mTracks.
      while (i < cnt && mOMap[i] != pOutputTrack) {
         const auto t = mIMap[i];
         // Class invariant justifies the assertion
         assert(t && t->IsLeader());
         ++i;
         mTracks.Remove(*t);
      }

      // The output track, still in the list, must also have been placed in
      // the map
      assert(i < cnt);

      // Find the input track it corresponds to
      if (!mIMap[i])
         // This track was an addition to output tracks; add it to mTracks
         mTracks.AppendOne(std::move(*mOutputTracks));
      else
         // Replace mTracks entry with the new track
         mTracks.ReplaceOne(*mIMap[i], std::move(*mOutputTracks));
      ++i;
   }

   // If tracks were removed from mOutputTracks, then there may be tracks
   // left at the end of the map that must be removed from mTracks.
   while (i < cnt) {
      const auto t = mIMap[i];
      // Class invariant justifies the assertion
      assert(t && t->IsLeader());
      ++i;
      mTracks.Remove(*t);
   }

   // Reset map
   mIMap.clear();
   mOMap.clear();

   // Make sure we processed everything
   assert(mOutputTracks->empty());

   // The output list is no longer needed
   mOutputTracks.reset();
   ++nEffectsDone;
}
