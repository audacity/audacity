/**********************************************************************

  Audacity: A Digital Audio Editor

  EffectOutputTracks.cpp

  Paul Licameli split from Effect.cpp and EffectBase.cpp

**********************************************************************/
#include "EffectOutputTracks.h"
#include "SyncLock.h"
#include "WaveTrack.h"

// Effect application counter
int EffectOutputTracks::nEffectsDone = 0;

EffectOutputTracks::EffectOutputTracks(
   TrackList &tracks, bool allSyncLockSelected
)  : mTracks{ tracks }
{
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
      Track *o = mOutputTracks->Add(aTrack->Duplicate());
      mIMap.push_back(aTrack);
      mOMap.push_back(o);
   }
   // Invariant is established
   assert(mIMap.size() == mOMap.size());
}

EffectOutputTracks::~EffectOutputTracks() = default;

Track *EffectOutputTracks::AddToOutputTracks(const std::shared_ptr<Track> &t)
{
   mIMap.push_back(nullptr);
   mOMap.push_back(t.get());
   assert(mIMap.size() == mOMap.size());
   return mOutputTracks->Add(t);
}

// If bGoodResult, replace mTracks tracks with successfully processed mOutputTracks copies.
// Else clear and DELETE mOutputTracks copies.
void EffectOutputTracks::Commit()
{
   if (!mOutputTracks) {
      // Already committed, violating precondition.  Maybe wrong intent...
      assert(false);
      // ... but harmless
      return;
   }

   auto iterOut = mOutputTracks->ListOfTracks::begin(),
      iterEnd = mOutputTracks->ListOfTracks::end();

   size_t cnt = mOMap.size();
   size_t i = 0;

   for (; iterOut != iterEnd; ++i) {
      ListOfTracks::value_type o = *iterOut;
      // If tracks were removed from mOutputTracks, then there will be
      // tracks in the map that must be removed from mTracks.
      while (i < cnt && mOMap[i] != o.get()) {
         const auto t = mIMap[i];
         if (t)
            mTracks.Remove(t);
         ++i;
      }

      // The output track, still in the list, must also have been placed in
      // the map
      assert(i < cnt);

      // Remove the track from the output list...don't delete it
      // `o` saves the track itself from deletion
      iterOut = mOutputTracks->erase(iterOut);

      // Find the input track it corresponds to
      const auto t = mIMap[i];
      if (!t)
         // This track was an addition to output tracks; add it to mTracks
         mTracks.Add(o);
      else
         // Replace mTracks entry with the new track
         mTracks.Replace(t, o);
   }

   // If tracks were removed from mOutputTracks, then there may be tracks
   // left at the end of the map that must be removed from mTracks.
   while (i < cnt) {
      const auto t = mIMap[i];
      if (t)
         mTracks.Remove(t);
      ++i;
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
