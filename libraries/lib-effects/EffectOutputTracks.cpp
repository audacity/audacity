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

   auto trackRange = mTracks.Leaders() +
      [&] (const Track *pTrack) {
         return allSyncLockSelected
         ? SyncLock::IsSelectedOrSyncLockSelected(pTrack)
         : dynamic_cast<const WaveTrack*>(pTrack) && pTrack->GetSelected();
      };

   for (auto aTrack : trackRange) {
      auto list = aTrack->Duplicate();
      assert(aTrack->NChannels() == list->NChannels());
      auto iter = TrackList::Channels(*list->Leaders().begin()).begin();
      for (auto pChannel : TrackList::Channels(aTrack)) {
         Track *o = *iter++;
         mIMap.push_back(pChannel);
         mOMap.push_back(o);
      }
      mOutputTracks->Append(std::move(*list));
   }
   // Invariant is established
   assert(mIMap.size() == mOMap.size());
}

EffectOutputTracks::~EffectOutputTracks() = default;

Track *EffectOutputTracks::AddToOutputTracks(const std::shared_ptr<Track> &t)
{
   assert(t && t->IsLeader());
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

   auto range = mOutputTracks->Any();
   std::vector<std::shared_ptr<Track>> channels;

   size_t cnt = mOMap.size();
   size_t i = 0;

   while (!range.empty()) {
      auto *const pOutputTrack = *range.first;
      const auto nChannels = pOutputTrack->NChannels();

      // Get shared pointers to keep the tracks alive after removal from list
      channels.clear();
      for (size_t iChannel = 0; iChannel < nChannels; ++iChannel)
         channels.emplace_back((*range.first++)->shared_from_this());

      // If tracks were removed from mOutputTracks, then there will be
      // tracks in the map that must be removed from mTracks.
      while (i < cnt && mOMap[i] != pOutputTrack) {
         const auto t = mIMap[i];
         // Class invariant justifies the assertion
         assert(t && t->IsLeader());
         i += t->NChannels();
         mTracks.Remove(*t);
      }

      // The output track, still in the list, must also have been placed in
      // the map
      assert(i < cnt);

      // Remove the track from the output list...don't delete it
      mOutputTracks->Remove(*pOutputTrack);

      // Find the input track it corresponds to
      if (!mIMap[i])
         for (auto &o : channels)
            // This track was an addition to output tracks; add it to mTracks
            ++i, mTracks.Add(o);
      else
         for (auto &o : channels)
            // Replace mTracks entry with the new track
            mTracks.Replace(mIMap[i++], o);
   }

   // If tracks were removed from mOutputTracks, then there may be tracks
   // left at the end of the map that must be removed from mTracks.
   while (i < cnt) {
      const auto t = mIMap[i];
      // Class invariant justifies the assertion
      assert(t && t->IsLeader());
      i += t->NChannels();
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
