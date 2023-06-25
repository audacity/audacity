/**********************************************************************

  Audacity: A Digital Audio Editor

  EffectOutputTracks.h

  Paul Licameli split from EffectBase.h

**********************************************************************/
#ifndef __AUDACITY_EFFECT_OUTPUT_TRACKS__
#define __AUDACITY_EFFECT_OUTPUT_TRACKS__

class Track;
class TrackList;

#include <memory>
#include <vector>

//! Use this object to copy the input tracks to tentative outputTracks
/*
 Do the processing on temporaries, and then the originals are replaced only on
 Commit().
 If not all sync-locked selected, then only selected wave tracks.
*/
class EFFECTS_API EffectOutputTracks
{
public:
   // A global counter of all the successful Effect invocations.
   static int nEffectsDone;
   static void IncEffectCounter() { ++nEffectsDone; }

   EffectOutputTracks(TrackList &tracks,
      bool allSyncLockSelected = false);
   EffectOutputTracks(const EffectOutputTracks&) = delete;

   ~EffectOutputTracks();

   //! Use this to add an output track, not corresponding to an input.
   /*!
    @pre `t && t->IsLeader()`
    @return a pointer to the given track
    */
   Track *AddToOutputTracks(const std::shared_ptr<Track> &t);

   //! Replace input tracks with temporaries only on commit
   /*
    @pre `Commit()` was not previously called
    */
   void Commit();

   //! Expose the output track list for iterations or even erasures
   /*
    @pre `Commit()` was not previously called
    */
   TrackList &Get() { return *mOutputTracks; }

private:
   TrackList &mTracks;
   /*!
    @invariant `mIMap.size() == mOMap.size()`

    TODO wide wave tracks -- remove this
    @invariant mIMap is a concatenation of complete channel groups
    */
   std::vector<Track*> mIMap;
   std::vector<Track*> mOMap;
   std::shared_ptr<TrackList> mOutputTracks;
};

#endif
