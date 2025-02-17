/**********************************************************************

  Audacity: A Digital Audio Editor

  EffectOutputTracks.h

  Paul Licameli split from EffectBase.h

**********************************************************************/
#ifndef __AUDACITY_EFFECT_OUTPUT_TRACKS__
#define __AUDACITY_EFFECT_OUTPUT_TRACKS__

class Track;
class TrackList;

#include "EffectInterface.h"

#include <memory>
#include <optional>
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

    using TimeInterval = std::pair<double, double>;
    /*!
     @param effectTimeInterval if given, and any copied tracks
     have clips with non-unit stretch intersecting that interval, then in the
     copies those clips are split, and new clips bounded by the interval, with
     the stretches applied, are inserted.
     @param allSyncLockSelected if true, unselected tracks that are sync-locked
     with a selected track are copied too
     @param stretchSyncLocked if false, do not apply the stretch interval to any
     unselected WaveTrack that is copied

     @pre `!effectTimeInterval.has_value() ||
        effectTimeInterval->first <= effectTimeInterval->second`
     */
    EffectOutputTracks(
        TrackList& tracks, EffectType effectType, std::optional<TimeInterval> effectTimeInterval, bool allSyncLockSelected = false,
        bool stretchSyncLocked = false);
    EffectOutputTracks(const EffectOutputTracks&) = delete;

    ~EffectOutputTracks();

    //! Use this to add an output track, not corresponding to an input.
    /*!
     @return a pointer to the given track
     */
    Track* AddToOutputTracks(const std::shared_ptr<Track>& t);

    /*!
     * @brief Gets the matching input track for the given output track if it
     * finds its match, else nullptr.
     */
    const Track* GetMatchingInput(const Track& outTrack) const;

    //! Replace input tracks with temporaries only on commit
    /*
     @pre `Commit()` was not previously called
     */
    void Commit();

    //! Expose the output track list for iterations or even erasures
    /*
     @pre `Commit()` was not previously called
     */
    TrackList& Get() { return *mOutputTracks; }

private:
    TrackList& mTracks;
    const EffectType mEffectType;
    /*!
     @invariant `mIMap.size() == mOutputTracks->Size()`
     @invariant `mIMap.size() == mOMap.size()`
     */
    std::vector<Track*> mIMap;
    std::vector<Track*> mOMap;
    std::shared_ptr<TrackList> mOutputTracks;
};

#endif
