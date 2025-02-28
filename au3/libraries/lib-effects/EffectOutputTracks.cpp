/**********************************************************************

  Audacity: A Digital Audio Editor

  EffectOutputTracks.cpp

  Paul Licameli split from Effect.cpp and EffectBase.cpp

**********************************************************************/
#include "EffectOutputTracks.h"
#include "BasicUI.h"
#include "SyncLock.h"
#include "UserException.h"
#include "WaveClip.h"
#include "WaveTrack.h"
#include "TimeStretching.h"

// Effect application counter
int EffectOutputTracks::nEffectsDone = 0;

EffectOutputTracks::EffectOutputTracks(
    TrackList& tracks, EffectType effectType,
    std::optional<TimeInterval> effectTimeInterval, bool allSyncLockSelected,
    bool stretchSyncLocked)
    : mTracks{tracks}
    , mEffectType{effectType}
{
    assert(
        !effectTimeInterval.has_value()
        || effectTimeInterval->first <= effectTimeInterval->second);
    // Reset map
    mIMap.clear();
    mOMap.clear();
    mOutputTracks = TrackList::Create(mTracks.GetOwner());

    auto trackRange = mTracks.Any()
                      +[&](const Track * pTrack) {
        return allSyncLockSelected
               ? SyncLock::IsSelectedOrSyncLockSelected(*pTrack)
               : dynamic_cast<const WaveTrack*>(pTrack) && pTrack->GetSelected();
    };

    for (auto aTrack : trackRange) {
        auto pTrack = aTrack->Duplicate(Track::DuplicateOptions {}.Backup());
        mIMap.push_back(aTrack);
        mOMap.push_back(pTrack.get());
        mOutputTracks->Add(pTrack, TrackList::DoAssignId::No);
    }

    if (
        effectTimeInterval.has_value()
        && effectTimeInterval->second > effectTimeInterval->first) {
        TimeStretching::WithClipRenderingProgress(
            [&](const ProgressReporter& parent)
        {
            const auto tracksToUnstretch
                =(stretchSyncLocked ? mOutputTracks->Any<WaveTrack>()
                  : mOutputTracks->Selected<WaveTrack>())
                  +[&](const WaveTrack * pTrack)
                {
                    return TimeStretching::HasPitchOrSpeed(
                        *pTrack, effectTimeInterval->first,
                        effectTimeInterval->second);
                };
            BasicUI::SplitProgress(
                tracksToUnstretch.begin(), tracksToUnstretch.end(),
                [&](WaveTrack* aTrack, const ProgressReporter& child) {
                aTrack->ApplyPitchAndSpeed(effectTimeInterval, child);
            },
                parent);
        });
    }

    // Invariant is established
    assert(mIMap.size() == mOutputTracks->Size());
    assert(mIMap.size() == mOMap.size());
}

EffectOutputTracks::~EffectOutputTracks() = default;

Track* EffectOutputTracks::AddToOutputTracks(const std::shared_ptr<Track>& t)
{
    mIMap.push_back(nullptr);
    mOMap.push_back(t.get());
    auto result = mOutputTracks->Add(t, TrackList::DoAssignId::No);
    // Invariant is maintained
    assert(mIMap.size() == mOutputTracks->Size());
    assert(mIMap.size() == mOMap.size());
    return result;
}

const Track* EffectOutputTracks::GetMatchingInput(const Track& outTrack) const
{
    const auto it = std::find(mOMap.begin(), mOMap.end(), &outTrack);
    if (it == mOMap.end()) {
        return nullptr;
    }
    const auto index = it - mOMap.begin();
    return mIMap[index];
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
            assert(t);
            ++i;
            mTracks.Remove(*t);
        }

        // The output track, still in the list, must also have been placed in
        // the map
        assert(i < cnt);

        // Find the input track it corresponds to
        if (!mIMap[i]) {
            // This track was an addition to output tracks; add it to mTracks
            mTracks.AppendOne(std::move(*mOutputTracks));
        } else if (
            mEffectType != EffectTypeNone && mEffectType != EffectTypeAnalyze) {
            // Replace mTracks entry with the new track
            mTracks.ReplaceOne(*mIMap[i], std::move(*mOutputTracks));
        } else {
            // This output track was just a placeholder for pre-processing. Discard
            // it.
            mOutputTracks->Remove(*pOutputTrack);
        }
        ++i;
    }

    // If tracks were removed from mOutputTracks, then there may be tracks
    // left at the end of the map that must be removed from mTracks.
    while (i < cnt) {
        const auto t = mIMap[i];
        // Class invariant justifies the assertion
        assert(t);
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
