#include "changedetection.h"

#include "log.h"

using namespace au::trackedit;

namespace {
/**
 * @tparam TYPE The type of object that is being compared between the lists, to detect change.
 * @param notification The notification to trigger for each item found in the first list,
 *                     but not the second.
 * @param comparison Lambda for detecting UNDOABLE change - not actual equality.
 *                   Which is why it doesn't simply use an '==' overload.
 *                   So for example, to detect addition/deletion/move, compare only ID's,
 *                   since e.g. a simple mono->stereo change, should not trigger the same refresh,
 *                   as adding/deleting/moving.
 */
template<typename TYPE>
void notifier(const std::vector<TYPE>& first,
              const std::vector<TYPE>& second,
              const std::function<void(const TYPE& item, int)>& notification,
              const std::function<bool(const TYPE& first, const TYPE& second)>& comparison)
{
    for (int i = 0; i < second.size(); i++) {
        bool found = false;
        for (auto it = first.begin(); it != first.end(); ++it) {
            if (comparison(*it, second[i])) {
                found = true; // Found one, so we don't notify, go to next.
                break;
            }
        }

        if (!found) {
            notification(second[i], i);
        }
    }
}

/**
 * Iterates over two TrackLists, and when two track ID's match, it invokes the action
 */
void clipsMatcher(const TrackList& first,
                  const TrackList& second,
                  const std::function<void(int, int)>& action)
{
    for (int i = 0; i < second.size(); i++) {
        for (int j = 0; j < first.size(); j++) {
            if (second[i].id == first[j].id) {
                action(i, j);
            }
        }
    }
}

/**
 * Detect and notifies of changes between two clip lists
 * @param trackeditProject a TrackEditProjectPtr expected to be valid
 * @param before a vector of Clips lists before the change
 * @param after a vector of Clips lists after the change
 */
bool clipsPair(ITrackeditProjectPtr trackeditProjectPtr,
               const Clips& before,
               const Clips& after)
{
    bool changed = false;

    //! Check for added clips:
    notifier<Clip>(
        before,
        after,
        [&](const Clip& clip, int) {
        changed = true;
        trackeditProjectPtr->notifyAboutClipAdded(clip);
    },
        [](const Clip& first, const Clip& second) {
        return first.key.clipId == second.key.clipId; // Here we're only interested in if the "id" exists.
    }
        );

    //! Check for removed clips:
    notifier<Clip>(
        after,
        before,
        [&](const Clip& clip, int) {
        changed = true;
        trackeditProjectPtr->notifyAboutClipRemoved(clip);
    },
        [](const Clip& first, const Clip& second) {
        return first.key.clipId == second.key.clipId; // Here we're only interested in if the "id" exists.
    }
        );

    //! Check for changed clips:
    auto clipComparison = [](const Clip& first, const Clip& second) {
        IF_ASSERT_FAILED(first.key == second.key)
        {
            // This comparison assumes the key has been the same.
            return false;
        }

        return first.clipVersion == second.clipVersion
               && first.groupId == second.groupId
               && first.startTime == second.startTime
               && first.endTime == second.endTime
               && first.stereo == second.stereo
               && first.pitch == second.pitch
               && first.speed == second.speed
               && first.title == second.title;
        //! For now these do not result in "autosave",
        //  and so should not be criteria under undo/redo refresh.
        //  This might change in future AU4 versions.
        // first.color == second.color &&
        // first.hasCustomColor == second.hasCustomColor &&
        // first.optimizeForVoice == second.optimizeForVoice &&
        // first.stretchToMatchTempo == second.stretchToMatchTempo
    };

    // Brute force I'm afraid. Still much faster than redrawing:
    for (const Clip& clipBefore : before) {
        for (const Clip& clipAfter : after) {
            if ((clipBefore.key == clipAfter.key)
                && !clipComparison(clipBefore, clipAfter)) {
                changed = true;
                trackeditProjectPtr->notifyAboutClipChanged(clipAfter);
            }
        }
    }

    return changed;
}
}  // namespace

namespace au::trackedit::changeDetection {
void notifyOfUndoRedo(const TracksAndClips& before,
                      const TracksAndClips& after,
                      ITrackeditProjectPtr trackeditProject)
{
    bool changed = false;

    auto trackIdCheck = [](const Track& first, const Track& second) {
        return first.id == second.id;
    };

    //! Checking for Track reorder. If detected, reload and return.
    {
        auto trackBefore = before.tracks.begin();
        auto trackAfter = after.tracks.begin();

        //! Not assuming they are of equal length,
        //  since a reorder could have happened as part of a compound action.
        while (trackBefore != before.tracks.end()
               && trackAfter != after.tracks.end()) {
            if (trackBefore->id != trackAfter->id) {
                //! Reorder found.
                //  Reload and return.
                trackeditProject->reload();
                return;
            }
            ++trackBefore;
            ++trackAfter;
        }
    }

    //! Checking for Track addition:
    notifier<Track>(
        before.tracks,
        after.tracks,
        [&](Track track, int index) {
        changed = true;
        trackeditProject->trackInserted().send(track, index);
    },
        trackIdCheck
        );

    //! Checking for Track removal:
    notifier<Track>(
        after.tracks,
        before.tracks,
        [&](Track track, int) {
        changed = true;
        trackeditProject->trackRemoved().send(track);
    },
        trackIdCheck
        );

    //! Checking for Track field change - brute force I'm afraid:
    {
        auto trackFieldComparison = [](const Track& first, const Track& second) {
            return first.type == second.type
                   && first.title == second.title;

            //! For now these do not result in "autosave",
            //  and so should not be criteria under undo/redo.
            //  This might change in future AU4 versions.
            // first.color == second.color;
        };

        for (const Track& trackBefore : before.tracks) {
            for (const Track& trackAfter : after.tracks) {
                if ((trackBefore.id == trackAfter.id)
                    && !trackFieldComparison(trackBefore, trackAfter)) {
                    changed = true;
                    trackeditProject->trackChanged().send(trackAfter);
                }
            }
        }
    }

    //! Now checking for changes in Clips lists.
    clipsMatcher(after.tracks,
                 before.tracks,
                 [&](int i, int j) {
        bool clipChange = clipsPair(trackeditProject, before.clips[j], after.clips[i]);
        if (clipChange) {
            changed = true;
        }
    }
                 );

    //! Despite Undo-Redo being called, if this fails no change was detected.
    //  Reload everything to be sure - slow,
    //  but better than leaving the UI in an invalid state.
    if (!changed) {
        trackeditProject->reload();

        //! And log. This means there are undetected changes,
        //  and the change-detector needs updating to find them.
        LOGE() << "Undo-Redo changes were not detected - reloading UI.";
    }
}
}
