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
void clipsPair(ITrackeditProjectPtr trackeditProjectPtr,
               const Clips& before,
               const Clips& after)
{
    //! Check for added clips:
    notifier<Clip>(
        before,
        after,
        [&](const Clip& clip, int) {
        trackeditProjectPtr->notifyAboutClipAdded(clip);
    },
        [](const Clip& first, const Clip& second) {
        return first.key == second.key;      // Here we're only interested in if the "id" exists.
    }
        );

    //! Check for removed clips:
    notifier<Clip>(
        after,
        before,
        [&](const Clip& clip, int) {
        trackeditProjectPtr->notifyAboutClipRemoved(clip);
    },
        [](const Clip& first, const Clip& second) {
        return first.key == second.key;      // Here we're only interested in if the "id" exists.
    }
        );

    //! Check for changed clips:

    auto clipComparison = [](const Clip& first, const Clip& second) {
        IF_ASSERT_FAILED(first.key == second.key)
        {
            // This comparison assumes the key has been the same.
            return false;
        }

        return first.groupId == second.groupId
               && first.startTime == second.startTime
               && first.endTime == second.endTime
               && first.stereo == second.stereo
               && first.pitch == second.pitch
               && first.speed == second.speed;
        //! For now these do not result in "autosave",
        //  and so should not be criteria under undo/redo refresh.
        //  This might change in future AU4 versions.
        // first.title == second.title &&
        // first.color == second.color &&
        // first.hasCustomColor == second.hasCustomColor &&
        // first.optimizeForVoice == second.optimizeForVoice &&
        // first.stretchToMatchTempo == second.stretchToMatchTempo
    };

    // Brute force I'm afraid:
    for (const Clip& clipBefore : before) {
        for (const Clip& clipAfter : after) {
            if ((clipBefore.key == clipAfter.key)
                && !clipComparison(clipBefore, clipAfter)) {
                trackeditProjectPtr->notifyAboutClipChanged(clipAfter);
            }
        }
    }
}
}  // namespace

namespace au::trackedit::changeDetection {
void notifyOfUndoRedo(const TracksAndClips& before,
                      const TracksAndClips& after,
                      ITrackeditProjectPtr trackeditProject)
{
    auto trackComparison = [](const Track& first, const Track& second) {
        return first.id == second.id;
        //! For now these do not result in "autosave",
        //  and so should not be criteria under undo/redo.
        //  This might change in future AU4 versions.
        // first.title == second.title &&
        // first.type == second.type &&
        // first.color == second.color;
    };

    if (before.tracks.size() < after.tracks.size()) {
        //! Before is smaller than after. Something had been deleted. Find and notify that it's added.

        notifier<Track>(
            before.tracks,
            after.tracks,
            [&](Track track, int index) {
            trackeditProject->trackInserted().send(track, index);
        },
            trackComparison
            );

        clipsMatcher(before.tracks,
                     after.tracks,
                     [&](int i, int j) {
            clipsPair(trackeditProject, before.clips[i], after.clips[j]);
        }
                     );
    } else if (before.tracks.size() > after.tracks.size()) {
        //! After is smaller than before. Something had been added. Find and notify that it's removed.

        notifier<Track>(
            after.tracks,
            before.tracks,
            [&](Track track, int) {
            trackeditProject->trackRemoved().send(track);
        },
            trackComparison
            );

        clipsMatcher(after.tracks,
                     before.tracks,
                     [&](int i, int j) {
            clipsPair(trackeditProject, before.clips[j], after.clips[i]);
        }
                     );
    } else {
        //! The sizes are euqual.
        //  Reordering is a problem:
        //  We can only detect that tracks are out of order, we cannot detect which have moved.
        //  Imagine moving the first track to last.
        //  All tracks in the project will be flagged as out of order.
        //  So for now we just reload as before.

        // TODO: We could try to devise an algorithm that finds the minimal difference between the lists.
        //       Later.

        for (int i = 0; i < before.tracks.size(); i++) {
            const Track& trackBefore = before.tracks[i];
            const Track& trackAfter = after.tracks[i];
            if (trackBefore.id != trackAfter.id) {
                trackeditProject->reload();

                //! We've reloaded after detecting reordering!
                //  So there's no need to carry on with trying to detect minor changes.
                return;
            }
        }

        //! Finally - can there just have been a change in the track fields?
        for (int i = 0; i < before.tracks.size(); i++) {
            const Track& trackBefore = before.tracks[i];
            const Track& trackAfter = after.tracks[i];
            if (!trackComparison(trackBefore, trackAfter)) {
                trackeditProject->trackChanged().send(trackBefore);
            }
        }

        clipsMatcher(after.tracks,
                     before.tracks,
                     [&](int i, int j) {
            clipsPair(trackeditProject, before.clips[j], after.clips[i]);
        }
                     );
    }
}
}
