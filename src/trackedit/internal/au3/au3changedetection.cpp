#include "au3changedetection.h"

#include "log.h"

using namespace au::trackedit;

namespace {
/**
 * @tparam TYPE The type of object that is being compared between the lists,
 *              to detect change. Note that the == operator needs to be implemented.
 *              A design limitation of this method is that the comparison assumes all fields compared
 *              for equality, are also reasons to notify for change.
 *              If this is undesirable an additional comparison lambda argument can be added
 *              in a future revision.
 * @param longestList A list that should be larger or equal to the second list, with elements of TYPE
 * @param shortestList A second list, smaller or equal to the first one.
 * @param notification The notification to trigger for each item found in the first list,
 *                     but not the second.
 * @param comparison Lambda for detecting UNDOABLE change - not actual equality.
 *                   Which is why it doesn't simply use an '==' overload.
 */
template<typename TYPE>
void notifier(const std::vector<TYPE>& longestList,
              const std::vector<TYPE>& shortestList,
              const std::function<void(TYPE item, int)>& notification,
              const std::function<bool(const TYPE& first, const TYPE& second)>& comparison)
{
    for (int i = 0; i < shortestList.size(); i++) {
        bool found = false;
        for (auto it = longestList.begin(); it != longestList.end(); ++it) {
            if (comparison(*it, shortestList[i])) {
                found = true; // Found one, so we don't notify, go to next.
                break;
            }
        }

        if (!found) {
            notification(shortestList[i], i);
        }
    }
}

/**
     * Searches for, and notifies of, Tracks changes
     * @param before a vector of Tracks before the change
     * @param after a vector of Tracks after the change
     * @param trackeditProject a TrackEditProjectPtr expected to be valid
     * @return true if a change has been detected and notified.
     */
bool forTracks(const TrackList& before,
               const TrackList& after,
               au::trackedit::ITrackeditProjectPtr trackeditProject)
{
    bool changed = false;

    auto trackComparison = [](const Track& first, const Track& second) {
        return first.id == second.id;
        //! For now these do not result in "autosave",
        //  and so should not be criteria under undo/redo.
        //  This might change in future AU4 versions.
        // first.title == second.title &&
        // first.type == second.type &&
        // first.color == second.color;
    };

    if (before.size() < after.size()) {
        //! Before is smaller than after. Something had been deleted. Find and notify that it's added.

        notifier<Track>(before,
                        after,
                        [&](Track track, int index) {
            trackeditProject->trackInserted().send(track, index);
        },
                        trackComparison);

        changed = true;
    } else if (before.size() > after.size()) {
        //! After is smaller than before. Something had been added. Find and notify that it's removed.

        notifier<Track>(after,
                        before,
                        [&](Track track, int) {
            trackeditProject->trackRemoved().send(track);
        },
                        trackComparison);

        changed = true;
    } else {
        //! The sizes are euqual.
        //  Reordering is a problem:
        //  We can only detect that tracks are out of order, we cannot detect which have moved.
        //  Imagine moving the first track to last.
        //  All tracks in the project will be flagged as out of order.
        //  So for now we just reload as before.

        // TODO: We could try to devise an algorithm that finds the minimal difference between the lists.
        //       Later.

        for (int i = 0; i < before.size(); i++) {
            const Track& trackBefore = before[i];
            const Track& trackAfter = after[i];
            if (trackBefore.id != trackAfter.id) {
                trackeditProject->reload();

                //! We've reloaded after detecting reordering!
                //  So there's no need to carry on with trying to detect minor changes.
                return true;
            }
        }

        //! Finally - can there just have been a change in the track fields?
        //  I don't see "autosave" calls being triggered for any such changes currently.
        //  BUT we have discussed implementing that,
        //  after which trackComparison will compare more than only the ID.
        for (int i = 0; i < before.size(); i++) {
            const Track& trackBefore = before[i];
            const Track& trackAfter = after[i];
            if (!trackComparison(trackBefore, trackAfter)) {
                trackeditProject->trackChanged().send(trackBefore);
                changed = true;
            }
        }
    }

    return changed;
}

/**
 * Searches for, and notifies of, clip changes.
 * Note that the two lists passed need to have equal length.
 * @param before a vector of Clips lists before the change
 * @param after a vector of Clips lists after the change
 * @param trackeditProjectPtr a TrackEditProjectPtr expected to be valid
 */
void forClips(const std::vector<Clips>& before,
              const std::vector<Clips>& after,
              ITrackeditProjectPtr trackeditProjectPtr)
{
    IF_ASSERT_FAILED(before.size() == after.size()) {
        return;
    }

    for (int i = 0; i < before.size(); i++) {
        const Clips& clipsBefore = before[i];
        const Clips& clipsAfter = after[i];

        if (clipsBefore.size() < clipsAfter.size()) {
            //! Before is smaller than after. Something had been deleted. Find and notify that it's added.

            notifier<Clip>(clipsBefore,
                           clipsAfter,
                           [&](Clip clip, int) {
                trackeditProjectPtr->notifyAboutClipAdded(clip);
            },
                           [](const Clip& first, const Clip& second) {
                return first.key == second.key;                 // Here we're only interested in if the "id" exists.
            });
        } else if (clipsBefore.size() > clipsAfter.size()) {
            //! After is smaller than before. Something had been added. Find and notify that it's removed.

            notifier<Clip>(clipsAfter,
                           clipsBefore,
                           [&](Clip clip, int) {
                trackeditProjectPtr->notifyAboutClipRemoved(clip);
            },
                           [](const Clip& first, const Clip& second) {
                return first.key == second.key;                    // Here we're only interested in if the "id" exists.
            });
        } else {
            //! The sizes are equal.
            //  Detecting reordering is a problem, see above.
            //  And for clip change notification TrackeditProject lacks the API now anyway.

            auto clipComparison = [](const Clip& first, const Clip& second) {
                return first.key == second.key
                       && first.groupId == second.groupId
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

            for (int c = 0; c < clipsBefore.size(); c++) {
                const Clip& clipBefore = clipsBefore[c];
                const Clip& clipAfter = clipsAfter[c];

                if (!clipComparison(clipBefore, clipAfter)) {
                    trackeditProjectPtr->notifyAboutClipChanged(clipAfter);
                }
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
    const TrackList& tracksBefore = before.tracks;
    const TrackList& tracksAfter = after.tracks;

    //! Track changes:
    bool changed = forTracks(tracksBefore, tracksAfter, trackeditProject);

    //! It cannot be that both clips AND tracks were changed in the same undo/redo action.
    if (changed) {
        return;
    }

    //! Clip changes:
    forClips(before.clips, after.clips, trackeditProject);
}
}
