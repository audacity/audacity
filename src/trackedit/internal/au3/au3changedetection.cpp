
#include "Au3ChangeDetection.h"

namespace {

/**
 *
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
 */
template <typename TYPE>
void notifier(const std::vector<TYPE>& longestList, const std::vector<TYPE>& shortestList, std::function<void(TYPE item, int)> notification)
{
    for (int i = 0; i < shortestList.size(); i++) {
        auto second = shortestList[i];
        auto iter = std::find_if(longestList.begin(), longestList.end(), [&](const auto& item) {
            return item == second;
        });

        if (iter == longestList.end()) {
            notification(shortestList[i], i);
        }
    }
}
}  // namespace

void au::trackedit::Au3ChangeDetection::notifyOfUndoRedo(const TracksAndClips& before, const TracksAndClips& after)
{
    auto& tracksBefore = before.first;
    auto& tracksAfter = after.first;

    //! Track changes:
    bool changed = _forTracks(tracksBefore, tracksAfter);

    //! It cannot be that both clips AND tracks were changed in the same undo/redo action.
    if (changed) {
        return;
    }

    //! Clip changes:
    _forClips(before.second, after.second);
}

bool au::trackedit::Au3ChangeDetection::_forTracks(const TrackList& tracksBefore, const TrackList& tracksAfter)
{
    auto trackeditProjectPtr = globalContext()->currentProject()->trackeditProject();

    bool changed = false;

    if (tracksBefore.size() < tracksAfter.size()) {

        //! Before is smaller than after. Something had been deleted. Find and notify that it's added.

        notifier<Track>(tracksBefore, tracksAfter, [&](Track track, int index) {
            trackeditProjectPtr->trackInserted().send(std::move(track), index);
        });

        changed = true;
    } else if (tracksBefore.size() > tracksAfter.size()) {

        //! After is smaller than before. Something had been added. Find and notify that it's removed.

        notifier<Track>(tracksAfter, tracksBefore, [&](Track track, int) {
            trackeditProjectPtr->trackRemoved().send(std::move(track));
        });

        changed = true;
    } else if (tracksBefore.size() == tracksAfter.size()) {
        //! Reordering is a problem:
        //  We can only detect that tracks are out of order, we cannot detect which have moved.
        //  Imagine moving the first track to last.
        //  All tracks in the project will be flagged as out of order.
        //  So for now we just reload as before.

        // TODO: I could try to devise an algorithm that finds the minimal difference between the lists.
        //       Later.

        for (int i = 0; i < tracksBefore.size(); i++) {
            auto& trackBefore = tracksBefore[i];
            auto& trackAfter = tracksAfter[i];
            if (trackBefore.id != trackAfter.id) {
                trackeditProjectPtr->reload();

                //! We've reloaded after detecting reordering!
                //  So there's no need to carry on with trying to detect minor changes.
                return true;
            }
        }

        //! Finally - can there just have been a change in the track fields?
        //  I don't see "autosave" calls being triggered for many such changes though.
        for (int i = 0; i < tracksBefore.size(); i++) {
            auto& trackBefore = tracksBefore[i];
            auto& trackAfter = tracksAfter[i];
            if (trackBefore != trackAfter) {
                trackeditProjectPtr->trackChanged().send(trackBefore);
                changed = true;
            }
        }
    }

    return changed;
}

void au::trackedit::Au3ChangeDetection::_forClips(const std::vector<Clips>& before, const std::vector<Clips>& after)
{
    auto trackeditProjectPtr = globalContext()->currentProject()->trackeditProject();

    IF_ASSERT_FAILED (before.size() == after.size()) {
        return;
    }

    for (int i = 0; i < before.size(); i++) {
        auto& clipsBefore = before[i];
        auto& clipsAfter = after[i];

        if (clipsBefore.size() < clipsAfter.size()) {

            //! Before is smaller than after. Something had been deleted. Find and notify that it's added.

            notifier<Clip>(clipsBefore, clipsAfter, [&](Clip clip, int) {
                trackeditProjectPtr->notifyAboutClipAdded(clip);
            });
        } else if (clipsBefore.size() > clipsAfter.size()) {

            //! After is smaller than before. Something had been added. Find and notify that it's removed.

            notifier<Clip>(clipsAfter, clipsBefore, [&](Clip clip, int) {
                trackeditProjectPtr->notifyAboutClipRemoved(clip);
            });
        } else if (clipsBefore.size() == clipsAfter.size()) {
            //! Detecting reordering is a problem, see above.
            //  And for clip change notification TrackeditProject lacks the API now anyway.

            for (int c = 0; c < clipsBefore.size(); c++) {
                auto& clipBefore = clipsBefore[c];
                auto& clipAfter = clipsAfter[c];

                if (clipBefore != clipAfter) {
                    trackeditProjectPtr->notifyAboutClipChanged(clipAfter);
                }
            }
        }
    }
}
