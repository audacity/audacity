
#include "Au3ChangeDetection.h"

namespace {
template <typename TYPE>
void notifier (const std::vector<TYPE>& firstList,
               const std::vector<TYPE>& secondList,
               std::function<void (TYPE item, int)> notification)
{
    for (int i = 0; i < secondList.size(); i++) {
        auto second = secondList[i];
        auto iter = std::find_if(firstList.begin(), firstList.end(), [&](const auto& item) {
            return item == second;
        });

        if (iter == firstList.end()) {
            notification(secondList[i], i);
        }
    }
}
}

void au::trackedit::Au3ChangeDetection::notifyOfUndoRedo(const TracksAndClips& before, const TracksAndClips& after)
{
    auto trackeditProject = globalContext()->currentProject()->trackeditProject();

    bool changed = false;

    //! Track changes:

    auto& tracksBefore = before.first;
    auto& tracksAfter = after.first;

    if (tracksBefore.size() < tracksAfter.size()) {
        //! Before is smaller than after. Something had been deleted. Find and notify that it's added.
        notifier<Track>(tracksBefore,
                        tracksAfter,
                        [&](Track track, int index) {
            trackeditProject->trackInserted().send(std::move(track), index);
        });
        changed = true;
    }
    else if (tracksBefore.size() > tracksAfter.size()) {
        //! After is smaller than before. Something had been added. Find and notify that it's removed.
        notifier<Track>(tracksAfter,
                        tracksBefore,
                        [&](Track track, int) {
            trackeditProject->trackRemoved().send(std::move(track));
        });
        changed = true;
    }
    else if (tracksBefore.size() == tracksAfter.size()) {
        // Reordering is a problem.
        // We can only detect that tracks are out of order, we cannot detect which have moved.
        // Imagine moving the first track to last.
        // All tracks before it will be flagged as out of order.
        // So for now we just reload as before.
        // TODO: I could try to devise an algorithm that finds the minimal difference between the lists.
        //       Later.

        for (int i = 0; i < tracksBefore.size(); i++) {
            auto& trackBefore = tracksBefore[i];
            auto& trackAfter = tracksAfter[i];
            if (trackBefore.id != trackAfter.id) {
                // Detected that there is a reorder.
                trackeditProject->reload();

                //! We've reloaded!
                //  No need to carry on with trying to detect minor changes.
                return;
            }
        }

        //! And apart from reorder - can there have been just a change in the track?
        //  I don't see "autosave" calls being triggered for any such changes though.
        for (int i = 0; i < tracksBefore.size(); i++) {
            auto& trackBefore = tracksBefore[i];
            auto& trackAfter = tracksAfter[i];
            if (trackBefore != trackAfter) {
                trackeditProject->trackChanged().send(trackBefore);
            }
        }
    }

    //! Clip changes:

    // Assuming tracks are unchanged in that case.
    IF_ASSERT_FAILED ((tracksBefore.size() == tracksAfter.size()) || changed) {
        return;
    }

    for (int i = 0; i < before.first.size(); i++) {
        auto& clipsBefore = before.second[i];
        auto& clipsAfter = after.second[i];

        if (clipsBefore.size() < clipsAfter.size()) {
            //! Before is smaller than after. Something had been deleted. Find and notify that it's added.
            notifier<Clip>(clipsBefore,
                           clipsAfter,
                           [&](Clip clip, int) {
                trackeditProject->notifyAboutClipAdded(clip);
            });
        }

        if (clipsBefore.size() > clipsAfter.size()) {
            //! After is smaller than before. Something had been added. Find and notify that it's removed.
            notifier<Clip>(clipsAfter,
                           clipsBefore,
                           [&](Clip clip, int) {
                trackeditProject->notifyAboutClipRemoved(clip);
            });
        }

        if (clipsBefore.size() == clipsAfter.size()) {
            // Reordering is a problem, see above

            for (int c = 0; c < clipsBefore.size(); c++) {
                auto& clipBefore = clipsBefore[c];
                auto& clipAfter = clipsAfter[c];

                if (clipBefore != clipAfter) {
                    trackeditProject->notifyAboutClipChanged(clipAfter);
                }
            }
        }
    }
}
