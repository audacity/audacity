
#include "Au3ChangeDetection.h"

// TODO: 2: THINK well - is this equality comparison what's needed?
//  Because maybe the slightest change then still triggers a refresh?
namespace {
template <typename TYPE>
void notifier (const std::vector<TYPE>& firstList,
               const std::vector<TYPE>& secondList,
               std::function<void (const TYPE& item, int)> notification)
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

    //! Track changes:

    auto& tracksBefore = before.first;
    auto& tracksAfter = after.first;

    if (tracksBefore.size() < tracksAfter.size()) {
        //! Before is smaller than after. Something had been deleted. Find and notify that it's added.
        notifier<Track>(tracksBefore,
                        tracksAfter,
                        [&](const Track& track, int index) {
            trackeditProject->trackInserted().send(track, index);
        });
    }
    else if (tracksBefore.size() > tracksAfter.size()) {
        //! After is smaller than before. Something had been added. Find and notify that it's removed.
        notifier<Track>(tracksAfter,
                        tracksBefore,
                        [&](const Track& track, int) {
            trackeditProject->trackRemoved().send(track);
        });
    }
    else if (tracksBefore.size() == tracksAfter.size()) {
        // Reordering is a problem.
        // I can only detect that tracks are out of order, I cannot detect which has moved.
        // Imagine moving the first track to last. All tracks before it will be flagged as out of order.
        // For now I just reload as before.
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

        // TODO: Implement also for just CHANGING something in a track.
    }

    //! Clip changes:

    // Assuming tracks are unchanged in that case.
    IF_ASSERT_FAILED (tracksBefore.size() == tracksAfter.size()) {
        return;
    }

    for (int i = 0; i < before.first.size(); i++) {
        auto& clipsBefore = before.second[i];
        auto& clipsAfter = after.second[i];

        if (clipsBefore.size() < clipsAfter.size()) {
            //! Before is smaller than after. Something had been deleted. Find and notify that it's added.
            notifier<Clip>(clipsBefore,
                           clipsAfter,
                           [&](const Clip& clip, int index) {
                trackeditProject->notifyAboutClipAdded(clip);
            });
        }

        if (clipsBefore.size() > clipsAfter.size()) {
            //! After is smaller than before. Something had been added. Find and notify that it's removed.
            notifier<Clip>(clipsAfter,
                           clipsBefore,
                           [&](const Clip& clip, int) {
                trackeditProject->notifyAboutClipRemoved(clip);
            });
        }

        // TODO: Unfinished. Fix after add/remove is done.
        if (clipsBefore.size() == clipsAfter.size()) {
            // Reordering is a problem, see above

            for (int c = 0; c < clipsBefore.size(); c++) {
                auto& clipBefore = clipsBefore[c];
                auto& clipAfter = clipsAfter[c];

                // TODO: Are these ID comparisons enough?
                if (clipBefore.key.clipId != clipAfter.key.clipId) {
                    // Detected that there is a reorder.
                    // trackeditProject->reload();
                    // TODO: NOTIFY for entire lane only at least.
                }
            }

            // TODO: Implement also for just CHANGING something in a track.
        }
    }
}
