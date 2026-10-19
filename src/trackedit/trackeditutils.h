/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <optional>

#include "trackedit/dom/clip.h"
#include "trackedit/dom/track.h"
#include "trackedit/itrackeditproject.h"

namespace au::trackedit::utils {
bool hasStereoTrack(const TrackList& tracks);

TrackIdList itemTrackIds(const ItemKeys& items);

std::optional<TrackItemKey> firstItemOnTrack(const ItemKeys& timeSortedItems, const TrackId& trackId);

std::vector<const Clip*> clipSetDifference(const Clips& set1, const Clips& set2);

template<typename Fn>
void executeAndNotifyAboutChangedClips(ITrackeditProjectPtr prj, TrackId trackId, Fn&& fn)
{
    const auto clipsBefore = prj->clipList(trackId);
    std::forward<Fn>(fn)();
    const auto clipsAfter = prj->clipList(trackId);
    for (const Clip* clip : clipSetDifference(clipsBefore, clipsAfter)) {
        prj->notifyAboutClipRemoved(*clip);
    }
    for (const Clip* clip : clipSetDifference(clipsAfter, clipsBefore)) {
        prj->notifyAboutClipAdded(*clip);
    }
}
}
