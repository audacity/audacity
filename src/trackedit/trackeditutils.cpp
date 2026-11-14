/*
 * Audacity: A Digital Audio Editor
 */
#include "trackeditutils.h"

#include <algorithm>

std::vector<const au::trackedit::Clip*> au::trackedit::utils::clipSetDifference(const Clips& set1, const Clips& set2)
{
    std::vector<const Clip*> result;
    for (const auto& clip : set1) {
        if (std::find_if(set2.begin(), set2.end(), [&clip](const Clip& c) { return c.key == clip.key; }) == set2.end()) {
            result.push_back(&clip);
        }
    }
    return result;
}

bool au::trackedit::utils::hasStereoTrack(const au::trackedit::TrackList& tracks)
{
    return std::any_of(tracks.begin(), tracks.end(), [](const au::trackedit::Track& track) {
        return track.type == au::trackedit::TrackType::Stereo;
    });
}

au::trackedit::TrackIdList au::trackedit::utils::itemTrackIds(const ItemKeys& items)
{
    TrackIdList result;
    for (const TrackItemKeyList* keys : { &items.clips, &items.labels }) {
        for (const TrackItemKey& key : *keys) {
            if (std::find(result.begin(), result.end(), key.trackId) == result.end()) {
                result.push_back(key.trackId);
            }
        }
    }
    return result;
}

std::optional<au::trackedit::TrackItemKey> au::trackedit::utils::firstItemOnTrack(const ItemKeys& timeSortedItems, const TrackId& trackId)
{
    const auto onTrack = [&trackId](const TrackItemKey& key) { return key.trackId == trackId; };

    auto it = std::find_if(timeSortedItems.clips.begin(), timeSortedItems.clips.end(), onTrack);
    if (it != timeSortedItems.clips.end()) {
        return *it;
    }

    it = std::find_if(timeSortedItems.labels.begin(), timeSortedItems.labels.end(), onTrack);
    if (it != timeSortedItems.labels.end()) {
        return *it;
    }

    return std::nullopt;
}

au::trackedit::TrackIdList au::trackedit::utils::audibleAudioTracks(const ITrackeditProject& project, const TrackIdList& trackIds)
{
    const auto isAudio = [](const Track& track) {
        return track.type == TrackType::Mono || track.type == TrackType::Stereo;
    };

    const TrackList tracks = project.trackList();
    const bool anySolo = std::any_of(tracks.cbegin(), tracks.cend(), [&isAudio](const Track& track) {
        return isAudio(track) && track.solo;
    });
    const auto isAudible = [anySolo](const Track& track) {
        return anySolo ? track.solo : !track.mute;
    };

    TrackIdList result;
    for (const TrackId& trackId : trackIds) {
        const std::optional<Track> track = project.track(trackId);
        if (track && isAudio(*track) && isAudible(*track) && !project.clipList(trackId).empty()) {
            result.push_back(trackId);
        }
    }

    return result;
}
