/*
 * Audacity: A Digital Audio Editor
 */
#include "trackeditutils.h"

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

au::trackedit::TrackIdList au::trackedit::utils::tracksToMixAndRender(const ITrackeditProject& project, const TrackIdList& selectedTracks)
{
    const TrackIdList candidates = selectedTracks.empty() ? project.trackIdList() : selectedTracks;

    TrackIdList result;
    for (const TrackId& trackId : candidates) {
        const std::optional<Track> track = project.track(trackId);
        if (track && (track->type == TrackType::Mono || track->type == TrackType::Stereo)) {
            result.push_back(trackId);
        }
    }

    return result;
}
