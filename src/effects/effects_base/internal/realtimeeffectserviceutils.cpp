/*
 * Audacity: A Digital Audio Editor
 */
#include "realtimeeffectserviceutils.h"
#include "irealtimeeffectservice.h"
#include "au3wrap/internal/domaccessor.h"

std::optional<au::effects::TrackId> au::effects::utils::trackId(const au::project::IAudacityProjectPtr& project,
                                                                const RealtimeEffectStatePtr& state)
{
    if (!project) {
        return {};
    }
    const auto au3Project = reinterpret_cast<au3::Au3Project*>(project->au3ProjectPtr());
    auto& trackList = TrackList::Get(*au3Project);
    for (WaveTrack* track : trackList.Any<WaveTrack>()) {
        if (RealtimeEffectList::Get(*track).FindState(state)) {
            return track->GetId();
        }
    }
    auto& masterList = RealtimeEffectList::Get(*au3Project);
    if (masterList.FindState(state)) {
        return IRealtimeEffectService::masterTrackId;
    }
    return {};
}

std::optional<au::effects::EffectChainLinkIndex> au::effects::utils::effectIndex(const au::project::IAudacityProjectPtr& project,
                                                                                 const RealtimeEffectStatePtr& state)
{
    if (!project) {
        return {};
    }
    const auto au3Project = reinterpret_cast<au3::Au3Project*>(project->au3ProjectPtr());
    auto& trackList = TrackList::Get(*au3Project);
    for (WaveTrack* track : trackList.Any<WaveTrack>()) {
        if (const std::optional<size_t> index = RealtimeEffectList::Get(*track).FindState(state)) {
            return static_cast<EffectChainLinkIndex>(*index);
        }
    }
    auto& masterList = RealtimeEffectList::Get(*au3Project);
    if (const std::optional<size_t> index = masterList.FindState(state)) {
        return static_cast<EffectChainLinkIndex>(*index);
    }
    return {};
}

std::optional<au::effects::utils::UtilData> au::effects::utils::utilData(const project::IAudacityProjectPtr& project, TrackId trackId)
{
    if (!project) {
        return {};
    }
    const auto au3Project = reinterpret_cast<au3::Au3Project*>(project->au3ProjectPtr());
    const auto isMasterTrack = trackId == IRealtimeEffectService::masterTrackId;
    const auto au3Track
        = isMasterTrack ? nullptr : dynamic_cast<au3::Au3Track*>(au3::DomAccessor::findTrack(*au3Project,
                                                                                             au3::Au3TrackId(trackId)));
    const auto effectList = au3Track ? &RealtimeEffectList::Get(*au3Track) : &RealtimeEffectList::Get(*au3Project);
    if (!au3Project || !(isMasterTrack || au3Track)) {
        return {};
    }
    return UtilData{ au3Project, au3Track, effectList };
}
