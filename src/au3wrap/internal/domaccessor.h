/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <memory>
#include <list>

#include "libraries/lib-wave-track/WaveTrack.h"
#include "libraries/lib-wave-track/WaveClip.h"

#include "trackedit/trackedittypes.h"
#include "../au3types.h"

namespace au::au3 {
class DomAccessor
{
public:
    static Au3Track* findTrack(Au3Project& prj, const Au3TrackId& au3trackId);
    static Au3WaveTrack* findWaveTrack(Au3Project& prj, const Au3TrackId& au3trackId);
    static std::shared_ptr<Au3WaveClip> findWaveClip(Au3WaveTrack* track, uint64_t au3ClipId);
    static std::shared_ptr<Au3WaveClip> findWaveClip(Au3Project& prj, const Au3TrackId& au3trackId, size_t index);
    static std::shared_ptr<Au3WaveClip> findWaveClip(Au3Project& prj, const trackedit::TrackId &trackId, trackedit::secs_t time);
    static size_t findClipIndexById(const Au3WaveTrack* track, const trackedit::ClipId& clipId);
    static trackedit::ClipId findClipIdByIndex(const Au3WaveTrack* track,  size_t clipIndex);
    static trackedit::ClipId findMatchedClip(const Au3WaveTrack* track, const Au3WaveTrack* originTrack,
                                             const trackedit::ClipId& originClipId);
    static std::list<std::shared_ptr<Au3WaveClip> > waveClipsAsList(Au3WaveTrack* track);
};
}
