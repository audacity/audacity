/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <memory>
#include <list>

#include "trackedit/trackedittypes.h"

#include "libraries/lib-wave-track/WaveTrack.h"
#include "libraries/lib-wave-track/WaveClip.h"

namespace au::au3 {
class DomAccessor
{
public:

    static WaveTrack* findWaveTrack(AudacityProject& prj, const TrackId& au3trackId);
    static std::shared_ptr<WaveClip> findWaveClip(WaveTrack* track, uint64_t au3ClipId);
    static std::shared_ptr<WaveClip> findWaveClip(AudacityProject& prj, const TrackId& au3trackId, size_t index);
    static size_t findClipIndexById(const WaveTrack* track, const trackedit::ClipId& clipId);
    static trackedit::ClipId findClipIdByIndex(const WaveTrack* track,  size_t clipIndex);
    static trackedit::ClipId findMatchedClip(WaveTrack* track, const WaveTrack* originTrack, const trackedit::ClipId& originClipId);
    static std::list<std::shared_ptr<WaveClip> > waveClipsAsList(WaveTrack* track);
};
}
