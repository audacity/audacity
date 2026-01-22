/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <memory>
#include <list>

#include "au3-wave-track/WaveTrack.h"
#include "au3-wave-track/WaveClip.h"
#include "au3-label-track/LabelTrack.h"

#include "trackedit/trackedittypes.h"
#include "../au3types.h"

namespace au::au3 {
class DomAccessor
{
public:
    static Au3Track* findTrack(Au3Project& prj, const Au3TrackId& au3trackId);
    static const Au3Track* findTrack(const Au3Project& prj, const Au3TrackId& au3trackId);
    static const Au3Track* findTrackByIndex(const Au3Project& prj, size_t index);

    static Au3WaveTrack* findWaveTrack(Au3Project& prj, const Au3TrackId& au3trackId);
    static const Au3WaveTrack* findWaveTrack(const Au3Project& prj, const Au3TrackId& au3trackId);
    static const Au3WaveTrack* findWaveTrackByIndex(const Au3Project& prj, size_t index);

    static std::shared_ptr<Au3WaveClip> findWaveClip(Au3WaveTrack* track, int64_t au3ClipId);
    static std::shared_ptr<Au3WaveClip> findWaveClip(Au3WaveTrack* track, size_t index);
    static std::shared_ptr<Au3WaveClip> findWaveClip(Au3Project& prj, const Au3TrackId& au3trackId, size_t index);
    static std::shared_ptr<Au3WaveClip> findWaveClip(Au3Project& prj, const trackedit::TrackId& trackId, trackedit::secs_t time);

    static size_t findClipIndexById(const Au3WaveTrack* track, const trackedit::ClipId& clipId);

    static trackedit::ClipId findClipIdByIndex(const Au3WaveTrack* track, size_t clipIndex);
    static trackedit::ClipId findMatchedClip(const Au3WaveTrack* track, const Au3WaveTrack* originTrack,
                                             const trackedit::ClipId& originClipId);
    static std::list<std::shared_ptr<Au3WaveClip> > waveClipsAsList(Au3WaveTrack* track);

    static Au3LabelTrack* findLabelTrack(Au3Project& prj, const Au3LabelTrackId& au3LabelTrackId);
    static const Au3LabelTrack* findLabelTrack(const Au3Project& prj, const Au3LabelTrackId& au3LabelTrackId);
    static const Au3LabelTrack* findLabelTrackByIndex(const Au3Project& prj, size_t index);

    static Au3Label* findLabel(Au3LabelTrack* track, int64_t labelId);

    static trackedit::TrackIdList findSelectedTracks(Au3Project& prj);
    static trackedit::TrackId findFocusedTrack(Au3Project& prj);
    static void setTrackFocused(Au3Project& prj, trackedit::TrackId trackId, bool focused);
    static void clearAllTrackFocus(Au3Project& prj);

    static trackedit::ClipKeyList findSelectedClips(Au3Project& prj);
    static void setClipSelected(Au3Project& prj, trackedit::ClipKey clipKey, bool selected);
    static void clearAllClipSelection(Au3Project& prj);

    static trackedit::LabelKeyList findSelectedLabels(Au3Project& prj);
    static void setLabelSelected(Au3Project& prj, trackedit::LabelKey labelKey, bool selected);
    static void clearAllLabelSelection(Au3Project& prj);

    static int getTrackHeight(const Au3Track* track);
    static void setTrackHeight(Au3Track* track, int height);
};
}
