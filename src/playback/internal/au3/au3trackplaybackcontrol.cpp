#include "framework/global/log.h"
#include "global/types/translatablestring.h"

#include "au3wrap/internal/domaccessor.h"

#include "au3-wave-track/WaveTrack.h"

#include "au3trackplaybackcontrol.h"

using namespace au::playback;
using namespace au::au3;

Au3Project& Au3TrackPlaybackControl::projectRef() const
{
    Au3Project* project = reinterpret_cast<Au3Project*>(globalContext()->currentProject()->au3ProjectPtr());
    return *project;
}

volume_dbfs_t Au3TrackPlaybackControl::volume(long trackId) const
{
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    IF_ASSERT_FAILED(track) {
        return 0.0;
    }

    return LINEAR_TO_DB(track->GetVolume());
}

void Au3TrackPlaybackControl::setVolume(long trackId, volume_dbfs_t vol, bool completed)
{
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    IF_ASSERT_FAILED(track) {
        return;
    }

    track->SetVolume(vol > -60 ? DB_TO_LINEAR(vol) : 0);

    if (completed) {
        projectHistory()->pushHistoryState(muse::trc("playback", "Moved volume slider"),
                                           muse::trc("playback", "Volume"),
                                           trackedit::UndoPushType::CONSOLIDATE);
    }
    return;
}

pan_t Au3TrackPlaybackControl::pan(long trackId) const
{
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    IF_ASSERT_FAILED(track) {
        return 0.0;
    }

    return track->GetPan();
}

void Au3TrackPlaybackControl::setPan(long trackId, au::audio::pan_t pan, bool completed)
{
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    IF_ASSERT_FAILED(track) {
        return;
    }

    track->SetPan(pan);

    if (completed) {
        projectHistory()->pushHistoryState(muse::trc("playback", "Moved pan dial"),
                                           muse::trc("playback", "Pan"),
                                           trackedit::UndoPushType::CONSOLIDATE);
    }
}

void Au3TrackPlaybackControl::setMuteOrSolo(long trackId, bool value, MuteOrSolo which)
{
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    IF_ASSERT_FAILED(track) {
        return;
    }

    auto& tracks = TrackList::Get(projectRef());

    auto get = [which](auto* t) {
        return which == MuteOrSolo::Solo ? t->GetSolo() : t->GetMute();
    };
    auto set = [which](auto* t, bool v) {
        which == MuteOrSolo::Solo ? t->SetSolo(v) : t->SetMute(v);
    };

    const bool exclusiveSet = application()->keyboardModifiers().testFlag(Qt::ShiftModifier);

    bool changed = false;
    if (exclusiveSet) {
        value = true;

        for (auto playable : tracks.Any<PlayableTrack>().Excluding(track)) {
            if (get(playable)) {
                set(playable, false);
                changed = true;
            }
        }
    }

    if (get(track) != value) {
        set(track, value);
        changed = true;
    }

    if (!changed) {
        return;
    }

    for (auto playable : tracks.Any<PlayableTrack>()) {
        m_muteOrSoloChanged.send(playable->GetId());
    }

    projectHistory()->modifyState();
    projectHistory()->markUnsaved();
}

void Au3TrackPlaybackControl::setSolo(long trackId, bool solo)
{
    setMuteOrSolo(trackId, solo, MuteOrSolo::Solo);
}

bool Au3TrackPlaybackControl::solo(long trackId) const
{
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    if (!track) {
        return false;
    }

    return track->GetSolo();
}

void Au3TrackPlaybackControl::setMuted(long trackId, bool mute)
{
    setMuteOrSolo(trackId, mute, MuteOrSolo::Mute);
}

bool Au3TrackPlaybackControl::muted(long trackId) const
{
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    if (!track) {
        return false;
    }

    return track->GetMute();
}

muse::async::Channel<long> Au3TrackPlaybackControl::muteOrSoloChanged() const
{
    return m_muteOrSoloChanged;
}
