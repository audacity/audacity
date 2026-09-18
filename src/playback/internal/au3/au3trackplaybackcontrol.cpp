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
                                           //: Undo history entry name; shown after Undo and Redo in the Edit menu
                                           muse::trc("playback", "Pan"),
                                           trackedit::UndoPushType::CONSOLIDATE);
    }
}

bool Au3TrackPlaybackControl::setMuteOrSolo(long trackId, bool value, MuteOrSolo which, bool exclusive)
{
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    if (!track) {
        return false;
    }

    auto& tracks = TrackList::Get(projectRef());

    auto get = [which](auto* t) {
        return which == MuteOrSolo::Solo ? t->GetSolo() : t->GetMute();
    };
    auto set = [which](auto* t, bool v) {
        which == MuteOrSolo::Solo ? t->SetSolo(v) : t->SetMute(v);
    };

    bool changed = false;
    if (exclusive) {
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

    return changed;
}

void Au3TrackPlaybackControl::setSolo(long trackId, bool solo, bool exclusive)
{
    if (setMuteOrSolo(trackId, solo, MuteOrSolo::Solo, exclusive)) {
        onMuteOrSoloChanged();
    }
}

bool Au3TrackPlaybackControl::solo(long trackId) const
{
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    if (!track) {
        return false;
    }

    return track->GetSolo();
}

void Au3TrackPlaybackControl::setMuted(long trackId, bool mute, bool exclusive)
{
    if (setMuteOrSolo(trackId, mute, MuteOrSolo::Mute, exclusive)) {
        onMuteOrSoloChanged();
    }
}

void Au3TrackPlaybackControl::setMuted(const trackedit::TrackIdList& trackIds, bool mute)
{
    // Apply to every track first (no short-circuiting), then notify and modify the history once.
    bool changed = false;
    for (const trackedit::TrackId trackId : trackIds) {
        if (setMuteOrSolo(trackId, mute, MuteOrSolo::Mute, false)) {
            changed = true;
        }
    }

    if (changed) {
        onMuteOrSoloChanged();
    }
}

bool Au3TrackPlaybackControl::muted(long trackId) const
{
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    if (!track) {
        return false;
    }

    return track->GetMute();
}

void Au3TrackPlaybackControl::onMuteOrSoloChanged()
{
    for (const auto& playable : TrackList::Get(projectRef()).Any<PlayableTrack>()) {
        m_muteOrSoloChanged.send(playable->GetId());
    }

    projectHistory()->modifyState();
    projectHistory()->markUnsaved();
}

muse::async::Channel<long> Au3TrackPlaybackControl::muteOrSoloChanged() const
{
    return m_muteOrSoloChanged;
}
