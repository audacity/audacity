#include "global/types/translatablestring.h"

#include "au3wrap/internal/domaccessor.h"
#include "playbacktypes.h"

#include "WaveTrack.h"

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

void Au3TrackPlaybackControl::setSolo(long trackId, bool solo)
{
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    IF_ASSERT_FAILED(track) {
        return;
    }

    auto& tracks = TrackList::Get(projectRef());

    if (track->GetSolo() == solo) {
        return;
    }

    TracksBehaviors::SoloBehavior currentSoloBehavior = playbackConfiguration()->currentSoloBehavior();
    Qt::KeyboardModifiers modifiers = QApplication::keyboardModifiers();
    if (modifiers.testFlag(Qt::ShiftModifier)) {
        if (currentSoloBehavior == TracksBehaviors::SoloBehavior::SoloBehaviorMulti) {
            currentSoloBehavior = TracksBehaviors::SoloBehavior::SoloBehaviorSimple;
        } else {
            currentSoloBehavior = TracksBehaviors::SoloBehavior::SoloBehaviorMulti;
        }
    }

    if (currentSoloBehavior == TracksBehaviors::SoloBehavior::SoloBehaviorMulti) {
        track->SetSolo(solo);
    } else {
        for (auto playable : tracks.Any<PlayableTrack>()) {
            if (playable->GetId() == trackId) {
                playable->SetSolo(solo);
            } else {
                playable->SetSolo(false);
            }
            m_muteOrSoloChanged.send(playable->GetId());
        }
    }

    if (solo == track->GetMute()) {
        track->SetMute(false);
    }

    m_muteOrSoloChanged.send(trackId);
    projectHistory()->modifyState();
    projectHistory()->markUnsaved();
}

bool Au3TrackPlaybackControl::solo(long trackId) const
{
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    IF_ASSERT_FAILED(track) {
        return false;
    }

    return track->GetSolo();
}

void Au3TrackPlaybackControl::setMuted(long trackId, bool mute)
{
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    IF_ASSERT_FAILED(track) {
        return;
    }

    if (track->GetMute() == mute) {
        return;
    }
    track->SetMute(mute);
    if (mute && track->GetSolo()) {
        track->SetSolo(false);
    }

    m_muteOrSoloChanged.send(trackId);
    projectHistory()->modifyState();
    projectHistory()->markUnsaved();
}

bool Au3TrackPlaybackControl::muted(long trackId) const
{
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    IF_ASSERT_FAILED(track) {
        return false;
    }

    return track->GetMute();
}

muse::async::Channel<long> Au3TrackPlaybackControl::muteOrSoloChanged() const
{
    return m_muteOrSoloChanged;
}
