#include "au3trackplaybackcontrol.h"

#include "WaveTrack.h"
#include "au3wrap/internal/domaccessor.h"
#include "global/types/translatablestring.h"

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

balance_t Au3TrackPlaybackControl::balance(long trackId) const
{
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    IF_ASSERT_FAILED(track) {
        return 0.0;
    }

    return track->GetPan();
}

void Au3TrackPlaybackControl::setBalance(long trackId, au::audio::balance_t balance, bool completed)
{
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    IF_ASSERT_FAILED(track) {
        return;
    }

    track->SetPan(balance);

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

    if (track->GetSolo() == solo) {
        return;
    }
    track->SetSolo(solo);
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
