#include "au3trackplaybackcontrol.h"

#include "WaveTrack.h"
#include "au3wrap/internal/domaccessor.h"

using namespace au::playback;
using namespace au::au3;

Au3Project& Au3TrackPlaybackControl::projectRef() const
{
    Au3Project* project = reinterpret_cast<Au3Project*>(globalContext()->currentProject()->au3ProjectPtr());
    return *project;
}

volume_dbfs_t Au3TrackPlaybackControl::volume(long trackId)
{
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    IF_ASSERT_FAILED(track) {
        return 0.0;
    }

    return LINEAR_TO_DB(track->GetVolume());
}

void Au3TrackPlaybackControl::setVolume(long trackId, volume_dbfs_t vol)
{
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    IF_ASSERT_FAILED(track) {
        return;
    }

    track->SetVolume(vol > -60 ? DB_TO_LINEAR(vol) : 0);
    return;
}

balance_t Au3TrackPlaybackControl::balance(long trackId)
{
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    IF_ASSERT_FAILED(track) {
        return 0.0;
    }

    return track->GetPan();
}

void Au3TrackPlaybackControl::setBalance(long trackId, balance_t balance)
{
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    IF_ASSERT_FAILED(track) {
        return;
    }

    track->SetPan(balance);
    return;
}
