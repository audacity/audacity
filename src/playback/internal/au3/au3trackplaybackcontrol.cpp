#include "au3trackplaybackcontrol.h"

#include "WaveTrack.h"
#include "au3wrap/internal/domaccessor.h"

using namespace au::playback;
using namespace au::au3;

AudacityProject& Au3TrackPlaybackControl::projectRef() const
{
    AudacityProject* project = reinterpret_cast<AudacityProject*>(globalContext()->currentProject()->au3ProjectPtr());
    return *project;
}

volume_dbfs_t Au3TrackPlaybackControl::volume(long trackId)
{
    WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), ::TrackId(trackId));

    return LINEAR_TO_DB(track->GetVolume());
}

void Au3TrackPlaybackControl::setVolume(long trackId, volume_dbfs_t vol)
{
    WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), ::TrackId(trackId));

    track->SetVolume(vol > -60 ? DB_TO_LINEAR(vol) : 0);
    return;
}

balance_t Au3TrackPlaybackControl::balance(long trackId)
{
    WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), ::TrackId(trackId));

    return track->GetPan();
}

void Au3TrackPlaybackControl::setBalance(long trackId, balance_t balance)
{
    WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), ::TrackId(trackId));

    track->SetPan(balance);
    return;
}
