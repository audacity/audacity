#include "domaccessor.h"

#include "domau3types.h"

#include "log.h"

using namespace au::au3;

WaveTrack* DomAccessor::findWaveTrack(AudacityProject& prj, const TrackId& au3trackId)
{
    Track* track = nullptr;
    TrackList& tracks = TrackList::Get(prj);
    for (Track* t : tracks) {
        if (t->GetId() == au3trackId) {
            track = t;
            break;
        }
    }

    return dynamic_cast<WaveTrack*>(track);
}

std::shared_ptr<WaveClip> DomAccessor::findWaveClip(WaveTrack* track, uint64_t au3ClipId)
{
    for (const std::shared_ptr<WaveClip>& interval : track->Intervals()) {
        if (WaveClipID(interval.get()).id == au3ClipId) {
            return interval;
        }
    }

    return nullptr;
}

std::shared_ptr<WaveClip> DomAccessor::findWaveClip(AudacityProject& prj, const TrackId& au3trackId, size_t index)
{
    WaveTrack* t = findWaveTrack(prj, au3trackId);
    if (!t) {
        return nullptr;
    }
    return findWaveClip(t, index);
}

std::list<std::shared_ptr<WaveClip> > DomAccessor::waveClipsAsList(WaveTrack* track)
{
    std::list<std::shared_ptr<WaveClip> > clips = { track->Intervals().begin(), track->Intervals().end() };
    return clips;
}
