#include "domaccessor.h"

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

std::shared_ptr<WaveClip> DomAccessor::findWaveClip(WaveTrack* track, size_t id)
{
    auto tracks = track->Intervals();
    for (auto it = tracks.begin(); it != tracks.end(); ++it) {
        if ((*it)->GetId() == id) {
            return *it;
        }
    }
    return std::shared_ptr<WaveClip>();
}

std::shared_ptr<WaveClip> DomAccessor::findWaveClip(AudacityProject& prj, const TrackId& au3trackId, size_t id)
{
    WaveTrack* t = findWaveTrack(prj, au3trackId);
    if (!t) {
        return nullptr;
    }
    return findWaveClip(t, id);
}
