#include "domaccessor.h"

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
