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

std::shared_ptr<WaveClip> DomAccessor::findWaveClip(WaveTrack* track, size_t index)
{
    auto clips = track->Intervals();
    IF_ASSERT_FAILED(index < clips.size()) {
        return nullptr;
    }

    auto it = track->Intervals().begin();
    std::advance(it, index);

    std::shared_ptr<WaveClip> clip = *it;
    return clip;
}
