#include "domaccessor.h"

#include "domau3types.h"
#include "containers.h"

#include "log.h"

using namespace au::au3;
using namespace muse;

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

// TODO: if you need this one, fix it first (indexes are not in use anymore)
std::shared_ptr<WaveClip> DomAccessor::findWaveClip(AudacityProject& prj, const TrackId& au3trackId, size_t index)
{
    WaveTrack* t = findWaveTrack(prj, au3trackId);
    if (!t) {
        return nullptr;
    }
    return findWaveClip(t, index);
}

size_t DomAccessor::findClipIndexById(const WaveTrack *track, const trackedit::ClipId &clipId)
{
    size_t index = 0;
    for (const auto& interval : track->Intervals()) {
        if (WaveClipID(interval.get()).id == clipId) {
            return index;
        }
        index++;
    }

    return muse::nidx;
}

au::trackedit::ClipId DomAccessor::findClipIdByIndex(const WaveTrack *track, size_t clipIndex)
{
    auto it = std::next(track->Intervals().begin(), clipIndex);
    if (it != track->Intervals().end()) {
        return au::au3::WaveClipID((*it).get()).id;
    }
    return -1;
}

au::trackedit::ClipId DomAccessor::findMatchedClip(WaveTrack *track, const WaveTrack *originTrack, const trackedit::ClipId &originClipId)
{
    size_t idx = findClipIndexById(originTrack, originClipId);
    if (idx == muse::nidx) {
        return -1;
    }

    return DomAccessor::findClipIdByIndex(track, idx);
}

std::list<std::shared_ptr<WaveClip> > DomAccessor::waveClipsAsList(WaveTrack* track)
{
    std::list<std::shared_ptr<WaveClip> > clips = { track->Intervals().begin(), track->Intervals().end() };
    return clips;
}
