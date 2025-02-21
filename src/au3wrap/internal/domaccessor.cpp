#include "domaccessor.h"

#include "containers.h"

#include "log.h"

using namespace au::au3;
using namespace muse;

Au3Track* DomAccessor::findTrack(Au3Project& prj, const Au3TrackId& au3trackId)
{
    return const_cast<Au3Track*>(findTrack(const_cast<const Au3Project&>(prj), au3trackId));
}

const Au3Track* DomAccessor::findTrack(const Au3Project& prj, const Au3TrackId& au3trackId)
{
    const Au3Track* track = nullptr;
    const Au3TrackList& tracks = Au3TrackList::Get(prj);
    for (const Au3Track* t : tracks) {
        if (t->GetId() == au3trackId) {
            track = t;
            break;
        }
    }

    return track;
}

const Au3Track* DomAccessor::findTrackByIndex(const Au3Project& prj, size_t index)
{
    const Au3Track* track = nullptr;
    const Au3TrackList& tracks = Au3TrackList::Get(prj);
    size_t i = 0;
    for (const Au3Track* t : tracks) {
        if (i == index) {
            track = t;
            break;
        }
    }

    return track;
}

Au3WaveTrack* DomAccessor::findWaveTrack(Au3Project& prj, const Au3TrackId& au3trackId)
{
    return const_cast<Au3WaveTrack*>(findWaveTrack(const_cast<const Au3Project&>(prj), au3trackId));
}

const Au3WaveTrack* DomAccessor::findWaveTrack(const Au3Project& prj, const Au3TrackId& au3trackId)
{
    return dynamic_cast<const Au3WaveTrack*>(findTrack(prj, au3trackId));
}

const Au3WaveTrack* DomAccessor::findWaveTrackByIndex(const Au3Project& prj, size_t index)
{
    return dynamic_cast<const Au3WaveTrack*>(findTrackByIndex(prj, index));
}

std::shared_ptr<Au3WaveClip> DomAccessor::findWaveClip(Au3WaveTrack* track, int64_t au3ClipId)
{
    for (const std::shared_ptr<Au3WaveClip>& interval : track->Intervals()) {
        if (interval->GetId() == au3ClipId) {
            return interval;
        }
    }

    return nullptr;
}

std::shared_ptr<Au3WaveClip> DomAccessor::findWaveClip(Au3WaveTrack* track, size_t index)
{
    int i = 0;
    for (const std::shared_ptr<Au3WaveClip>& interval : track->Intervals()) {
        if (i == index) {
            return interval;
        }
    }

    return nullptr;
}

// TODO: if you need this one, fix it first (indexes are not in use anymore)
std::shared_ptr<Au3WaveClip> DomAccessor::findWaveClip(Au3Project& prj, const Au3TrackId& au3trackId, size_t index)
{
    Au3WaveTrack* t = findWaveTrack(prj, au3trackId);
    if (!t) {
        return nullptr;
    }
    return findWaveClip(t, index);
}

std::shared_ptr<WaveClip> DomAccessor::findWaveClip(Au3Project& prj, const trackedit::TrackId& trackId, trackedit::secs_t time)
{
    TrackList& tracks = TrackList::Get(prj);
    WaveTrack* au3Track = dynamic_cast<WaveTrack*>(tracks.FindById(::TrackId(trackId)));

    for (const std::shared_ptr<WaveClip>& clip : au3Track->Intervals()) {
        if (clip->Start() <= time && clip->End() >= time) {
            return clip;
        }
    }

    return nullptr;
}

size_t DomAccessor::findClipIndexById(const Au3WaveTrack* track, const trackedit::ClipId& clipId)
{
    size_t index = 0;
    for (const auto& interval : track->Intervals()) {
        if (interval->GetId() == clipId) {
            return index;
        }
        index++;
    }

    return muse::nidx;
}

au::trackedit::ClipId DomAccessor::findClipIdByIndex(const Au3WaveTrack* track, size_t clipIndex)
{
    auto it = std::next(track->Intervals().begin(), clipIndex);
    if (it != track->Intervals().end()) {
        return (*it).get()->GetId();
    }
    return -1;
}

au::trackedit::ClipId DomAccessor::findMatchedClip(const Au3WaveTrack* track, const Au3WaveTrack* originTrack,
                                                   const trackedit::ClipId& originClipId)
{
    size_t idx = findClipIndexById(originTrack, originClipId);
    if (idx == muse::nidx) {
        return -1;
    }

    return DomAccessor::findClipIdByIndex(track, idx);
}

std::list<std::shared_ptr<Au3WaveClip> > DomAccessor::waveClipsAsList(Au3WaveTrack* track)
{
    std::list<std::shared_ptr<Au3WaveClip> > clips = { track->Intervals().begin(), track->Intervals().end() };
    return clips;
}
