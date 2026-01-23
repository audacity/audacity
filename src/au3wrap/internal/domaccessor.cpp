#include "domaccessor.h"

#include "containers.h"

#include "log.h"

using namespace au::au3;
using namespace au::trackedit;
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
        i++;
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
        if (clip->Start() <= time && clip->End() > time) {
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

Au3LabelTrack* DomAccessor::findLabelTrack(Au3Project& prj, const Au3LabelTrackId& au3LabelTrackId)
{
    return const_cast<Au3LabelTrack*>(findLabelTrack(const_cast<const Au3Project&>(prj), au3LabelTrackId));
}

const Au3LabelTrack* DomAccessor::findLabelTrack(const Au3Project& prj, const Au3LabelTrackId& au3LabelTrackId)
{
    const Au3TrackList& tracks = Au3TrackList::Get(prj);
    for (auto lt : tracks.Any<const LabelTrack>()) {
        if (lt->GetId() == au3LabelTrackId) {
            return lt;
        }
    }

    return nullptr;
}

const Au3LabelTrack* DomAccessor::findLabelTrackByIndex(const Au3Project& prj, size_t index)
{
    const Au3TrackList& tracks = Au3TrackList::Get(prj);
    size_t i = 0;
    for (auto lt : tracks.Any<const LabelTrack>()) {
        if (i == index) {
            return lt;
        }
        ++i;
    }

    return nullptr;
}

Au3Label* DomAccessor::findLabel(Au3LabelTrack* track, int64_t labelId)
{
    IF_ASSERT_FAILED(track) {
        return nullptr;
    }

    return track->GetLabelById(labelId);
}

TrackIdList DomAccessor::findSelectedTracks(const Au3Project& prj)
{
    TrackIdList tracks;
    for (const Track* track : Au3TrackList::Get(prj).Selected<const Track>()) {
        tracks.push_back(track->GetId());
    }
    return tracks;
}

au::trackedit::TrackId DomAccessor::findFocusedTrack(const Au3Project& prj)
{
    const Au3TrackList& tracks = Au3TrackList::Get(prj);
    for (const Au3Track* track : tracks) {
        if (track->GetFocused()) {
            return track->GetId();
        }
    }
    return INVALID_TRACK;
}

void DomAccessor::setTrackFocused(Au3Project& prj, const au::trackedit::TrackId trackId, bool focused)
{
    auto track = findTrack(prj, ::TrackId { trackId });
    if (track) {
        track->SetFocused(focused);
    }
}

void DomAccessor::clearAllTrackFocus(Au3Project& prj)
{
    Au3TrackList& tracks = Au3TrackList::Get(prj);
    for (Au3Track* track : tracks) {
        track->SetFocused(false);
    }
}

ClipKeyList DomAccessor::findSelectedClips(const Au3Project& prj)
{
    ClipKeyList selectedClips;
    const Au3TrackList& tracks = Au3TrackList::Get(prj);

    for (const Au3Track* track : tracks) {
        const Au3WaveTrack* waveTrack = dynamic_cast<const Au3WaveTrack*>(track);
        if (!waveTrack) {
            continue;
        }

        for (const auto& clip : waveTrack->Intervals()) {
            if (clip->GetSelected()) {
                trackedit::ClipKey key;
                key.trackId = waveTrack->GetId();
                key.itemId = clip->GetId();
                selectedClips.push_back(key);
            }
        }
    }

    return selectedClips;
}

void DomAccessor::setClipSelected(Au3Project& prj, const ClipKey& clipKey, bool selected)
{
    auto track = findWaveTrack(prj, ::TrackId { clipKey.trackId });
    if (!track) {
        return;
    }

    auto clip = findWaveClip(track, clipKey.itemId);

    if (clip) {
        clip->SetSelected(selected);
    }
}

void DomAccessor::clearAllClipSelection(Au3Project& prj)
{
    Au3TrackList& tracks = Au3TrackList::Get(prj);

    for (Au3Track* track : tracks) {
        Au3WaveTrack* waveTrack = dynamic_cast<Au3WaveTrack*>(track);
        if (!waveTrack) {
            continue;
        }

        for (const auto& clip : waveTrack->Intervals()) {
            clip->SetSelected(false);
        }
    }
}

LabelKeyList DomAccessor::findSelectedLabels(const Au3Project& prj)
{
    LabelKeyList selectedLabels;
    const Au3TrackList& tracks = Au3TrackList::Get(prj);

    for (const Au3Track* track : tracks) {
        const Au3LabelTrack* labelTrack = dynamic_cast<const Au3LabelTrack*>(track);
        if (!labelTrack) {
            continue;
        }

        for (int i = 0; i < labelTrack->GetNumLabels(); ++i) {
            const LabelStruct* label = labelTrack->GetLabel(i);
            if (label && label->GetSelected()) {
                trackedit::LabelKey key;
                key.trackId = labelTrack->GetId();
                key.itemId = label->GetId();
                selectedLabels.push_back(key);
            }
        }
    }

    return selectedLabels;
}

void DomAccessor::setLabelSelected(Au3Project& prj, const LabelKey& labelKey, bool selected)
{
    auto track = findLabelTrack(prj, ::TrackId { labelKey.trackId });
    if (!track) {
        return;
    }

    Au3Label* label = findLabel(track, labelKey.itemId);

    if (label) {
        label->SetSelected(selected);
    }
}

void DomAccessor::clearAllLabelSelection(Au3Project& prj)
{
    Au3TrackList& tracks = Au3TrackList::Get(prj);

    for (Au3Track* track : tracks) {
        Au3LabelTrack* labelTrack = dynamic_cast<Au3LabelTrack*>(track);
        if (!labelTrack) {
            continue;
        }

        for (int i = 0; i < labelTrack->GetNumLabels(); ++i) {
            const LabelStruct* constLabel = labelTrack->GetLabel(i);
            if (constLabel) {
                LabelStruct* label = labelTrack->GetLabelById(constLabel->GetId());
                if (label) {
                    label->SetSelected(false);
                }
            }
        }
    }
}
