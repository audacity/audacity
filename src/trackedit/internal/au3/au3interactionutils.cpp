/*
 * Audacity: A Digital Audio Editor
 */
#include "au3interactionutils.h"
#include "../../trackedittypes.h"
#include "au3wrap/internal/domaccessor.h"
#include "libraries/lib-wave-track/WaveTrack.h"
#include "libraries/lib-wave-track/WaveClip.h"
#include "libraries/lib-project-rate/ProjectRate.h"
#include "libraries/lib-project-rate/QualitySettings.h"
#include "global/containers.h"
#include "log.h"
#include <unordered_set>

namespace {
std::vector<size_t> getEmptyWaveTrackIndices(const au::au3::Au3TrackList& tracks)
{
    using namespace au::au3;
    std::vector<size_t> indices;
    auto i = 0;
    for (const Au3Track* track : tracks) {
        const Au3WaveTrack* const waveTrack = dynamic_cast<const Au3WaveTrack*>(track);
        if (waveTrack && waveTrack->IsEmpty()) {
            indices.push_back(i);
        }
        ++i;
    }
    return indices;
}

au::trackedit::NeedsDownmixing addClipToTrack(const std::shared_ptr<au::au3::Au3WaveClip>& clip,
                                              au::au3::Au3WaveTrack& dstWaveTrack,
                                              au::au3::Au3TrackList& copy)
{
    using namespace au::trackedit::utils;

    const size_t dstTrackIndex = getTrackIndex(copy, dstWaveTrack);
    auto ptr = &dstWaveTrack;

    // All clips in movement were removed from `copy` before the move-clip iteration began,
    // so it's using `copy` as reference that we must check whether toggling the entire track is allowed.
    const auto emptyTrackIndices = getEmptyWaveTrackIndices(copy);
    if (muse::contains(emptyTrackIndices, dstTrackIndex) && dstWaveTrack.NChannels() != clip->NChannels()) {
        ptr = toggleStereo(copy, getTrackIndex(copy, dstWaveTrack));
    }

    ptr->InsertInterval(clip, false);

    if (clip->NChannels() == 2 && ptr->NChannels() == 1) {
        return au::trackedit::NeedsDownmixing::Yes;
    } else {
        return au::trackedit::NeedsDownmixing::No;
    }
}
}

au::au3::Au3WaveTrack* au::trackedit::utils::getWaveTrack(au3::Au3TrackList& tracks, const au3::Au3TrackId& trackId)
{
    return dynamic_cast<au3::Au3WaveTrack*>(tracks.FindById(trackId));
}

au::au3::Au3WaveTrack* au::trackedit::utils::getWaveTrack(au3::Au3TrackList& tracks, const ClipKey& clip)
{
    return getWaveTrack(tracks, au3::Au3TrackId { clip.trackId });
}

const au::au3::Au3WaveTrack* au::trackedit::utils::getWaveTrack(const au3::Au3TrackList& tracks, TrackIndex index)
{
    auto it = tracks.begin();
    std::advance(it, index.value);
    if (it == tracks.end()) {
        return nullptr;
    }
    return dynamic_cast<const au3::Au3WaveTrack*>(*it);
}

au::au3::Au3WaveTrack* au::trackedit::utils::getWaveTrack(au3::Au3TrackList& tracks, TrackIndex index)
{
    return const_cast<au3::Au3WaveTrack*>(getWaveTrack(const_cast<const au3::Au3TrackList&>(tracks), index));
}

size_t au::trackedit::utils::getTrackIndex(const au3::Au3TrackList& tracks, const au3::Au3Track& track)
{
    return std::distance(tracks.begin(), tracks.Find(&track));
}

size_t au::trackedit::utils::getTrackIndex(const au3::Au3TrackList& tracks, const trackedit::TrackId& id)
{
    return std::distance(tracks.begin(), tracks.Find(tracks.FindById(au3::Au3TrackId { id })));
}

void au::trackedit::utils::exchangeTrack(au3::Au3TrackList& tracks, au3::Au3WaveTrack& oldOne, au3::Au3WaveTrack& newOne)
{
    auto tmp = TrackList::Temporary(nullptr, newOne.shared_from_this());
    tracks.ReplaceOne(oldOne, std::move(*tmp));
}

au::au3::Au3WaveTrack* au::trackedit::utils::toggleStereo(au3::Au3TrackList& tracks, size_t trackIndex)
{
    au::au3::Au3WaveTrack* const track = getWaveTrack(tracks, TrackIndex { trackIndex });
    IF_ASSERT_FAILED(track) {
        return nullptr;
    }

    return toggleStereo(tracks, *track);
}

au::au3::Au3WaveTrack* au::trackedit::utils::toggleStereo(au3::Au3TrackList& tracks, au3::Au3WaveTrack& track)
{
    IF_ASSERT_FAILED(track.IsEmpty()) {
        return &track;
    }

    const auto replacement = std::static_pointer_cast<au3::Au3WaveTrack>(track.Duplicate(au3::Au3Track::DuplicateOptions {}.Backup()));
    const auto wasStereo = track.NChannels() == 2;
    if (wasStereo) {
        replacement->MakeMono();
    } else {
        replacement->MonoToStereo();
    }

    exchangeTrack(tracks, track, *replacement);

    return replacement.get();
}

/*!
 * @pre (trackFactory != nullptr && projectRate > 0) || tracks.GetOwner() != nullptr
 */
au::au3::Au3WaveTrack* au::trackedit::utils::appendWaveTrack(au3::Au3TrackList& tracks, size_t nChannels,
                                                             const ::WaveTrackFactory* trackFactory,
                                                             double projectRate)
{
    if (!trackFactory || projectRate == 0.) {
        const au3::Au3Project* project = tracks.GetOwner();
        IF_ASSERT_FAILED(project) {
            return nullptr;
        }
        trackFactory = &WaveTrackFactory::Get(*project);
        projectRate = ProjectRate::Get(*project).GetRate();
    }
    sampleFormat defaultFormat = QualitySettings::SampleFormatChoice();
    const au3::Au3WaveTrack::Holder track = trackFactory->Create(nChannels, defaultFormat, projectRate);
    track->SetName(tracks.MakeUniqueTrackName(au3::Au3WaveTrack::GetDefaultAudioTrackNamePreference()));
    tracks.Add(track);
    return track.get();
}

au::trackedit::NeedsDownmixing au::trackedit::utils::moveClipsVertically(VerticalDrag dragDirection, const au3::Au3TrackList& orig,
                                                                         au3::Au3TrackList& copy,
                                                                         const trackedit::ClipKeyList& selectedClips)
{
    const auto& project = *orig.GetOwner();
    const auto& factory = ::WaveTrackFactory::Get(project);
    const auto rate = ::ProjectRate::Get(project).GetRate();

    NeedsDownmixing needsDownmixing = NeedsDownmixing::No;

    struct MovedClip {
        MovedClip(const std::shared_ptr<au3::Au3WaveClip>& ptr, size_t origTrackIndex)
            : ptr(ptr)
            , origTrackIndex(origTrackIndex)
        {
        }

        const std::shared_ptr<au3::Au3WaveClip> ptr;
        const size_t origTrackIndex;
    };

    std::vector<MovedClip> movedClips;
    movedClips.reserve(selectedClips.size());
    for (const auto& selectedClip : selectedClips) {
        au3::Au3WaveTrack* srcWaveTrack = getWaveTrack(copy, selectedClip);
        IF_ASSERT_FAILED(srcWaveTrack) {
            continue;
        }
        const auto srcTrackIndex = getTrackIndex(copy, *srcWaveTrack);
        movedClips.emplace_back(au3::DomAccessor::findWaveClip(srcWaveTrack, selectedClip.clipId), srcTrackIndex);
        srcWaveTrack->RemoveInterval(movedClips.back().ptr);
    }

    for (const auto& clip : movedClips) {
        const au3::Au3WaveTrack* waveTrack = getWaveTrack(copy, TrackIndex { clip.origTrackIndex });
        IF_ASSERT_FAILED(waveTrack) {
            continue;
        }

        ::TrackList::iterator it = copy.begin();
        const auto dstTrackIndex = static_cast<int>(clip.origTrackIndex) + (dragDirection == VerticalDrag::Up ? -1 : 1);
        IF_ASSERT_FAILED(dstTrackIndex >= 0) {
            continue;
        }
        std::advance(it, dstTrackIndex);
        au3::Au3WaveTrack* const dstTrack = it != copy.end() ? dynamic_cast<au3::Au3WaveTrack*>(*it) : appendWaveTrack(copy,
                                                                                                                       waveTrack->NChannels(), &factory,
                                                                                                                       rate);

        needsDownmixing |= addClipToTrack(clip.ptr, *dstTrack, copy);
    }

    return needsDownmixing;
}

au::trackedit::TrackListInfo au::trackedit::utils::getTrackListInfo(const au3::Au3TrackList& tracks)
{
    std::vector<size_t> stereoTrackIndices;
    std::vector<size_t> emptyTrackIndices;
    auto i = 0;
    for (const au3::Au3Track* track : tracks) {
        const au3::Au3WaveTrack* waveTrack = dynamic_cast<const au3::Au3WaveTrack*>(track);
        if (waveTrack) {
            if (waveTrack->NChannels() == 2) {
                stereoTrackIndices.push_back(i);
            }
            if (waveTrack->IsEmpty()) {
                emptyTrackIndices.push_back(i);
            }
        }
        ++i;
    }
    return { tracks.Size(), std::move(stereoTrackIndices), std::move(emptyTrackIndices) };
}

bool au::trackedit::utils::clipIdSetsAreEqual(const au3::Au3WaveTrack& track1, const au3::Au3WaveTrack& track2)
{
    const auto numClips = track1.GetNumClips();
    if (numClips != track2.GetNumClips()) {
        return false;
    }
    std::unordered_set<au3::Au3ClipId> clipIds1;
    for (auto i = 0; i < numClips; ++i) {
        clipIds1.insert(track1.GetClip(i)->GetId());
    }
    for (auto i = 0; i < numClips; ++i) {
        if (!clipIds1.count(track2.GetClip(i)->GetId())) {
            return false;
        }
    }
    return true;
}

using ProgressCb = std::function<void (double)>;
using CancelCb = std::function<bool ()>;

muse::Ret au::trackedit::utils::withProgress(muse::IInteractive& interactive, const std::string& title, const std::function<bool(ProgressCb,
                                                                                                                                 CancelCb)>& action)
{
    muse::Progress progress;
    interactive.showProgress(title, &progress);
    progress.started();

    ProgressCb progressCb = [&](double progressFraction)
    {
        progress.progress(progressFraction * 1000, 1000, "");
        QCoreApplication::processEvents();
        return !progress.isCanceled();
    };

    CancelCb cancelCb = [&progress] { return progress.isCanceled(); };

    const bool result = action(std::move(progressCb), std::move(cancelCb));

    if (progress.isCanceled()) {
        return muse::make_ret(muse::Ret::Code::Cancel);
    }

    progress.finish(muse::make_ok());

    return result;
}
