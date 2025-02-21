#include "au3interaction.h"

#include <algorithm>

#include "ProjectRate.h"
#include "TempoChange.h"
#include "TimeWarper.h"
#include "QualitySettings.h"

#include "global/types/number.h"
#include "global/concurrency/concurrent.h"

#include "libraries/lib-project/Project.h"
#include "libraries/lib-wave-track/WaveTrack.h"
#include "libraries/lib-track/Track.h"
#include "libraries/lib-wave-track/WaveClip.h"
#include "libraries/lib-wave-track/TimeStretching.h"
#include "libraries/lib-wave-track/WaveTrackUtilities.h"

#include "au3wrap/internal/domaccessor.h"
#include "au3wrap/internal/domconverter.h"
#include "au3wrap/internal/wxtypes_convert.h"
#include "au3wrap/au3types.h"

#include "trackedit/dom/track.h"

#include "defer.h"
#include "log.h"
#include "trackediterrors.h"
#include "translation.h"

using namespace au::trackedit;
using namespace au::au3;

Au3Project& Au3Interaction::projectRef() const
{
    Au3Project* project = reinterpret_cast<Au3Project*>(globalContext()->currentProject()->au3ProjectPtr());
    return *project;
}

TrackIdList Au3Interaction::pasteIntoNewTracks(const std::vector<TrackData>& tracksData)
{
    auto& project = projectRef();
    auto& tracks = Au3TrackList::Get(project);
    auto prj = globalContext()->currentTrackeditProject();
    secs_t selectedStartTime = globalContext()->playbackState()->playbackPosition();

    TrackIdList tracksIdsPastedInto;

    Au3Track* pFirstNewTrack = NULL;
    for (auto data : tracksData) {
        auto pNewTrack = createNewTrackAndPaste(data.track, tracks, selectedStartTime);
        if (!pFirstNewTrack) {
            pFirstNewTrack = pNewTrack.get();
        }

        auto newTrack = DomConverter::track(pNewTrack.get());
        prj->notifyAboutTrackAdded(newTrack);
        for (const auto& clip : prj->clipList(pNewTrack->GetId())) {
            prj->notifyAboutClipAdded(clip);
        }

        tracksIdsPastedInto.push_back(newTrack.id);
    }

    pushProjectHistoryPasteState();

    return tracksIdsPastedInto;
}

std::shared_ptr<au::au3::Au3Track> Au3Interaction::createNewTrackAndPaste(std::shared_ptr<Au3Track> track, Au3TrackList& list, secs_t begin)
{
    auto& trackFactory = WaveTrackFactory::Get(projectRef());
    auto& pSampleBlockFactory = trackFactory.GetSampleBlockFactory();

    //! NOTE: using dynamic_cast directly because when using UndoManager
    //! project may not contain track with given ID anymore
    Au3WaveTrack* waveTrack = dynamic_cast<Au3WaveTrack*>(track.get());
    IF_ASSERT_FAILED(waveTrack) {
        return nullptr;
    }
    auto pFirstTrack = waveTrack->EmptyCopy(pSampleBlockFactory);
    list.Add(pFirstTrack->SharedPointer());
    pFirstTrack->Paste(begin, *track);
    return pFirstTrack->SharedPointer();
}

TrackIdList Au3Interaction::determineDestinationTracksIds(const std::vector<Track>& tracks, const TrackIdList& destinationTrackIds,
                                                          size_t clipboardTracksSize) const
{
    //! NOTE: determine tracks to which clipboard content will be pasted,
    //! depending on clipboard size and currently selected tracks

    if (destinationTrackIds.size() < clipboardTracksSize) {
        //! NOTE: not enough tracks selected, add more consecutively
        return expandDestinationTracks(tracks, destinationTrackIds, clipboardTracksSize);
    } else if (destinationTrackIds.size() > clipboardTracksSize) {
        //! NOTE: more tracks selected than needed, return sub-vector
        return TrackIdList(destinationTrackIds.begin(), destinationTrackIds.begin() + clipboardTracksSize);
    }

    //! NOTE: selected tracks size matches clipboard size
    return destinationTrackIds;
}

TrackIdList Au3Interaction::expandDestinationTracks(const std::vector<Track>& tracks, const TrackIdList& destinationTrackIds,
                                                    size_t clipboardTracksSize) const
{
    TrackIdList result = destinationTrackIds;
    bool collecting = false;

    for (const auto& track : tracks) {
        if (!collecting && track.id == destinationTrackIds.back()) {
            collecting = true;
            continue;
        }
        if (collecting) {
            result.push_back(track.id);
            if (result.size() >= clipboardTracksSize) {
                break;
            }
        }
    }

    //! NOTE: if insufficient tracks are available in `tracks`, result may still be shorter than clipboard size
    return result;
}

muse::Ret Au3Interaction::canPasteTrackData(const TrackIdList& dstTracksIds, const std::vector<TrackData>& clipsToPaste, secs_t begin) const
{
    IF_ASSERT_FAILED(dstTracksIds.size() <= clipsToPaste.size()) {
        return make_ret(trackedit::Err::NotEnoughDataInClipboard);
    }

    for (size_t i = 0; i < dstTracksIds.size(); ++i) {
        Au3WaveTrack* dstWaveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(dstTracksIds[i]));
        IF_ASSERT_FAILED(dstWaveTrack) {
            return make_ret(trackedit::Err::WaveTrackNotFound);
        }

        if (dstWaveTrack->NChannels() == 1 && clipboard()->trackData(i).track.get()->NChannels() == 2) {
            return make_ret(trackedit::Err::StereoClipIntoMonoTrack);
        }
    }

    return muse::make_ok();
}

Au3Interaction::Au3Interaction()
{
    m_progress = std::make_shared<muse::Progress>();
}

muse::Ret Au3Interaction::makeRoomForClip(const ClipKey& clipKey)
{
    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();

    WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return make_ret(trackedit::Err::WaveTrackNotFound);
    }

    std::shared_ptr<WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.clipId);
    IF_ASSERT_FAILED(clip) {
        return make_ret(trackedit::Err::ClipNotFound);
    }

    std::list<std::shared_ptr<WaveClip> > clips = DomAccessor::waveClipsAsList(waveTrack);

    clips.sort([](const std::shared_ptr<WaveClip>& c1, const std::shared_ptr<WaveClip>& c2) {
        return c1->GetPlayStartTime() < c2->GetPlayStartTime();
    });

    for (const auto& otherClip : clips) {
        if (clip == otherClip) {
            //! NOTE do not modify the clip the action was taken on
            continue;
        }

        trimOrDeleteOverlapping(waveTrack, clip->GetPlayStartTime(), clip->GetPlayEndTime(), otherClip);
    }

    return muse::make_ret(muse::Ret::Code::Ok);
}

muse::Ret Au3Interaction::makeRoomForClipsOnTracks(const std::vector<TrackId>& tracksIds, const std::vector<TrackData>& trackData,
                                                   secs_t begin)
{
    IF_ASSERT_FAILED(tracksIds.size() <= trackData.size()) {
        return make_ret(trackedit::Err::NotEnoughDataInClipboard);
    }

    for (size_t i = 0; i < tracksIds.size(); ++i) {
        WaveTrack* dstWaveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(tracksIds.at(i)));
        IF_ASSERT_FAILED(dstWaveTrack) {
            return make_ret(trackedit::Err::WaveTrackNotFound);
        }

        //! NOTE need to snap begin just like Paste() function do
        secs_t snappedBegin = dstWaveTrack->SnapToSample(begin);

        const WaveTrack* wt = dynamic_cast<const Au3WaveTrack*>(trackData.at(i).track.get());
        for (const auto& interval : wt->Intervals()) {
            auto ok = makeRoomForDataOnTrack(tracksIds.at(i),
                                             snappedBegin + interval->GetPlayStartTime(), snappedBegin + interval->GetPlayEndTime());
            if (!ok) {
                return make_ret(trackedit::Err::FailedToMakeRoomForClip);
            }
        }
    }

    return muse::make_ok();
}

muse::Ret Au3Interaction::makeRoomForDataOnTracks(const std::vector<TrackId>& tracksIds, const std::vector<TrackData>& trackData,
                                                  secs_t begin)
{
    IF_ASSERT_FAILED(tracksIds.size() <= trackData.size()) {
        return make_ret(trackedit::Err::NotEnoughDataInClipboard);
    }

    for (size_t i = 0; i < tracksIds.size(); ++i) {
        WaveTrack* dstWaveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(tracksIds.at(i)));
        IF_ASSERT_FAILED(dstWaveTrack) {
            return make_ret(trackedit::Err::WaveTrackNotFound);
        }

        //! NOTE need to snap begin just like Paste() function do
        secs_t snappedBegin = dstWaveTrack->SnapToSample(begin);
        secs_t insertDuration = trackData.at(i).track.get()->GetEndTime();

        auto ok = makeRoomForDataOnTrack(tracksIds.at(i), snappedBegin, snappedBegin + insertDuration);
        if (!ok) {
            return make_ret(trackedit::Err::FailedToMakeRoomForClip);
        }
    }

    return muse::make_ok();
}

muse::Ret Au3Interaction::makeRoomForDataOnTrack(const TrackId trackId, secs_t begin, secs_t end)
{
    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();

    WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return make_ret(trackedit::Err::WaveTrackNotFound);
    }

    std::list<std::shared_ptr<WaveClip> > clips = DomAccessor::waveClipsAsList(waveTrack);

    clips.sort([](const std::shared_ptr<WaveClip>& c1, const std::shared_ptr<WaveClip>& c2) {
        return c1->GetPlayStartTime() < c2->GetPlayStartTime();
    });

    for (const auto& otherClip : clips) {
        trimOrDeleteOverlapping(waveTrack, begin, end, otherClip);
    }

    return muse::make_ret(muse::Ret::Code::Ok);
}

void Au3Interaction::trimOrDeleteOverlapping(WaveTrack* waveTrack, secs_t begin, secs_t end, std::shared_ptr<WaveClip> otherClip)
{
    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();

    if (muse::RealIsEqualOrLess(begin, otherClip->GetPlayStartTime())
        && muse::RealIsEqualOrMore(end, otherClip->GetPlayEndTime())) {
        waveTrack->RemoveInterval(otherClip);
        prj->notifyAboutTrackChanged(DomConverter::track(waveTrack));

        return;
    }

    //! NOTE check if clip boundaries are within other clip
    if (!muse::RealIsEqualOrLess(begin, otherClip->GetPlayStartTime())
        && !muse::RealIsEqualOrMore(end, otherClip->GetPlayEndTime())) {
        secs_t otherClipStartTime = otherClip->GetPlayStartTime();
        secs_t otherClipEndTime = otherClip->GetPlayEndTime();

        auto leftClip = waveTrack->CopyClip(*otherClip, true);
        waveTrack->InsertInterval(std::move(leftClip), false);
        prj->notifyAboutTrackChanged(DomConverter::track(waveTrack));

        secs_t rightClipOverlap = (end - otherClip->GetPlayStartTime());
        otherClip->TrimLeft(rightClipOverlap);
        prj->notifyAboutClipChanged(DomConverter::clip(waveTrack, otherClip.get()));

        leftClip->SetPlayStartTime(otherClipStartTime);
        secs_t leftClipOverlap = (otherClipEndTime - begin);
        leftClip->TrimRight(leftClipOverlap);
        prj->notifyAboutClipChanged(DomConverter::clip(waveTrack, leftClip.get()));

        prj->notifyAboutTrackChanged(DomConverter::track(waveTrack));

        return;
    }

    //! NOTE check if clip hovers left side of the clip
    if (muse::RealIsEqualOrLess(begin, otherClip->GetPlayStartTime())
        && !muse::RealIsEqualOrMore(end, otherClip->GetPlayEndTime())
        && muse::RealIsEqualOrMore(end, otherClip->GetPlayStartTime())) {
        secs_t overlap = (end - otherClip->GetPlayStartTime());
        otherClip->TrimLeft(overlap);
        prj->notifyAboutClipChanged(DomConverter::clip(waveTrack, otherClip.get()));

        return;
    }

    //! NOTE check if clip hovers right side of the clip
    if (!muse::RealIsEqualOrLess(begin, otherClip->GetPlayStartTime())
        && muse::RealIsEqualOrLess(begin, otherClip->GetPlayEndTime())
        && muse::RealIsEqualOrMore(end, otherClip->GetPlayEndTime())) {
        secs_t overlap = (otherClip->GetPlayEndTime() - begin);
        otherClip->TrimRight(overlap);
        prj->notifyAboutClipChanged(DomConverter::clip(waveTrack, otherClip.get()));

        return;
    }
}

std::optional<secs_t> Au3Interaction::shortestClipDuration(const ClipKeyList& clipKeys) const
{
    std::optional<secs_t> shortestClipDuration;
    for (const auto& selectedClip : clipKeys) {
        Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(selectedClip.trackId));
        IF_ASSERT_FAILED(waveTrack) {
            continue;
        }

        std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, selectedClip.clipId);
        IF_ASSERT_FAILED(clip) {
            continue;
        }
        if (!shortestClipDuration.has_value() || !muse::RealIsEqualOrMore(clip->GetPlayDuration(), shortestClipDuration.value())) {
            shortestClipDuration = clip->GetPlayDuration();
        }
    }

    return shortestClipDuration;
}

bool Au3Interaction::anyLeftFullyUntrimmed(const ClipKeyList& clipKeys) const
{
    for (const auto& selectedClip : clipKeys) {
        Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(selectedClip.trackId));
        IF_ASSERT_FAILED(waveTrack) {
            continue;
        }

        std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, selectedClip.clipId);
        IF_ASSERT_FAILED(clip) {
            continue;
        }
        if (muse::RealIsEqualOrLess(clip->GetTrimLeft(), 0.0)) {
            return true;
        }
    }

    return false;
}

bool Au3Interaction::anyRightFullyUntrimmed(const ClipKeyList& clipKeys) const
{
    for (const auto& selectedClip : clipKeys) {
        Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(selectedClip.trackId));
        IF_ASSERT_FAILED(waveTrack) {
            continue;
        }

        std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, selectedClip.clipId);
        IF_ASSERT_FAILED(clip) {
            continue;
        }
        if (muse::RealIsEqualOrLess(clip->GetTrimRight(), 0.0)) {
            return true;
        }
    }

    return false;
}

ClipKeyList Au3Interaction::determineClipsForInteraction(const ClipKey& clipKey) const
{
    if (!muse::contains(selectionController()->selectedClips(), clipKey)) {
        //! NOTE: hover handle single clip trim
        return ClipKeyList{ clipKey };
    } else {
        return selectionController()->selectedClips();
    }
}

secs_t Au3Interaction::clampLeftTrimDelta(const ClipKeyList& clipKeys,
                                          secs_t deltaSec,
                                          secs_t minClipDuration) const
{
    if (clipKeys.size() == 1) {
        //! NOTE hover handle single clip trim
        ClipKey selectedClip = clipKeys.front();
        Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(selectedClip.trackId));
        IF_ASSERT_FAILED(waveTrack) {
            return 0.0;
        }

        std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, selectedClip.clipId);
        IF_ASSERT_FAILED(clip) {
            return 0.0;
        }

        if (muse::RealIsEqualOrLess(deltaSec, 0.0) && muse::is_equal(clip->GetTrimLeft(), 0.0)) {
            //! NOTE: clip is fully untrimmed
            return 0.0;
        }
    } else {
        //! NOTE: handle multi-clip trim
        //! NOTE: check if trim delta is applicable to every clip
        std::optional<secs_t> duration = shortestClipDuration(clipKeys);

        if (!duration.has_value()) {
            return 0.0;
        }

        if (!muse::RealIsEqualOrMore(duration.value() - deltaSec, minClipDuration)) {
            return duration.value() - minClipDuration;
        }

        if (!muse::RealIsEqualOrLess(deltaSec, duration.value())
            || (muse::RealIsEqualOrLess(deltaSec, 0.0) && anyLeftFullyUntrimmed(clipKeys))) {
            return 0.0;
        }

        //! NOTE: check that no clip in selection extends beyond its track start
        std::optional<secs_t> leftmostClipStartTime = getLeftmostClipStartTime(selectionController()->selectedClips());

        if (leftmostClipStartTime.has_value()) {
            if (muse::RealIsEqualOrLess(leftmostClipStartTime.value() + deltaSec, 0.0)) {
                return 0.0;
            }
        }
    }

    return deltaSec;
}

secs_t Au3Interaction::clampRightTrimDelta(const ClipKeyList& clipKeys,
                                           secs_t deltaSec,
                                           secs_t minClipDuration) const
{
    if (clipKeys.size() == 1) {
        //! NOTE hover handle single clip trim
        ClipKey selectedClip = clipKeys.front();
        Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(selectedClip.trackId));
        IF_ASSERT_FAILED(waveTrack) {
            return 0.0;
        }

        std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, selectedClip.clipId);
        IF_ASSERT_FAILED(clip) {
            return 0.0;
        }

        if (muse::RealIsEqualOrLess(deltaSec, 0.0) && muse::is_equal(clip->GetTrimRight(), 0.0)) {
            //! NOTE: clip is fully untrimmed
            return 0.0;
        }
    } else {
        //! NOTE: handle multi-clip trim
        //! NOTE: check if trim delta is applicable to every clip
        std::optional<secs_t> duration = shortestClipDuration(clipKeys);

        if (!duration.has_value()) {
            return 0.0;
        }

        if (!muse::RealIsEqualOrMore(duration.value() - deltaSec, minClipDuration)) {
            return duration.value() - minClipDuration;
        }

        if (!muse::RealIsEqualOrLess(deltaSec, duration.value())
            || (muse::RealIsEqualOrLess(deltaSec, 0.0) && anyRightFullyUntrimmed(clipKeys))) {
            return 0.0;
        }
    }

    return deltaSec;
}

secs_t Au3Interaction::clampLeftStretchDelta(const ClipKeyList& clipKeys,
                                             secs_t deltaSec,
                                             secs_t minClipDuration) const
{
    //! NOTE: check if stretch delta is applicable to every clip
    std::optional<secs_t> duration = shortestClipDuration(clipKeys);

    if (!duration.has_value()) {
        return 0.0;
    }

    if (!muse::RealIsEqualOrMore(duration.value() - deltaSec, minClipDuration)) {
        return duration.value() - minClipDuration;
    }

    return deltaSec;
}

secs_t Au3Interaction::clampRightStretchDelta(const ClipKeyList& clipKeys,
                                              secs_t deltaSec,
                                              secs_t minClipDuration) const
{
    //! NOTE: check if stretch delta is applicable to every clip
    std::optional<secs_t> duration = shortestClipDuration(clipKeys);

    if (!duration.has_value()) {
        return 0.0;
    }

    if (!muse::RealIsEqualOrMore(duration.value() - deltaSec, minClipDuration)) {
        return duration.value() - minClipDuration;
    }

    return deltaSec;
}

bool Au3Interaction::trimClipsLeft(const ClipKeyList& clipKeys, secs_t deltaSec, bool completed)
{
    for (const auto& selectedClip : clipKeys) {
        Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(selectedClip.trackId));
        IF_ASSERT_FAILED(waveTrack) {
            return false;
        }

        std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, selectedClip.clipId);
        IF_ASSERT_FAILED(clip) {
            return false;
        }

        if (completed) {
            auto ok = makeRoomForClip(selectedClip);
            if (!ok) {
                return false;
            }
        }

        clip->TrimLeft(deltaSec);

        trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
        prj->notifyAboutClipChanged(DomConverter::clip(waveTrack, clip.get()));
    }

    return true;
}

bool Au3Interaction::trimClipsRight(const ClipKeyList& clipKeys, secs_t deltaSec, bool completed)
{
    for (const auto& selectedClip : clipKeys) {
        Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(selectedClip.trackId));
        IF_ASSERT_FAILED(waveTrack) {
            return false;
        }

        std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, selectedClip.clipId);
        IF_ASSERT_FAILED(clip) {
            return false;
        }

        if (completed) {
            auto ok = makeRoomForClip(selectedClip);
            if (!ok) {
                make_ret(trackedit::Err::FailedToMakeRoomForClip);
            }
        }

        clip->TrimRight(deltaSec);

        trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
        prj->notifyAboutClipChanged(DomConverter::clip(waveTrack, clip.get()));
    }

    return true;
}

bool Au3Interaction::stretchClipsLeft(const ClipKeyList& clipKeys, secs_t deltaSec, bool completed)
{
    for (const auto& selectedClip : clipKeys) {
        Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(selectedClip.trackId));
        IF_ASSERT_FAILED(waveTrack) {
            return false;
        }

        std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, selectedClip.clipId);
        IF_ASSERT_FAILED(clip) {
            return false;
        }

        if (completed) {
            auto ok = makeRoomForClip(selectedClip);
            if (!ok) {
                return false;
            }
        }

        secs_t newStart = clip->GetPlayStartTime() + deltaSec;
        clip->StretchLeftTo(newStart);

        trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
        prj->notifyAboutClipChanged(DomConverter::clip(waveTrack, clip.get()));
    }

    return true;
}

bool Au3Interaction::stretchClipsRight(const ClipKeyList& clipKeys, secs_t deltaSec, bool completed)
{
    for (const auto& selectedClip : clipKeys) {
        Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(selectedClip.trackId));
        IF_ASSERT_FAILED(waveTrack) {
            return false;
        }

        std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, selectedClip.clipId);
        IF_ASSERT_FAILED(clip) {
            return false;
        }

        if (completed) {
            auto ok = makeRoomForClip(selectedClip);
            if (!ok) {
                make_ret(trackedit::Err::FailedToMakeRoomForClip);
            }
        }

        secs_t newEnd = clip->GetPlayEndTime() - deltaSec;
        clip->StretchRightTo(newEnd);

        trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
        prj->notifyAboutClipChanged(DomConverter::clip(waveTrack, clip.get()));
    }

    return true;
}

muse::secs_t Au3Interaction::clipStartTime(const trackedit::ClipKey& clipKey) const
{
    Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return -1.0;
    }

    std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.clipId);
    IF_ASSERT_FAILED(clip) {
        return -1.0;
    }

    return clip->Start();
}

bool Au3Interaction::changeClipStartTime(const trackedit::ClipKey& clipKey, secs_t newStartTime, bool completed)
{
    Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.clipId);
    IF_ASSERT_FAILED(clip) {
        return false;
    }

    if (completed) {
        auto ok = makeRoomForClip(clipKey);
        if (!ok) {
            return false;
        }
    }

    if (!muse::RealIsEqualOrMore(newStartTime, 0.0)) {
        newStartTime = 0.0;
    }

    //! TODO Not sure what this method needs to be called to change the position, will need to clarify
    clip->SetPlayStartTime(newStartTime);
    // LOGD() << "changed PlayStartTime of track: " << clipKey.trackId
    //        << " clip: " << clipKey.index
    //        << " new PlayStartTime: " << newStartTime;

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->notifyAboutClipChanged(DomConverter::clip(waveTrack, clip.get()));

    m_clipStartTimeChanged.send(clipKey, newStartTime, completed);

    return true;
}

bool Au3Interaction::moveClipToTrack(const ClipKey& clipKey, TrackId trackId, bool completed)
{
    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();

    Au3WaveTrack* srcWaveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(srcWaveTrack) {
        return false;
    }

    std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(srcWaveTrack, clipKey.clipId);
    IF_ASSERT_FAILED(clip) {
        return false;
    }

    Au3WaveTrack* dstWaveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    IF_ASSERT_FAILED(dstWaveTrack) {
        return false;
    }

    //! NOTE: detach clip from the source track
    srcWaveTrack->RemoveInterval(clip);
    selectionController()->removeClipSelection(clipKey);
    prj->notifyAboutClipRemoved(DomConverter::clip(srcWaveTrack, clip.get()));

    //! NOTE: insert clip to the destination track
    dstWaveTrack->InsertInterval(clip, false);
    ClipKey newClipKey = clipKey;
    newClipKey.trackId = dstWaveTrack->GetId();
    selectionController()->addSelectedClip(newClipKey);
    prj->notifyAboutClipAdded(DomConverter::clip(dstWaveTrack, clip.get()));

    return true;
}

muse::async::Channel<au::trackedit::ClipKey, secs_t /*newStartTime*/, bool /*completed*/>
Au3Interaction::clipStartTimeChanged() const
{
    return m_clipStartTimeChanged;
}

bool Au3Interaction::trimTracksData(const std::vector<TrackId>& tracksIds, secs_t begin, secs_t end)
{
    for (TrackId trackId : tracksIds) {
        Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
        IF_ASSERT_FAILED(waveTrack) {
            continue;
        }

        waveTrack->Trim(begin, end);

        trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
        prj->notifyAboutTrackChanged(DomConverter::track(waveTrack));
    }

    pushProjectHistoryTracksTrimState(begin, end);

    return true;
}

bool Au3Interaction::silenceTracksData(const std::vector<trackedit::TrackId>& tracksIds, secs_t begin, secs_t end)
{
    for (TrackId trackId : tracksIds) {
        Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
        IF_ASSERT_FAILED(waveTrack) {
            return false;
        }
        waveTrack->Silence(begin, end, {});

        trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
        prj->notifyAboutTrackChanged(DomConverter::track(waveTrack));
    }

    pushProjectHistoryTrackSilenceState(begin, end);

    return true;
}

bool Au3Interaction::changeTrackTitle(const TrackId trackId, const muse::String& title)
{
    Au3Track* track = DomAccessor::findTrack(projectRef(), Au3TrackId(trackId));
    IF_ASSERT_FAILED(track) {
        return false;
    }

    track->SetName(wxFromString(title));
    LOGD() << "changed name of track: " << trackId;

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->notifyAboutTrackChanged(DomConverter::track(track));

    return true;
}

bool Au3Interaction::changeClipTitle(const trackedit::ClipKey& clipKey, const muse::String& newTitle)
{
    Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.clipId);
    IF_ASSERT_FAILED(clip) {
        return false;
    }

    clip->SetName(wxFromString(newTitle));
    LOGD() << "changed name of clip: " << clipKey.clipId << ", track: " << clipKey.trackId;

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->notifyAboutClipChanged(DomConverter::clip(waveTrack, clip.get()));

    return true;
}

bool Au3Interaction::changeClipPitch(const ClipKey& clipKey, int pitch)
{
    WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    std::shared_ptr<WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.clipId);
    IF_ASSERT_FAILED(clip) {
        return false;
    }

    clip->SetCentShift(pitch);
    LOGD() << "changed pitch of clip: " << clipKey.clipId << ", track: " << clipKey.trackId << ", pitch: " << pitch;

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->notifyAboutClipChanged(DomConverter::clip(waveTrack, clip.get()));

    pushProjectHistoryChangeClipPitchState();

    return true;
}

bool Au3Interaction::resetClipPitch(const ClipKey& clipKey)
{
    WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    std::shared_ptr<WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.clipId);
    IF_ASSERT_FAILED(clip) {
        return false;
    }

    clip->SetCentShift(0);
    LOGD() << "reseted pitch of clip: " << clipKey.clipId << ", track: " << clipKey.trackId;

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->notifyAboutClipChanged(DomConverter::clip(waveTrack, clip.get()));

    pushProjectHistoryResetClipPitchState();

    return true;
}

bool Au3Interaction::changeClipSpeed(const ClipKey& clipKey, double speed)
{
    bool ok = doChangeClipSpeed(clipKey, speed);

    if (ok) {
        pushProjectHistoryChangeClipSpeedState();
    }

    return ok;
}

bool Au3Interaction::resetClipSpeed(const ClipKey& clipKey)
{
    bool ok = doChangeClipSpeed(clipKey, 1);

    if (ok) {
        pushProjectHistoryResetClipSpeedState();
    }

    return ok;
}

bool Au3Interaction::changeClipColor(const ClipKey& clipKey, const std::string& newColor)
{
    WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    std::shared_ptr<WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.clipId);
    IF_ASSERT_FAILED(clip) {
        return false;
    }

    clip->SetColor(newColor);

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->notifyAboutClipChanged(DomConverter::clip(waveTrack, clip.get()));

    return true;
}

bool Au3Interaction::changeClipOptimizeForVoice(const ClipKey& clipKey, bool optimize)
{
    WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    std::shared_ptr<WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.clipId);
    IF_ASSERT_FAILED(clip) {
        return false;
    }

    clip->SetPitchAndSpeedPreset(optimize ? PitchAndSpeedPreset::OptimizeForVoice : PitchAndSpeedPreset::Default);
    LOGD() << "changed optimize for voice of clip: " << clipKey.clipId << ", track: " << clipKey.trackId << ", optimize: " << optimize;

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->notifyAboutClipChanged(DomConverter::clip(waveTrack, clip.get()));

    return true;
}

bool Au3Interaction::renderClipPitchAndSpeed(const ClipKey& clipKey)
{
    muse::Concurrent::run([this, clipKey]() {
        m_progress->start();

        muse::ProgressResult result;

        DEFER {
            m_progress->finish(result);
        };

        WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(clipKey.trackId));
        IF_ASSERT_FAILED(waveTrack) {
            return;
        }

        std::shared_ptr<WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.clipId);
        IF_ASSERT_FAILED(clip) {
            return;
        }

        auto progressCallBack = [this](double progressFraction) {
            m_progress->progress(progressFraction * 1000, 1000, "");
        };

        waveTrack->ApplyPitchAndSpeed({ { clip->GetPlayStartTime(), clip->GetPlayEndTime() } }, progressCallBack);
        LOGD() << "apply pitch and speed for clip: " << clipKey.clipId << ", track: " << clipKey.trackId;

        trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
        prj->notifyAboutTrackChanged(DomConverter::track(waveTrack)); //! todo: replase with onClipChanged

        pushProjectHistoryRenderClipStretchingState();
    });

    return true;
}

void Au3Interaction::clearClipboard()
{
    clipboard()->clearTrackData();
}

muse::Ret Au3Interaction::pasteFromClipboard(secs_t begin)
{
    if (clipboard()->trackDataEmpty()) {
        return make_ret(trackedit::Err::TrackEmpty);
    }

    const auto newGroupId = determineGroupId({});
    auto copiedData = clipboard()->trackDataCopy(newGroupId);

    TrackIdList selectedTracks = selectionController()->selectedTracks();
    if (selectedTracks.empty()) {
        auto tracksIdsToSelect = pasteIntoNewTracks(copiedData);
        selectionController()->setSelectedTracks(tracksIdsToSelect);
        return muse::make_ok();
    }

    project::IAudacityProjectPtr project = globalContext()->currentProject();
    //! TODO: we need to make sure that we get a trackList with order
    //! the same as in the TrackPanel
    auto tracks = project->trackeditProject()->trackList();
    size_t clipboardTracksSize = clipboard()->trackDataSize();

    TrackIdList dstTracksIds = determineDestinationTracksIds(tracks, selectedTracks, clipboardTracksSize);

    bool newTracksNeeded = false;
    if (dstTracksIds.size() != clipboardTracksSize) {
        newTracksNeeded = true;
    }

    auto ret = canPasteTrackData(dstTracksIds, copiedData, begin);
    if (!ret) {
        return ret;
    }

    muse::Ret ok;
    if (clipboard()->isMultiSelectionCopy()) {
        ok = makeRoomForClipsOnTracks(dstTracksIds, copiedData, begin);
    } else {
        ok = makeRoomForDataOnTracks(dstTracksIds, copiedData, begin);
    }
    if (!ok) {
        make_ret(trackedit::Err::FailedToMakeRoomForClip);
    }

    for (size_t i = 0; i < dstTracksIds.size(); ++i) {
        Au3WaveTrack* dstWaveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(dstTracksIds[i]));
        IF_ASSERT_FAILED(dstWaveTrack) {
            return make_ret(trackedit::Err::WaveTrackNotFound);
        }
        auto prj = globalContext()->currentTrackeditProject();

        // use an unordered_set to store the IDs of the clips before the paste
        std::unordered_set<int> clipIdsBefore;
        for (const auto& clip : prj->clipList(dstTracksIds[i])) {
            clipIdsBefore.insert(clip.key.clipId);
        }

        const auto trackToPaste = std::static_pointer_cast<Au3WaveTrack>(copiedData.at(i).track);

        // When the source is mono, may paste its only channel
        // repeatedly into a stereo track
        if (trackToPaste->NChannels() == 1 && dstWaveTrack->NChannels() == 2) {
            trackToPaste->MonoToStereo();
        }

        if (clipboard()->isMultiSelectionCopy()) {
            trackToPaste->MoveTo(begin + trackToPaste->GetStartTime());
            for (const auto& interval : trackToPaste->Intervals()) {
                dstWaveTrack->InsertInterval(interval, false);
            }
        } else {
            dstWaveTrack->Paste(begin, *trackToPaste);
        }

        // Check which clips were added and trigger the onClipAdded event
        for (const auto& clip : prj->clipList(dstTracksIds[i])) {
            if (clipIdsBefore.find(clip.key.clipId) == clipIdsBefore.end()) {
                prj->notifyAboutClipAdded(clip);
            }
        }
    }

    if (newTracksNeeded) {
        // remove already pasted elements from the clipboard and paste the rest into the new tracks
        copiedData.erase(copiedData.begin(), copiedData.begin() + dstTracksIds.size());
        auto tracksIdsToSelect = pasteIntoNewTracks(copiedData);
        dstTracksIds.insert(dstTracksIds.end(), tracksIdsToSelect.begin(), tracksIdsToSelect.end());
    }

    selectionController()->setSelectedTracks(dstTracksIds);

    pushProjectHistoryPasteState();

    return muse::make_ok();
}

bool Au3Interaction::cutClipIntoClipboard(const ClipKey& clipKey)
{
    Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.clipId);
    IF_ASSERT_FAILED(clip) {
        return false;
    }

    auto track = waveTrack->Cut(clip->Start(), clip->End());
    clipboard()->addTrackData(TrackData { track, clipKey });

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->notifyAboutClipRemoved(DomConverter::clip(waveTrack, clip.get()));
    projectHistory()->pushHistoryState("Cut to the clipboard", "Cut");

    return true;
}

bool Au3Interaction::cutClipDataIntoClipboard(const TrackIdList& tracksIds, secs_t begin, secs_t end)
{
    for (const auto& trackId : tracksIds) {
        bool ok = cutTrackDataIntoClipboard(trackId, begin, end);
        if (!ok) {
            return false;
        }
    }

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    projectHistory()->pushHistoryState("Cut to the clipboard", "Cut");

    return true;
}

bool Au3Interaction::cutTrackDataIntoClipboard(const TrackId trackId, secs_t begin, secs_t end)
{
    Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    auto track = waveTrack->Cut(begin, end);
    trackedit::ClipKey dummyClipKey = trackedit::ClipKey();
    clipboard()->addTrackData(TrackData { track, dummyClipKey });

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->notifyAboutTrackChanged(DomConverter::track(waveTrack));

    return true;
}

bool Au3Interaction::copyClipIntoClipboard(const ClipKey& clipKey)
{
    Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.clipId);
    IF_ASSERT_FAILED(clip) {
        return false;
    }

    auto track = waveTrack->Copy(clip->Start(), clip->End());
    clipboard()->addTrackData(TrackData { track, clipKey });

    return true;
}

bool Au3Interaction::copyClipDataIntoClipboard(const ClipKey& clipKey, secs_t begin, secs_t end)
{
    Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.clipId);
    IF_ASSERT_FAILED(clip) {
        return false;
    }

    auto track = waveTrack->Copy(begin, end);
    clipboard()->addTrackData(TrackData { track, clipKey });

    return true;
}

bool Au3Interaction::copyNonContinuousTrackDataIntoClipboard(const TrackId trackId, const ClipKeyList& clipKeys, secs_t offset)
{
    Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    auto& trackFactory = WaveTrackFactory::Get(projectRef());
    auto& pSampleBlockFactory = trackFactory.GetSampleBlockFactory();
    auto clipboardTrack = waveTrack->EmptyCopy(pSampleBlockFactory);

    std::vector<std::shared_ptr<Au3WaveClip> > intervals;
    for (const auto& clipKey : clipKeys) {
        std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.clipId);
        IF_ASSERT_FAILED(clip) {
            return false;
        }

        clipboardTrack->InsertInterval(waveTrack->CopyClip(*clip, true), false);
    }

    for (const auto& clip : clipboardTrack->SortedIntervalArray()) {
        clip->SetPlayStartTime(clip->GetPlayStartTime() + offset);
    }

    trackedit::ClipKey dummyClipKey = trackedit::ClipKey();
    clipboard()->addTrackData(TrackData { clipboardTrack, dummyClipKey });
    clipboard()->setMultiSelectionCopy(true);

    return true;
}

bool Au3Interaction::copyContinuousTrackDataIntoClipboard(const TrackId trackId, secs_t begin, secs_t end)
{
    Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    auto track = waveTrack->Copy(begin, end);
    trackedit::ClipKey dummyClipKey = trackedit::ClipKey();
    clipboard()->addTrackData(TrackData { track, dummyClipKey });

    return true;
}

bool Au3Interaction::removeClip(const trackedit::ClipKey& clipKey)
{
    Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.clipId);
    IF_ASSERT_FAILED(clip) {
        return false;
    }

    secs_t start = clip->Start();
    secs_t end = clip->End();
    secs_t duration = end - start;

    waveTrack->Clear(clip->Start(), clip->End());

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->notifyAboutTrackChanged(DomConverter::track(waveTrack));

    pushProjectHistoryDeleteState(start, duration);

    return true;
}

bool Au3Interaction::removeClips(const ClipKeyList& clipKeyList)
{
    if (clipKeyList.empty()) {
        return false;
    }

    for (const auto& clipKey : clipKeyList) {
        Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(clipKey.trackId));
        IF_ASSERT_FAILED(waveTrack) {
            return false;
        }

        std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.clipId);
        IF_ASSERT_FAILED(clip) {
            return false;
        }

        waveTrack->Clear(clip->Start(), clip->End());

        trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
        prj->notifyAboutTrackChanged(DomConverter::track(waveTrack));
    }

    pushProjectHistoryDeleteMultipleState();

    return true;
}

bool Au3Interaction::removeTracksData(const TrackIdList& tracksIds, secs_t begin, secs_t end)
{
    secs_t duration = end - begin;
    secs_t start = begin;

    for (const TrackId& trackId : tracksIds) {
        Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
        IF_ASSERT_FAILED(waveTrack) {
            continue;
        }

        waveTrack->Clear(begin, end);

        trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
        prj->notifyAboutTrackChanged(DomConverter::track(waveTrack));
    }

    pushProjectHistoryDeleteState(start, duration);

    return true;
}

bool Au3Interaction::moveClips(secs_t timePositionOffset, int trackPositionOffset, bool completed)
{
    //! NOTE: cannot start moving until previous move is handled
    if (m_busy) {
        return false;
    }
    m_busy = true;

    //! NOTE: check if offset is applicable to every clip and recalculate if needed
    std::optional<secs_t> leftmostClipStartTime = getLeftmostClipStartTime(selectionController()->selectedClips());

    if (leftmostClipStartTime.has_value()) {
        if (muse::RealIsEqualOrLess(leftmostClipStartTime.value() + timePositionOffset, 0.0)) {
            timePositionOffset = -leftmostClipStartTime.value();
        }
    }

    int boundary = trackPositionOffsetMin();
    trackPositionOffset = std::max(trackPositionOffset, boundary);
    bool canMoveClips = (trackPositionOffset != 0) ? canMoveClipsToTrack(trackPositionOffset)
                        : false;

    for (const auto& selectedClip : selectionController()->selectedClipsInTrackOrder()) {
        Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(selectedClip.trackId));
        IF_ASSERT_FAILED(waveTrack) {
            continue;
        }

        std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, selectedClip.clipId);
        IF_ASSERT_FAILED(clip) {
            continue;
        }

        changeClipStartTime(selectedClip, clip->GetPlayStartTime() + timePositionOffset, completed);

        if (trackPositionOffset == 0 || !canMoveClips) {
            continue;
        }

        //! NOTE: trackIdList may change on every loop iteration
        trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
        auto trackIds = prj->trackIdList();

        int destinationTrackIdx = -1.0;
        TrackId dstTrackId = -1.0;
        for (int i = 0; i < trackIds.size(); ++i) {
            if (trackIds.at(i) == selectedClip.trackId) {
                destinationTrackIdx = i + trackPositionOffset;
                if (destinationTrackIdx >= trackIds.size()) {
                    if (waveTrack->NChannels() == 1) {
                        createMonoTrack();
                    } else {
                        createStereoTrack();
                    }

                    dstTrackId = *prj->trackIdList().rbegin();
                } else {
                    dstTrackId = trackIds.at(destinationTrackIdx);
                }
                break;
            }
        }

        if (dstTrackId != -1.0) {
            moveClipToTrack(selectedClip, dstTrackId, completed);
        }
    }

    if (completed) {
        //! TODO AU4: later when having keyboard arrow shortcut for moving clips
        //! make use of UndoPush::CONSOLIDATE arg in UndoManager
        projectHistory()->pushHistoryState("Clip moved", "Move clip");
    }

    m_busy = false;

    return !(trackPositionOffset == 0 || !canMoveClips);
}

bool Au3Interaction::splitTracksAt(const TrackIdList& tracksIds, secs_t pivot)
{
    for (const auto& trackId : tracksIds) {
        Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
        IF_ASSERT_FAILED(waveTrack) {
            return false;
        }

        waveTrack->SplitAt(pivot);

        trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
        prj->notifyAboutTrackChanged(DomConverter::track(waveTrack));
    }

    projectHistory()->pushHistoryState("Split", "Split");

    return true;
}

bool Au3Interaction::mergeSelectedOnTrack(const TrackId trackId, secs_t begin, secs_t end)
{
    Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    //! TODO fix this so it displays progress if there's
    //! a need to change pitch/speed
    ProgressReporter dummyProgressReporter;
    waveTrack->Join(begin, end, dummyProgressReporter);

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->notifyAboutTrackChanged(DomConverter::track(waveTrack));

    return true;
}

bool Au3Interaction::duplicateSelectedOnTrack(const TrackId trackId, secs_t begin, secs_t end)
{
    auto& tracks = Au3TrackList::Get(projectRef());
    Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    auto dest = waveTrack->Copy(begin, end, false);
    dest->MoveTo(std::max(static_cast<double>(begin), waveTrack->GetStartTime()));
    tracks.Add(dest);

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->notifyAboutTrackAdded(DomConverter::track(dest.get()));

    return true;
}

bool Au3Interaction::splitCutSelectedOnTrack(const TrackId trackId, secs_t begin, secs_t end)
{
    Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    auto track = waveTrack->SplitCut(begin, end);
    trackedit::ClipKey dummyClipKey = trackedit::ClipKey();
    clipboard()->addTrackData(TrackData { track, dummyClipKey });

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->notifyAboutTrackChanged(DomConverter::track(waveTrack));

    return true;
}

bool Au3Interaction::splitDeleteSelectedOnTrack(const TrackId trackId, secs_t begin, secs_t end)
{
    Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    waveTrack->SplitDelete(begin, end);

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->notifyAboutTrackChanged(DomConverter::track(waveTrack));

    return true;
}

bool Au3Interaction::mergeSelectedOnTracks(const TrackIdList& tracksIds, secs_t begin, secs_t end)
{
    secs_t duration = end - begin;

    for (const auto& trackId : tracksIds) {
        bool ok = mergeSelectedOnTrack(trackId, begin, end);
        if (!ok) {
            return false;
        }
    }

    pushProjectHistoryJoinState(begin, duration);

    return true;
}

bool Au3Interaction::duplicateSelectedOnTracks(const TrackIdList& tracksIds, secs_t begin, secs_t end)
{
    for (const auto& trackId : tracksIds) {
        bool ok = duplicateSelectedOnTrack(trackId, begin, end);
        if (!ok) {
            return false;
        }
    }

    pushProjectHistoryDuplicateState();

    return true;
}

bool Au3Interaction::duplicateClip(const ClipKey& clipKey)
{
    return duplicateClips({ clipKey });
}

bool Au3Interaction::duplicateClips(const ClipKeyList& clipKeyList)
{
    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return false;
    }

    //Get list of tracks in order by the UI position
    trackedit::TrackIdList tracks = prj->trackIdList();

    //Get a set of the tracks we want copy content from
    std::set<TrackId> selectedTracks;
    std::transform(clipKeyList.begin(), clipKeyList.end(), std::inserter(selectedTracks,
                                                                         selectedTracks.begin()), [](const ClipKey& clipKey) {
        return clipKey.trackId;
    });
    if (selectedTracks.empty()) {
        return false;
    }

    //Get only the selected tracks but keeping the UI order
    std::vector<Au3WaveTrack*> waveTracks;
    for (auto trackId : tracks) {
        if (selectedTracks.find(trackId) != selectedTracks.end()) {
            auto waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
            if (waveTrack) {
                waveTracks.push_back(waveTrack);
            }
        }
    }

    auto& projectTracks = Au3TrackList::Get(projectRef());
    auto& trackFactory = WaveTrackFactory::Get(projectRef());
    auto& pSampleBlockFactory = trackFactory.GetSampleBlockFactory();

    for (const auto& track : waveTracks) {
        auto newTrack = track->EmptyCopy(pSampleBlockFactory);

        std::vector<ClipKey> clipsToDuplicate;
        std::copy_if(clipKeyList.begin(), clipKeyList.end(), std::back_inserter(clipsToDuplicate), [track](const ClipKey& clipKey) {
            return clipKey.trackId == track->GetId();
        });

        for (const auto& clipKey : clipsToDuplicate) {
            std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(track, clipKey.clipId);

            IF_ASSERT_FAILED(clip) {
                continue;
            }

            newTrack->InsertInterval(track->CopyClip(*clip, true), false);
        }
        projectTracks.Add(newTrack);
        prj->notifyAboutTrackAdded(DomConverter::track(newTrack.get()));
    }

    pushProjectHistoryDuplicateState();

    return true;
}

bool Au3Interaction::clipSplitCut(const ClipKey& clipKey)
{
    Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.clipId);
    IF_ASSERT_FAILED(clip) {
        return false;
    }

    auto track = waveTrack->SplitCut(clip->Start(), clip->End());
    trackedit::ClipKey dummyClipKey = trackedit::ClipKey();
    clipboard()->addTrackData(TrackData { track, dummyClipKey });

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->notifyAboutTrackChanged(DomConverter::track(waveTrack));

    projectHistory()->pushHistoryState("Split-cut to the clipboard", "Split cut");

    return true;
}

bool Au3Interaction::clipSplitDelete(const ClipKey& clipKey)
{
    Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.clipId);
    IF_ASSERT_FAILED(clip) {
        return false;
    }

    waveTrack->SplitDelete(clip->Start(), clip->End());

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->notifyAboutTrackChanged(DomConverter::track(waveTrack));

    pushProjectHistorySplitDeleteState(clip->Start(), clip->End() - clip->Start());

    return true;
}

bool Au3Interaction::splitCutSelectedOnTracks(const TrackIdList tracksIds, secs_t begin, secs_t end)
{
    for (const auto& trackId : tracksIds) {
        bool ok = splitCutSelectedOnTrack(trackId, begin, end);
        if (!ok) {
            return false;
        }
    }

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    projectHistory()->pushHistoryState("Split-cut to the clipboard", "Split cut");

    return true;
}

bool Au3Interaction::splitDeleteSelectedOnTracks(const TrackIdList tracksIds, secs_t begin, secs_t end)
{
    secs_t duration = end - begin;

    for (const auto& trackId : tracksIds) {
        bool ok = splitDeleteSelectedOnTrack(trackId, begin, end);
        if (!ok) {
            return false;
        }
    }

    pushProjectHistorySplitDeleteState(begin, duration);

    return true;
}

bool Au3Interaction::trimClipLeft(const ClipKey& clipKey, secs_t deltaSec, secs_t minClipDuration, bool completed)
{
    //! NOTE: other clips must follow if selected
    ClipKeyList clips = determineClipsForInteraction(clipKey);

    secs_t adjustedDelta = clampLeftTrimDelta(clips, deltaSec, minClipDuration);

    //! NOTE: don't be tempted to early return if delta is 0.0 or by any other reason:
    //! we still need to trigger cannibalistic clip behaviour and save project state
    //! to the history so this function has to execute till the end
    bool result = trimClipsLeft(clips, adjustedDelta, completed);
    if (!result) {
        return false;
    }

    if (completed) {
        projectHistory()->pushHistoryState("Clip trimmed", "Trim clip");
    }

    return true;
}

bool Au3Interaction::trimClipRight(const ClipKey& clipKey, secs_t deltaSec, secs_t minClipDuration, bool completed)
{
    //! NOTE: other clips must follow if selected
    ClipKeyList clips = determineClipsForInteraction(clipKey);
    secs_t adjustedDelta = clampRightTrimDelta(clips, deltaSec, minClipDuration);

    //! NOTE: don't be tempted to early return if delta is 0.0 or by any other reason:
    //! we still need to trigger cannibalistic clip behaviour and save project state
    //! to the history so this function has to execute till the end
    bool result = trimClipsRight(clips, adjustedDelta, completed);
    if (!result) {
        return false;
    }

    if (completed) {
        projectHistory()->pushHistoryState("Clip trimmed", "Trim clip");
    }

    return true;
}

bool Au3Interaction::stretchClipLeft(const ClipKey& clipKey,
                                     secs_t deltaSec,
                                     secs_t minClipDuration,
                                     bool completed)
{
    //! NOTE: other clips must follow if selected
    ClipKeyList clips = determineClipsForInteraction(clipKey);

    secs_t adjustedDelta = clampLeftStretchDelta(clips, deltaSec, minClipDuration);

    //! NOTE: don't be tempted to early return if delta is 0.0 or by any other reason:
    //! we still need to trigger cannibalistic clip behaviour and save project state
    //! to the history so this function has to execute till the end
    bool result = stretchClipsLeft(clips, adjustedDelta, completed);
    if (!result) {
        return false;
    }

    if (completed) {
        projectHistory()->pushHistoryState("Clip stretched", "Stretch clip");
    }

    return true;
}

bool Au3Interaction::stretchClipRight(const ClipKey& clipKey,
                                      secs_t deltaSec,
                                      secs_t minClipDuration,
                                      bool completed)
{
    //! NOTE: other clips must follow if selected
    ClipKeyList clips = determineClipsForInteraction(clipKey);
    secs_t adjustedDelta = clampRightStretchDelta(clips, deltaSec, minClipDuration);

    //! NOTE: don't be tempted to early return if delta is 0.0 or by any other reason:
    //! we still need to trigger cannibalistic clip behaviour and save project state
    //! to the history so this function has to execute till the end
    bool result = stretchClipsRight(clips, adjustedDelta, completed);
    if (!result) {
        return false;
    }

    if (completed) {
        projectHistory()->pushHistoryState("Clip stretched", "Stretch clip");
    }

    return true;
}

bool Au3Interaction::newMonoTrack()
{
    auto track = createMonoTrack();

    selectionController()->setSelectedTracks({ track->GetId() });

    pushProjectHistoryTrackAddedState();

    return true;
}

bool Au3Interaction::newStereoTrack()
{
    auto track = createStereoTrack();

    selectionController()->setSelectedTracks({ track->GetId() });

    pushProjectHistoryTrackAddedState();

    return true;
}

bool Au3Interaction::newLabelTrack()
{
    NOT_IMPLEMENTED;
    return false;
}

bool Au3Interaction::deleteTracks(const TrackIdList& trackIds)
{
    auto& project = projectRef();
    auto& tracks = Au3TrackList::Get(project);

    for (const auto& trackId : trackIds) {
        Au3Track* au3Track = DomAccessor::findTrack(project, Au3TrackId(trackId));
        IF_ASSERT_FAILED(au3Track) {
            continue;
        }
        auto track = DomConverter::track(au3Track);

        tracks.Remove(*au3Track);

        trackedit::ITrackeditProjectPtr trackEdit = globalContext()->currentTrackeditProject();
        trackEdit->notifyAboutTrackRemoved(track);
    }

    projectHistory()->pushHistoryState("Delete track", "Delete track");

    return true;
}

bool Au3Interaction::duplicateTracks(const TrackIdList& trackIds)
{
    auto& project = projectRef();
    auto& tracks = Au3TrackList::Get(project);

    for (const auto& trackId : trackIds) {
        Au3Track* au3Track = DomAccessor::findTrack(project, Au3TrackId(trackId));

        IF_ASSERT_FAILED(au3Track) {
            continue;
        }

        auto au3Clone = au3Track->Duplicate();
        Au3TrackList::AssignUniqueId(au3Clone);

        tracks.Add(au3Clone, ::TrackList::DoAssignId::Yes);

        auto clone = DomConverter::track(au3Clone.get());

        trackedit::ITrackeditProjectPtr trackEdit = globalContext()->currentTrackeditProject();
        trackEdit->notifyAboutTrackInserted(clone, tracks.Size());
    }
    projectHistory()->pushHistoryState("Duplicate track", "Duplicate track");

    return true;
}

void Au3Interaction::moveTracks(const TrackIdList& trackIds, const TrackMoveDirection direction)
{
    if (trackIds.empty()) {
        return;
    }

    TrackIdList sortedTrackIds = trackIds;
    auto isAscending = (direction == TrackMoveDirection::Up || direction == TrackMoveDirection::Bottom);
    std::sort(sortedTrackIds.begin(), sortedTrackIds.end(), [this, isAscending](const TrackId& a, const TrackId& b) {
        return isAscending ? trackPosition(a) < trackPosition(b) : trackPosition(a) > trackPosition(b);
    });

    auto canMoveWithoutPushing = [this, direction, &sortedTrackIds](const TrackId trackId) {
        int currentPos = trackPosition(trackId);
        int targetPos = (direction == TrackMoveDirection::Up) ? currentPos - 1 : currentPos + 1;

        for (const auto& id : sortedTrackIds) {
            if (trackPosition(id) == targetPos) {
                return false;
            }
        }
        return canMoveTrack(trackId, direction);
    };

    for (const auto& trackId : sortedTrackIds) {
        if (direction == TrackMoveDirection::Top || direction == TrackMoveDirection::Bottom || canMoveWithoutPushing(trackId)) {
            moveTrack(trackId, direction);
        }
    }
    projectHistory()->pushHistoryState("Move track", "Move track");
}

void Au3Interaction::moveTracksTo(const TrackIdList& trackIds, int to)
{
    if (trackIds.empty()) {
        return;
    }

    TrackIdList sortedTrackIds = trackIds;
    auto isAscending = (to > trackPosition(trackIds.front()));
    std::sort(sortedTrackIds.begin(), sortedTrackIds.end(), [this, isAscending](const TrackId& a, const TrackId& b) {
        return isAscending ? trackPosition(a) < trackPosition(b) : trackPosition(a) > trackPosition(b);
    });

    for (const auto& trackId : sortedTrackIds) {
        moveTrackTo(trackId, to);
    }

    projectHistory()->pushHistoryState("Move track", "Move track");
}

bool Au3Interaction::insertSilence(const TrackIdList& trackIds, secs_t begin, secs_t end, secs_t duration)
{
    if (trackIds.empty()) {
        const auto prj = globalContext()->currentTrackeditProject();
        auto& tracks = Au3TrackList::Get(projectRef());
        auto& trackFactory = ::WaveTrackFactory::Get(projectRef());

        sampleFormat defaultFormat = QualitySettings::SampleFormatChoice();
        auto rate = ::ProjectRate::Get(projectRef()).GetRate();

        auto track = trackFactory.Create(defaultFormat, rate);
        track->SetName(tracks.MakeUniqueTrackName(Au3WaveTrack::GetDefaultAudioTrackNamePreference()));
        tracks.Add(track, ::TrackList::DoAssignId::Yes,
                   ::TrackList::EventPublicationSynchrony::Synchronous);
        prj->notifyAboutTrackAdded(DomConverter::track(track.get()));
        doInsertSilence({ track->GetId() }, begin, end, duration);
    } else {
        doInsertSilence(trackIds, begin, end, duration);
    }

    projectHistory()->pushHistoryState(muse::trc("trackedit", "Insert silence"), muse::trc("trackedit", "Insert silence"));

    return true;
}

void Au3Interaction::doInsertSilence(const TrackIdList& trackIds, secs_t begin, secs_t end, secs_t duration)
{
    const auto prj = globalContext()->currentTrackeditProject();

    for (const auto& trackId : trackIds) {
        Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));

        IF_ASSERT_FAILED(waveTrack) {
            continue;
        }

        if (!muse::is_zero(duration)) {
            PasteTimeWarper warper{ end, begin + duration };

            auto copy = waveTrack->EmptyCopy();

            copy->InsertSilence(0.0, duration);
            copy->Flush();
            bool preserveSplits = true;
            bool mergeExtraSplits = true;
            waveTrack->ClearAndPaste(begin, end, *copy, preserveSplits, mergeExtraSplits, &warper);
            waveTrack->Flush();
        } else {
            // If the duration is zero, there's no need to actually
            // generate anything
            waveTrack->Clear(begin, end);
        }

        prj->notifyAboutTrackChanged(DomConverter::track(waveTrack));
    }
}

std::shared_ptr<WaveTrack> Au3Interaction::createMonoTrack()
{
    auto& project = projectRef();
    auto& tracks = Au3TrackList::Get(project);
    auto& trackFactory = ::WaveTrackFactory::Get(project);

    sampleFormat defaultFormat = QualitySettings::SampleFormatChoice();
    auto rate = ::ProjectRate::Get(project).GetRate();

    auto track = trackFactory.Create(defaultFormat, rate);
    track->SetName(tracks.MakeUniqueTrackName(Au3WaveTrack::GetDefaultAudioTrackNamePreference()));

    tracks.Add(track);

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->notifyAboutTrackAdded(DomConverter::track(track.get()));

    return track;
}

std::shared_ptr<WaveTrack> Au3Interaction::createStereoTrack()
{
    auto& project = projectRef();
    auto& tracks = Au3TrackList::Get(project);
    auto& trackFactory = ::WaveTrackFactory::Get(project);

    sampleFormat defaultFormat = QualitySettings::SampleFormatChoice();
    auto rate = ::ProjectRate::Get(project).GetRate();

    auto waveTrack = trackFactory.Create(2, defaultFormat, rate);
    tracks.Add(waveTrack);
    auto& newTrack = **tracks.rbegin();
    newTrack.SetName(tracks.MakeUniqueTrackName(Au3WaveTrack::GetDefaultAudioTrackNamePreference()));

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    auto track = *tracks.rbegin();
    prj->notifyAboutTrackAdded(DomConverter::track(track));

    return waveTrack;
}

bool Au3Interaction::canMoveTrack(const TrackId trackId, const TrackMoveDirection direction)
{
    auto& project = projectRef();
    auto& tracks = ::TrackList::Get(project);
    Au3Track* au3Track = DomAccessor::findTrack(project, Au3TrackId(trackId));

    switch (direction) {
    case TrackMoveDirection::Up:
    case TrackMoveDirection::Top:
        return tracks.CanMoveUp(*au3Track);
    case TrackMoveDirection::Down:
    case TrackMoveDirection::Bottom:
        return tracks.CanMoveDown(*au3Track);
    }

    return false;
}

int Au3Interaction::trackPosition(const TrackId trackId)
{
    auto& project = projectRef();
    auto& tracks = ::TrackList::Get(project);
    Au3Track* au3Track = DomAccessor::findTrack(project, Au3TrackId(trackId));

    return std::distance(tracks.begin(), tracks.Find(au3Track));
}

void Au3Interaction::moveTrackTo(const TrackId trackId, int to)
{
    auto& project = projectRef();
    auto& tracks = ::TrackList::Get(project);
    Au3Track* au3Track = DomAccessor::findTrack(project, Au3TrackId(trackId));

    IF_ASSERT_FAILED(au3Track) {
        return;
    }

    int from = std::distance(tracks.begin(), tracks.Find(au3Track));

    if (to == from) {
        return;
    }

    int pos = from;
    if (pos < to) {
        while (pos != to && tracks.CanMoveDown(*au3Track)) {
            pos++;
            tracks.MoveDown(*au3Track);
        }
    } else {
        while (pos != to && tracks.CanMoveUp(*au3Track)) {
            pos--;
            tracks.MoveUp(*au3Track);
        }
    }

    if (pos != to) {
        LOGW("Can't move track from position %d to %d, track was moved to position %d", from, to, pos);
    }

    auto track = DomConverter::track(au3Track);

    trackedit::ITrackeditProjectPtr trackEdit = globalContext()->currentTrackeditProject();
    trackEdit->notifyAboutTrackMoved(track, pos);
}

int Au3Interaction::trackPositionOffsetMin() const
{
    auto selectedTracks = selectionController()->selectedTracks();

    if (selectedTracks.size() == 0) {
        return 0;
    }

    int minOffset = 0;
    auto& tracks = ::TrackList::Get(projectRef());

    for (auto it = tracks.begin(); it != tracks.end(); ++it) {
        const auto& track = *it;
        if (muse::contains(selectedTracks, TrackId(track->GetId()))) {
            break;
        } else {
            minOffset--;
        }
    }

    return minOffset;
}

bool Au3Interaction::canMoveClipsToTrack(int trackPositionOffset) const
{
    if (trackPositionOffset == 0) {
        return false;
    }

    for (const auto& selectedClip : selectionController()->selectedClipsInTrackOrder()) {
        Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(selectedClip.trackId));
        IF_ASSERT_FAILED(waveTrack) {
            continue;
        }

        std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, selectedClip.clipId);
        IF_ASSERT_FAILED(clip) {
            continue;
        }

        trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
        auto trackIds = prj->trackIdList();

        int destinationTrackIdx = -1.0;
        TrackId dstTrackId = -1.0;
        for (int i = 0; i < trackIds.size(); ++i) {
            if (trackIds.at(i) == selectedClip.trackId) {
                destinationTrackIdx = i + trackPositionOffset;
                if (destinationTrackIdx >= trackIds.size()) {
                    return true;
                } else {
                    dstTrackId = trackIds.at(destinationTrackIdx);

                    Au3WaveTrack* dstWaveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(dstTrackId));
                    IF_ASSERT_FAILED(waveTrack) {
                        continue;
                    }

                    if (dstWaveTrack->NChannels() != waveTrack->NChannels()) {
                        return false;
                    }
                }
                break;
            }
        }
    }
    return true;
}

std::optional<secs_t> Au3Interaction::getLeftmostClipStartTime(const ClipKeyList& clipKeys) const
{
    std::optional<secs_t> leftmostClipStartTime;
    for (const auto& selectedClip : clipKeys) {
        Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(selectedClip.trackId));
        IF_ASSERT_FAILED(waveTrack) {
            continue;
        }

        std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, selectedClip.clipId);
        IF_ASSERT_FAILED(clip) {
            continue;
        }

        if (clip->GetPlayStartTime() < leftmostClipStartTime || !leftmostClipStartTime.has_value()) {
            leftmostClipStartTime = clip->GetPlayStartTime();
        }
    }

    return leftmostClipStartTime;
}

void Au3Interaction::moveTrack(const TrackId trackId, const TrackMoveDirection direction)
{
    auto& project = projectRef();
    auto& tracks = Au3TrackList::Get(project);
    Au3Track* au3Track = DomAccessor::findTrack(project, Au3TrackId(trackId));

    IF_ASSERT_FAILED(au3Track) {
        return;
    }

    int initialPosition = std::distance(tracks.begin(), tracks.Find(au3Track));

    IF_ASSERT_FAILED(initialPosition >= 0 && initialPosition < static_cast<int>(tracks.Size())) {
        return;
    }

    int targetPosition = initialPosition;

    switch (direction) {
    case TrackMoveDirection::Up:
        if (!tracks.CanMoveUp(*au3Track)) {
            break;
        }
        targetPosition--;
        tracks.MoveUp(*au3Track);
        break;
    case TrackMoveDirection::Down:
        if (!tracks.CanMoveDown(*au3Track)) {
            break;
        }
        targetPosition++;
        tracks.MoveDown(*au3Track);
        break;
    case TrackMoveDirection::Top:
        while (tracks.CanMoveUp(*au3Track)) {
            targetPosition--;
            tracks.MoveUp(*au3Track);
        }
        break;
    case TrackMoveDirection::Bottom:
        while (tracks.CanMoveDown(*au3Track)) {
            targetPosition++;
            tracks.MoveDown(*au3Track);
        }
        break;
    default:
        return;
    }

    if (initialPosition == targetPosition) {
        LOGW() << "Can't move track to " << &direction;
        return;
    }

    auto track = DomConverter::track(au3Track);

    trackedit::ITrackeditProjectPtr trackEdit = globalContext()->currentTrackeditProject();
    trackEdit->notifyAboutTrackMoved(track, targetPosition);
}

muse::secs_t Au3Interaction::clipDuration(const trackedit::ClipKey& clipKey) const
{
    Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return -1.0;
    }

    std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.clipId);
    IF_ASSERT_FAILED(clip) {
        return -1.0;
    }

    return clip->End() - clip->Start();
}

void Au3Interaction::pushProjectHistoryJoinState(secs_t start, secs_t duration)
{
    std::stringstream ss;
    ss << "Joined " << duration << " seconds at " << start;

    projectHistory()->pushHistoryState(ss.str(), "Join");
}

bool Au3Interaction::undo()
{
    if (!canUndo()) {
        return false;
    }

    auto trackeditProject = globalContext()->currentProject()->trackeditProject();

    projectHistory()->undo();

    // Undo removes all tracks from current state and
    // inserts tracks from the previous state so we need
    // to reload whole model
    trackeditProject->reload();

    return true;
}

bool Au3Interaction::canUndo()
{
    return projectHistory()->undoAvailable();
}

bool Au3Interaction::redo()
{
    if (!canRedo()) {
        return false;
    }

    auto trackeditProject = globalContext()->currentProject()->trackeditProject();

    projectHistory()->redo();

    // Redo removes all tracks from current state and
    // inserts tracks from the previous state so we need
    // to reload whole model
    trackeditProject->reload();

    return true;
}

bool Au3Interaction::canRedo()
{
    return projectHistory()->redoAvailable();
}

bool Au3Interaction::toggleStretchToMatchProjectTempo(const ClipKey& clipKey)
{
    Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.clipId);
    IF_ASSERT_FAILED(clip) {
        return false;
    }

    bool newValue = !clip->GetStretchToMatchProjectTempo();
    clip->SetStretchToMatchProjectTempo(newValue);

    if (newValue) {
        double expectedEndTime = clip->End();
        auto prj = globalContext()->currentTrackeditProject();
        double projectTempo = prj->timeSignature().tempo;
        clip->SetClipTempo(projectTempo);
        clip->StretchRightTo(expectedEndTime);
        prj->notifyAboutClipChanged(DomConverter::clip(waveTrack, clip.get()));
    }

    return true;
}

int64_t Au3Interaction::clipGroupId(const ClipKey& clipKey) const
{
    Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return -1.0;
    }

    std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.clipId);
    IF_ASSERT_FAILED(clip) {
        return -1.0;
    }

    return clip->GetGroupId();
}

void Au3Interaction::setClipGroupId(const ClipKey& clipKey, int64_t id)
{
    Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return;
    }

    std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.clipId);
    IF_ASSERT_FAILED(clip) {
        return;
    }

    clip->SetGroupId(id);

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->notifyAboutClipChanged(DomConverter::clip(waveTrack, clip.get()));
}

void Au3Interaction::groupClips(const ClipKeyList& clipKeyList)
{
    const auto newGroupId = determineGroupId(clipKeyList);

    for (const auto& clipKey : clipKeyList) {
        setClipGroupId(clipKey, newGroupId);
    }

    projectHistory()->pushHistoryState("Clips grouped", "Clips grouped");
}

void Au3Interaction::ungroupClips(const ClipKeyList& clipKeyList)
{
    for (const auto& clipKey : clipKeyList) {
        setClipGroupId(clipKey, -1);
    }

    projectHistory()->pushHistoryState("Clips ungrouped", "Clips ungrouped");
}

int64_t Au3Interaction::determineGroupId(const ClipKeyList& clipKeyList) const
{
    if (!clipKeyList.empty()) {
        //! NOTE: check if any clip already belongs to a group
        for (const auto& selectedClip : clipKeyList) {
            if (clipGroupId(selectedClip) != -1) {
                return clipGroupId(selectedClip);
            }
        }
    }

    //! NOTE: none of the clips is grouped, find unique id for them
    auto prj = globalContext()->currentTrackeditProject();
    auto groupsList = prj->groupsIdsList();

    int64_t newGroupId = 0;
    while (muse::contains(groupsList, newGroupId)) {
        newGroupId++;
    }

    return newGroupId;
}

ClipKeyList Au3Interaction::clipsInGroup(int64_t id) const
{
    if (id == -1) {
        return ClipKeyList();
    }

    ClipKeyList groupedClips;

    auto prj = globalContext()->currentTrackeditProject();
    for (const auto& trackId : prj->trackIdList()) {
        for (const auto& clipKey : prj->clipList(trackId)) {
            if (clipGroupId(clipKey.key) == id) {
                groupedClips.push_back(clipKey.key);
            }
        }
    }

    return groupedClips;
}

muse::ProgressPtr Au3Interaction::progress() const
{
    return m_progress;
}

void Au3Interaction::pushProjectHistoryDuplicateState()
{
    projectHistory()->pushHistoryState("Duplicated", "Duplicate");
}

void Au3Interaction::pushProjectHistorySplitDeleteState(secs_t start, secs_t duration)
{
    std::stringstream ss;
    ss << "Split-deleted " << duration << " seconds at " << start;

    projectHistory()->pushHistoryState(ss.str(), "Split delete");
}

void Au3Interaction::pushProjectHistoryTrackAddedState()
{
    projectHistory()->pushHistoryState("Created new audio track", "New track");
}

void Au3Interaction::pushProjectHistoryTracksTrimState(secs_t start, secs_t end)
{
    std::stringstream ss;
    ss << "Trim selected audio tracks from " << start << " seconds to " << end << " seconds";

    projectHistory()->pushHistoryState(ss.str(), "Trim Audio");
}

void Au3Interaction::pushProjectHistoryTrackSilenceState(secs_t start, secs_t end)
{
    std::stringstream ss;
    ss << "Silenced selected tracks for " << start << " seconds at " << end << "";

    projectHistory()->pushHistoryState(ss.str(), "Silence");
}

void Au3Interaction::pushProjectHistoryPasteState()
{
    projectHistory()->pushHistoryState("Pasted from the clipboard", "Paste");
}

void Au3Interaction::pushProjectHistoryDeleteState(secs_t start, secs_t duration)
{
    std::stringstream ss;
    ss << "Delete " << duration << " seconds at " << start;

    projectHistory()->pushHistoryState(ss.str(), "Delete");
}

void Au3Interaction::pushProjectHistoryDeleteMultipleState()
{
    projectHistory()->pushHistoryState("Delete", "Deleted multiple clips");
}

void Au3Interaction::pushProjectHistoryChangeClipPitchState()
{
    projectHistory()->pushHistoryState("Pitch Shift", "Changed Pitch Shift");
}

void Au3Interaction::pushProjectHistoryResetClipPitchState()
{
    projectHistory()->pushHistoryState("Reset Clip Pitch", "Reset Clip Pitch");
}

void Au3Interaction::pushProjectHistoryChangeClipSpeedState()
{
    projectHistory()->pushHistoryState("Changed Speed", "Changed Speed");
}

void Au3Interaction::pushProjectHistoryResetClipSpeedState()
{
    projectHistory()->pushHistoryState("Reset Clip Speed", "Reset Clip Speed");
}

void Au3Interaction::pushProjectHistoryRenderClipStretchingState()
{
    projectHistory()->pushHistoryState("Rendered time-stretched audio", "Render");
}

bool Au3Interaction::doChangeClipSpeed(const ClipKey& clipKey, double speed)
{
    WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    std::shared_ptr<WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.clipId);
    IF_ASSERT_FAILED(clip) {
        return false;
    }

    TimeStretching::SetClipStretchRatio(*waveTrack, *clip, speed);
    makeRoomForClip(clipKey);

    LOGD() << "changed speed of clip: " << clipKey.clipId << ", track: " << clipKey.trackId;

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->notifyAboutClipChanged(DomConverter::clip(waveTrack, clip.get()));

    return true;
}
