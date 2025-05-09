#include "au3interaction.h"
#include "au3interactionutils.h"
#include "au3trackdata.h"

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

#include "au3wrap/internal/domaccessor.h"
#include "au3wrap/internal/domconverter.h"
#include "au3wrap/internal/trackcolor.h"
#include "au3wrap/internal/wxtypes_convert.h"
#include "au3wrap/au3types.h"

#include "trackedit/dom/track.h"
#include "trackedit/trackeditutils.h"

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

TrackIdList Au3Interaction::pasteIntoNewTracks(const std::vector<Au3TrackDataPtr>& tracksData)
{
    auto& project = projectRef();
    auto& tracks = Au3TrackList::Get(project);
    auto prj = globalContext()->currentTrackeditProject();
    secs_t selectedStartTime = globalContext()->playbackState()->playbackPosition();

    TrackIdList tracksIdsPastedInto;

    Au3Track* pFirstNewTrack = nullptr;
    for (const auto& data : tracksData) {
        auto pNewTrack = createNewTrackAndPaste(data->track(), tracks, selectedStartTime);
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
    pFirstTrack->Paste(begin, *track, false);
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

muse::Ret Au3Interaction::canPasteTrackData(const TrackIdList& dstTracksIds, const std::vector<Au3TrackDataPtr>& clipsToPaste,
                                            secs_t begin) const
{
    IF_ASSERT_FAILED(dstTracksIds.size() <= clipsToPaste.size()) {
        return make_ret(trackedit::Err::NotEnoughDataInClipboard);
    }

    for (size_t i = 0; i < dstTracksIds.size(); ++i) {
        Au3WaveTrack* dstWaveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(dstTracksIds[i]));
        IF_ASSERT_FAILED(dstWaveTrack) {
            return make_ret(trackedit::Err::WaveTrackNotFound);
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

muse::Ret Au3Interaction::makeRoomForClipsOnTracks(const std::vector<TrackId>& tracksIds,
                                                   const std::vector<Au3TrackDataPtr>& trackData,
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

        const WaveTrack* wt = dynamic_cast<const Au3WaveTrack*>(trackData.at(i)->track().get());
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

muse::Ret Au3Interaction::makeRoomForDataOnTracks(const std::vector<TrackId>& tracksIds,
                                                  const std::vector<Au3TrackDataPtr>& trackData,
                                                  secs_t begin,
                                                  bool pasteIntoExistingClip)
{
    IF_ASSERT_FAILED(tracksIds.size() <= trackData.size()) {
        return make_ret(trackedit::Err::NotEnoughDataInClipboard);
    }

    for (size_t i = 0; i < tracksIds.size(); ++i) {
        WaveTrack* dstWaveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(tracksIds.at(i)));
        IF_ASSERT_FAILED(dstWaveTrack) {
            return make_ret(trackedit::Err::WaveTrackNotFound);
        }

        const auto trackToPaste = std::static_pointer_cast<Au3WaveTrack>(trackData.at(i)->track());

        //! NOTE need to snap begin just like Paste() function do
        secs_t snappedBegin = dstWaveTrack->SnapToSample(begin);
        secs_t insertDuration = trackData.at(i)->track()->GetEndTime();

        // if paste into existing clip and there is a single clip to paste,
        // we need to make room for the clip to be extended
        if (pasteIntoExistingClip
            && singleClipOnTrack(trackToPaste.get())
            && dstWaveTrack->GetClipAtTime(begin) != nullptr) {
            secs_t currentClipEnd = dstWaveTrack->GetClipAtTime(begin)->GetPlayEndTime();
            snappedBegin = dstWaveTrack->SnapToSample(currentClipEnd);
        }

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

bool Au3Interaction::singleClipOnTrack(WaveTrack* waveTrack) const
{
    IF_ASSERT_FAILED(waveTrack) {
        return make_ret(trackedit::Err::WaveTrackNotFound);
    }

    if (waveTrack->Intervals().size() == 1) {
        return true;
    }
    return false;
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

    return clip->GetPlayStartTime();
}

muse::secs_t au::trackedit::Au3Interaction::clipEndTime(const ClipKey& clipKey) const
{
    Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return -1.0;
    }

    std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.clipId);
    IF_ASSERT_FAILED(clip) {
        return -1.0;
    }

    return clip->GetPlayEndTime();
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

    pushProjectHistoryChangeTrackTitle();

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

    pushProjectHistoryChangeClipTitle();

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

bool Au3Interaction::changeTrackColor(const TrackId trackId, const std::string& color)
{
    Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    auto& trackColor = TrackColor::Get(waveTrack);
    trackColor.SetColor(muse::draw::Color::fromString(color));

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->notifyAboutTrackChanged(DomConverter::track(waveTrack));

    for (auto& clips: DomAccessor::waveClipsAsList(waveTrack)) {
        //Set it back to auto
        clips->SetColor("");
        prj->notifyAboutClipChanged(DomConverter::clip(waveTrack, clips.get()));
    }

    projectHistory()->pushHistoryState("Changed track color", "Changed track color");

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
        prj->notifyAboutTrackChanged(DomConverter::track(waveTrack)); //! todo: replace with onClipChanged

        pushProjectHistoryRenderClipStretchingState();
    });

    return true;
}

void Au3Interaction::clearClipboard()
{
    clipboard()->clearTrackData();
}

bool Au3Interaction::clipTransferNeedsDownmixing(const std::vector<Au3TrackDataPtr>& srcTracks,
                                                 const TrackIdList& dstTracks) const
{
    IF_ASSERT_FAILED(srcTracks.size() >= dstTracks.size()) {
        return false;
    }

    for (size_t i = 0; i < dstTracks.size(); ++i) {
        const auto srcTrack = std::static_pointer_cast<Au3WaveTrack>(srcTracks.at(i)->track());
        const auto dstTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(dstTracks[i]));
        if (dstTrack->IsEmpty()) {
            // For empty tracks, we convert the track rather than the clip.
            continue;
        }
        IF_ASSERT_FAILED(srcTrack && dstTrack) {
            continue;
        }
        if (srcTrack->NChannels() == 2 && dstTrack->NChannels() == 1) {
            return true;
        }
    }
    return false;
}

namespace {
void notifyAboutTrackToggledStereo(au::trackedit::ITrackeditProject& prj, const Au3TrackList& trackList, const Au3WaveTrack& track)
{
    // prj.notifyAboutTrackRemoved(DomConverter::track(&track));
    // prj.notifyAboutTrackInserted(DomConverter::track(&track), utils::getTrackIndex(trackList, track));
    prj.trackChanged().send(DomConverter::track(&track));
}
}

muse::Ret Au3Interaction::pasteFromClipboard(secs_t begin, bool moveClips, bool moveAllTracks)
{
    if (clipboard()->trackDataEmpty()) {
        return make_ret(trackedit::Err::TrackEmpty);
    }

    std::vector<std::shared_ptr<Au3TrackData> > copiedData;
    {
        const std::vector<ITrackDataPtr> trackData = clipboard()->trackDataCopy();
        copiedData.reserve(trackData.size());
        for (const auto& trackDataHolder : trackData) {
            copiedData.push_back(std::static_pointer_cast<Au3TrackData>(trackDataHolder));
        }
    }

    project::IAudacityProjectPtr project = globalContext()->currentProject();
    auto tracks = project->trackeditProject()->trackList();

    if (moveAllTracks) {
        const auto it = std::max_element(copiedData.begin(), copiedData.end(),
                                         [](const auto& a, const auto& b) {
            return a->track()->GetEndTime() < b->track()->GetEndTime();
        });
        const secs_t duration = (*it)->track()->GetEndTime();
        auto existingTracks = project->trackeditProject()->trackIdList();
        insertBlankSpace(existingTracks, begin, duration);
    }

    TrackIdList selectedTracks = selectionController()->selectedTracks();
    if (selectedTracks.empty()) {
        const TrackIdList tracksIdsToSelect = pasteIntoNewTracks(copiedData);
        selectionController()->setSelectedTracks(tracksIdsToSelect);
        pushProjectHistoryPasteState();
        return muse::make_ok();
    }

    //! TODO: we need to make sure that we get a trackList with order
    //! the same as in the TrackPanel
    size_t clipboardTracksSize = clipboard()->trackDataSize();

    TrackIdList dstTracksIds = determineDestinationTracksIds(tracks, selectedTracks, clipboardTracksSize);

    if (clipTransferNeedsDownmixing(copiedData, dstTracksIds) && !userIsOkWithDownmixing()) {
        return muse::make_ok();
    }

    const bool newTracksNeeded = dstTracksIds.size() != clipboardTracksSize;

    auto ret = canPasteTrackData(dstTracksIds, copiedData, begin);
    if (!ret) {
        return ret;
    }

    muse::Ret ok { muse::make_ok() };
    bool pasteIntoExistingClip = !configuration()->pasteAsNewClip() && !moveAllTracks;

    if (!moveClips) {
        if (clipboard()->isMultiSelectionCopy()) {
            ok = makeRoomForClipsOnTracks(dstTracksIds, copiedData, begin);
        } else {
            ok = makeRoomForDataOnTracks(dstTracksIds, copiedData, begin, pasteIntoExistingClip);
        }
    }

    muse::Defer finalize{ [&]{
            if (ok) {
                pushProjectHistoryPasteState();
            } else {
                projectHistory()->rollbackState();
                project->trackeditProject()->reload();
            }
        } };

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

        const auto trackToPaste = std::static_pointer_cast<Au3WaveTrack>(copiedData.at(i)->track());

        if (dstWaveTrack->IsEmpty() && trackToPaste->NChannels() != dstWaveTrack->NChannels()) {
            auto& trackList = au3::Au3TrackList::Get(projectRef());
            dstWaveTrack = utils::toggleStereo(trackList, *dstWaveTrack);
            notifyAboutTrackToggledStereo(*prj, trackList, *dstWaveTrack);
        } else if (trackToPaste->NChannels() == 1 && dstWaveTrack->NChannels() == 2) {
            trackToPaste->MonoToStereo();
        } else if (trackToPaste->NChannels() == 2 && dstWaveTrack->NChannels() == 1) {
            ok = utils::withProgress(*interactive(),
                                     muse::trc("trackedit", "Mixing down to mono"),
                                     [&](utils::ProgressCb progressCb, utils::CancelCb cancelCb)
            {
                return trackToPaste->MixDownToMono(progressCb, cancelCb);
            });
            if (!ok) {
                return ok;
            }
        }

        // If we have multiple clips, we want to insert them one by one
        // to keep any existing clips that may fall between them intact
        if (!moveClips && clipboard()->isMultiSelectionCopy()) {
            trackToPaste->MoveTo(begin + trackToPaste->GetStartTime());
            for (const auto& interval : trackToPaste->Intervals()) {
                dstWaveTrack->InsertInterval(interval, false);
            }
        } else if (pasteIntoExistingClip
                   && singleClipOnTrack(trackToPaste.get())
                   && dstWaveTrack->GetClipAtTime(begin) != nullptr) {
            auto [leftClip, rightClip] = dstWaveTrack->SplitAt(begin);
            rightClip->SetPlayStartTime(begin + trackToPaste->GetClip(0)->GetPlayDuration());
            dstWaveTrack->Paste(begin, *trackToPaste, false);
            ProgressReporter dummyProgressReporter;
            dstWaveTrack->Join(leftClip->GetPlayStartTime(), rightClip->GetPlayEndTime(), dummyProgressReporter);
        } else {
            dstWaveTrack->Paste(begin, *trackToPaste, moveClips);
        }

        // Check which clips were added and trigger the onClipAdded event
        for (const auto& clip : prj->clipList(dstTracksIds[i])) {
            if (clipIdsBefore.find(clip.key.clipId) == clipIdsBefore.end()) {
                prj->notifyAboutClipAdded(clip);
            }
        }
        prj->notifyAboutTrackChanged(DomConverter::track(dstWaveTrack));
    }

    if (newTracksNeeded) {
        // remove already pasted elements from the clipboard and paste the rest into the new tracks
        copiedData.erase(copiedData.begin(), copiedData.begin() + dstTracksIds.size());
        const auto tracksIdsToSelect = pasteIntoNewTracks(copiedData);
        dstTracksIds.insert(dstTracksIds.end(), tracksIdsToSelect.begin(), tracksIdsToSelect.end());
    }

    selectionController()->setSelectedTracks(dstTracksIds);

    return ok;
}

ITrackDataPtr Au3Interaction::cutClip(const ClipKey& clipKey)
{
    Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return nullptr;
    }

    std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.clipId);
    IF_ASSERT_FAILED(clip) {
        return nullptr;
    }

    constexpr bool moveClips = true;
    auto track = waveTrack->Cut(clip->Start(), clip->End(), moveClips);
    const auto data = std::make_shared<Au3TrackData>(std::move(track));

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->notifyAboutClipRemoved(DomConverter::clip(waveTrack, clip.get()));
    prj->notifyAboutTrackChanged(DomConverter::track(waveTrack));

    return data;
}

ITrackDataPtr Au3Interaction::cutTrackData(const TrackId trackId, secs_t begin, secs_t end, bool moveClips)
{
    Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return nullptr;
    }

    auto track = waveTrack->Cut(begin, end, moveClips);
    const auto data = std::make_shared<Au3TrackData>(std::move(track));

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->notifyAboutTrackChanged(DomConverter::track(waveTrack));

    return data;
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
    clipboard()->addTrackData(std::make_shared<Au3TrackData>(std::move(track)));

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

    clipboard()->addTrackData(std::make_shared<Au3TrackData>(std::move(clipboardTrack)));
    if (clipKeys.size() > 1) {
        clipboard()->setMultiSelectionCopy(true);
    }

    return true;
}

bool Au3Interaction::copyContinuousTrackDataIntoClipboard(const TrackId trackId, secs_t begin, secs_t end)
{
    Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    auto track = waveTrack->Copy(begin, end);
    clipboard()->addTrackData(std::make_shared<Au3TrackData>(std::move(track)));

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

    waveTrack->Clear(clip->Start(), clip->End(), false);

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->notifyAboutTrackChanged(DomConverter::track(waveTrack));

    pushProjectHistoryDeleteState(start, duration);

    return true;
}

bool Au3Interaction::removeClips(const ClipKeyList& clipKeyList, bool moveClips)
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

        waveTrack->Clear(clip->Start(), clip->End(), moveClips);

        trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
        prj->notifyAboutTrackChanged(DomConverter::track(waveTrack));
    }

    pushProjectHistoryDeleteMultipleState();

    return true;
}

bool Au3Interaction::removeTracksData(const TrackIdList& tracksIds, secs_t begin, secs_t end, bool moveClips)
{
    secs_t duration = end - begin;
    secs_t start = begin;

    for (const TrackId& trackId : tracksIds) {
        Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
        IF_ASSERT_FAILED(waveTrack) {
            continue;
        }

        waveTrack->Clear(begin, end, moveClips);

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
    const muse::Defer defer([&] { m_busy = false; });

    trackPositionOffset = std::clamp(trackPositionOffset, -1, 1);

    const trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();

    if (!m_startTracklistInfo) {
        m_startTracklistInfo.emplace(utils::getTrackListInfo(Au3TrackList::Get(projectRef())));
    }

    //! NOTE: check if offset is applicable to every clip and recalculate if needed
    std::optional<secs_t> leftmostClipStartTime = getLeftmostClipStartTime(selectionController()->selectedClips());

    if (leftmostClipStartTime.has_value()) {
        if (muse::RealIsEqualOrLess(leftmostClipStartTime.value() + timePositionOffset, 0.0)) {
            timePositionOffset = -leftmostClipStartTime.value();
        }
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

        changeClipStartTime(selectedClip, clip->GetPlayStartTime() + timePositionOffset, completed);
    }

    if (trackPositionOffset != 0) {
        // Update m_moveClipsNeedsDownmixing only when moving up/down
        m_moveClipsNeedsDownmixing = moveSelectedClipsUpOrDown(trackPositionOffset) == NeedsDownmixing::Yes;
    }

    if (completed) {
        m_startTracklistInfo.reset();

        bool ok = true;
        const muse::Defer defer2([&] {
            m_moveClipsNeedsDownmixing = false;
            if (ok) {
                projectHistory()->pushHistoryState("Clip moved", "Move clip");
            } else {
                projectHistory()->rollbackState();
                prj->reload();
            }
        });

        if (m_moveClipsNeedsDownmixing && !userIsOkWithDownmixing()) {
            return ok = false;
        }

        //! TODO AU4: later when having keyboard arrow shortcut for moving clips
        //! make use of UndoPush::CONSOLIDATE arg in UndoManager
        ok = utils::withProgress(*interactive(),
                                 muse::trc("trackedit", "Rendering clips"),
                                 [&](utils::ProgressCb progressCb, utils::CancelCb cancelCb)
        {
            std::vector<std::pair<WaveTrack*, std::shared_ptr<WaveTrack> > > toReplace;
            for (const trackedit::TrackId track : selectionController()->selectedTracks()) {
                Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(track));
                IF_ASSERT_FAILED(waveTrack) {
                    continue;
                }
                const auto copy = std::static_pointer_cast<WaveTrack>(waveTrack->Duplicate(::Track::DuplicateOptions {}.Backup()));
                if (copy->FixClipChannels(progressCb, cancelCb)) {
                    toReplace.emplace_back(waveTrack, copy);
                } else {
                    return false;
                }
            }
            // No early return, meaning that the user did not cancel the operation. Now we can apply the changes.
            for (const auto& [oldOne, newOne] : toReplace) {
                utils::exchangeTrack(Au3TrackList::Get(projectRef()), *oldOne, *newOne);
                prj->notifyAboutTrackChanged(DomConverter::track(newOne.get()));
            }
            return true;
        });
    }

    return trackPositionOffset != 0;
}

bool Au3Interaction::splitTracksAt(const TrackIdList& tracksIds, std::vector<secs_t> pivots)
{
    for (const auto& trackId : tracksIds) {
        Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
        IF_ASSERT_FAILED(waveTrack) {
            return false;
        }

        for (const auto& pivot : pivots) {
            waveTrack->SplitAt(pivot);
        }

        trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
        prj->notifyAboutTrackChanged(DomConverter::track(waveTrack));
    }

    projectHistory()->pushHistoryState("Split", "Split");

    return true;
}

NeedsDownmixing Au3Interaction::moveSelectedClipsUpOrDown(int offset)
{
    // We create a temporary copy, from which we remove the moving clips.
    // This will help us decide whether the track can be toggled stereo or if it's the clips that should be converted.
    // Also, it feels better to do all the magic on some floating track list and, if all goes well, replace the original
    // with the modified copy, then sending the appropriate signals.

    // The algorithm is easier if we only allow moving up or down one track at a time.
    // From a UX perspective, this doesn't change a thing, since dragging a clip results in a myriad of `moveClips` calls.
    IF_ASSERT_FAILED(offset == -1 || offset == 1) {
        return NeedsDownmixing::No;
    }

    const auto& orig = ::TrackList::Get(projectRef());
    // Shallow-copies wave data, no worries.
    const auto copy = orig.Duplicate();
    const auto prj = globalContext()->currentTrackeditProject();
    ClipKeyList selectedClips = selectionController()->selectedClips();

    const auto dragDirection = offset == -1 ? utils::VerticalDrag::Up : utils::VerticalDrag::Down;
    // Make sure clips aren't dragged past the topmost track:
    if (dragDirection == utils::VerticalDrag::Up && std::any_of(selectedClips.begin(), selectedClips.end(), [&](const ClipKey& clip) {
        return utils::getTrackIndex(orig, clip.trackId) == 0;
    })) {
        return NeedsDownmixing::No;
    }

    const NeedsDownmixing needsDownmixing = utils::moveClipsVertically(dragDirection, orig,
                                                                       *copy, selectedClips);

    // Clean-up after ourselves, preserving original track formats:
    // Tracks that were empty at the start of the interaction, are empty now and differ in format must be restored.
    const TrackListInfo copyInfo = utils::getTrackListInfo(*copy);
    for (const size_t index : copyInfo.emptyTrackIndices) {
        if (index >= m_startTracklistInfo->size) {
            continue;
        }
        const auto isStereoNow = muse::contains(copyInfo.stereoTrackIndices, index);
        const auto wasStereoBefore = muse::contains(m_startTracklistInfo->stereoTrackIndices, index);
        if (isStereoNow != wasStereoBefore) {
            // Toggle back the way it was.
            utils::toggleStereo(*copy, index);
        }
    }

    // Now we can update the original with the modified copy.
    auto& mutOrig = const_cast<au3::Au3TrackList&>(orig);

    // Consume the tracks one by one.
    while (!copy->empty()) {
        Au3Track* const newTrack = *copy->begin();
        const Au3WaveTrack* const newWaveTrack = dynamic_cast<Au3WaveTrack*>(newTrack);

        if (!newWaveTrack) {
            // Not a wave track - not interested.
            copy->Remove(*newTrack);
            continue;
        }

        Au3WaveTrack* origWaveTrack = utils::getWaveTrack(mutOrig, newTrack->GetId());

        if (!origWaveTrack) {
            // This must be a new track created 'cos the user dragged clips down.
            assert(offset == 1);
            origWaveTrack = utils::appendWaveTrack(mutOrig, newWaveTrack->NChannels());
            prj->notifyAboutTrackAdded(DomConverter::track(origWaveTrack));
        }

        if (utils::clipIdSetsAreEqual(*origWaveTrack, *newWaveTrack)) {
            // No difference here, the tracks were untouched. Do not do anything to avoid unnecessary UI refresh.
            copy->Remove(*newTrack);
            continue;
        }

        // Default case: a clip from this track was either added or removed.

        const auto wasToggled = origWaveTrack->NChannels() != newWaveTrack->NChannels();
        const auto trackId = origWaveTrack->GetId();
        const auto clipsBefore = prj->clipList(trackId);
        // Careful, this decreases the `origWaveTrack` ref count.
        mutOrig.ReplaceOne(*origWaveTrack, std::move(*copy));
        if (wasToggled) {
            notifyAboutTrackToggledStereo(*prj, mutOrig, *newWaveTrack);
        } else {
            // Minimize the amount of UI refresh needed.
            const auto clipsAfter = prj->clipList(trackId);
            const auto removedClips = utils::clipSetDifference(clipsBefore, clipsAfter);
            for (const Clip* clip : removedClips) {
                prj->notifyAboutClipRemoved(*clip);
            }
            const auto addedClips = utils::clipSetDifference(clipsAfter, clipsBefore);
            for (const Clip* clip : addedClips) {
                prj->notifyAboutClipAdded(*clip);
            }
        }
    }

    // The selected clips were moved up or down, so we need to update their track IDs.
    for (auto& clipKey : selectedClips) {
        const size_t prevIndex = utils::getTrackIndex(orig, clipKey.trackId);
        clipKey.trackId = utils::getWaveTrack(orig, utils::TrackIndex { prevIndex + offset })->GetId();
    }
    selectionController()->setSelectedClips(selectedClips);

    if (offset < 0) {
        // The user dragged up. It's possible that the bottom-most tracks were created during this interaction,
        // in which case we make it nice to the user and remove them automatically.
        // `m_startTracklistInfo` tells use what the tracks looks like at the start of the interaction. We check all extra tracks.
        const auto tracks = prj->trackList();
        for (auto i = m_startTracklistInfo->size; i < tracks.size(); ++i) {
            const auto& track = tracks[i];
            Au3WaveTrack* const waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(track.id));
            if (waveTrack->IsEmpty()) {
                ::TrackList::Get(projectRef()).Remove(*waveTrack);
                prj->notifyAboutTrackRemoved(track);
            }
        }
    }

    return needsDownmixing;
}

bool Au3Interaction::splitRangeSelectionAtSilences(const TrackIdList& tracksIds, secs_t begin, secs_t end)
{
    for (const auto& trackId : tracksIds) {
        Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
        IF_ASSERT_FAILED(waveTrack) {
            continue;
        }

        waveTrack->Disjoin(begin, end);

        trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
        prj->notifyAboutTrackChanged(DomConverter::track(waveTrack));
    }

    projectHistory()->pushHistoryState("Split clips at silence", "Split at silence");

    return true;
}

bool Au3Interaction::splitClipsAtSilences(const ClipKeyList& clipKeyList)
{
    for (const auto& clipKey : clipKeyList) {
        Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(clipKey.trackId));
        IF_ASSERT_FAILED(waveTrack) {
            continue;
        }

        std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.clipId);
        IF_ASSERT_FAILED(clip) {
            continue;
        }

        waveTrack->Disjoin(clip->Start(), clip->End());

        trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
        prj->notifyAboutTrackChanged(DomConverter::track(waveTrack));
    }

    projectHistory()->pushHistoryState("Split clips at silence", "Split at silence");

    return true;
}

bool Au3Interaction::splitRangeSelectionIntoNewTracks(const TrackIdList& tracksIds, secs_t begin, secs_t end)
{
    for (const auto& trackId : tracksIds) {
        Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
        IF_ASSERT_FAILED(waveTrack) {
            continue;
        }

        bool hasClipInSelection = false;
        for (const auto& interval : waveTrack->Intervals()) {
            if ((interval->GetPlayStartTime() < end) && (interval->GetPlayEndTime() > begin)) {
                hasClipInSelection = true;
                break;
            }
        }

        if (!hasClipInSelection) {
            continue;
        }

        auto newTrack = waveTrack->Copy(begin, end, false);
        newTrack->MoveTo(begin);
        waveTrack->SplitDelete(begin, end);

        auto& projectTracks = Au3TrackList::Get(projectRef());
        projectTracks.Add(newTrack);

        trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
        prj->notifyAboutTrackChanged(DomConverter::track(waveTrack));
        prj->notifyAboutTrackAdded(DomConverter::track(newTrack.get()));
    }

    projectHistory()->pushHistoryState("Split into new track", "Split into new track");

    return true;
}

bool Au3Interaction::splitClipsIntoNewTracks(const ClipKeyList& clipKeyList)
{
    std::map<TrackId, std::vector<ClipKey> > clipsPerTrack;
    for (const auto& clipKey : clipKeyList) {
        clipsPerTrack[clipKey.trackId].push_back(clipKey);
    }

    for (const auto& [trackId, clips] : clipsPerTrack) {
        Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
        IF_ASSERT_FAILED(waveTrack) {
            continue;
        }

        auto& trackFactory = WaveTrackFactory::Get(projectRef());
        auto& pSampleBlockFactory = trackFactory.GetSampleBlockFactory();
        auto newTrack = waveTrack->EmptyCopy(pSampleBlockFactory);
        auto& projectTracks = Au3TrackList::Get(projectRef());

        for (const auto& clipKey : clips) {
            std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.clipId);
            IF_ASSERT_FAILED(clip) {
                continue;
            }

            newTrack->InsertInterval(waveTrack->CopyClip(*clip, true), false);
            waveTrack->SplitDelete(clip->Start(), clip->End());
        }
        projectTracks.Add(newTrack);

        trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
        prj->notifyAboutTrackChanged(DomConverter::track(waveTrack));
        prj->notifyAboutTrackAdded(DomConverter::track(newTrack.get()));
    }

    projectHistory()->pushHistoryState("Split into new track", "Split into new track");

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
    clipboard()->addTrackData(std::make_shared<Au3TrackData>(std::move(track)));

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
    clipboard()->addTrackData(std::make_shared<Au3TrackData>(std::move(track)));

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

bool Au3Interaction::trimClipLeft(const ClipKey& clipKey, secs_t deltaSec, secs_t minClipDuration, bool completed, UndoPushType type)
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
        projectHistory()->pushHistoryState("Clip left trimmed", "Trim clip left", type);
    }

    return true;
}

bool Au3Interaction::trimClipRight(const ClipKey& clipKey, secs_t deltaSec, secs_t minClipDuration, bool completed, UndoPushType type)
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
        projectHistory()->pushHistoryState("Clip right trimmed", "Trim clip right", type);
    }

    return true;
}

bool Au3Interaction::stretchClipLeft(const ClipKey& clipKey,
                                     secs_t deltaSec,
                                     secs_t minClipDuration,
                                     bool completed,
                                     UndoPushType type)
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
        projectHistory()->pushHistoryState("Clip left stretched", "Stretch clip left", type);
    }

    return true;
}

bool Au3Interaction::stretchClipRight(const ClipKey& clipKey,
                                      secs_t deltaSec,
                                      secs_t minClipDuration,
                                      bool completed,
                                      UndoPushType type)
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
        projectHistory()->pushHistoryState("Clip right stretched", "Stretch clip right", type);
    }

    return true;
}

bool Au3Interaction::newMonoTrack()
{
    addWaveTrack(1);
    return true;
}

bool Au3Interaction::newStereoTrack()
{
    addWaveTrack(2);
    return true;
}

void Au3Interaction::addWaveTrack(int numChannels)
{
    const auto track = utils::appendWaveTrack(Au3TrackList::Get(projectRef()), numChannels);

    const auto prj = globalContext()->currentTrackeditProject();
    prj->notifyAboutTrackAdded(DomConverter::track(track));

    selectionController()->setSelectedTracks({ track->GetId() });

    pushProjectHistoryTrackAddedState();
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
    if (trackIds.empty()) {
        return true;
    }

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
            constexpr bool moveClips = false;
            waveTrack->Clear(begin, end, moveClips);
        }

        prj->notifyAboutTrackChanged(DomConverter::track(waveTrack));
    }
}

void Au3Interaction::insertBlankSpace(const TrackIdList& trackIds, secs_t begin, secs_t duration)
{
    auto& trackFactory = WaveTrackFactory::Get(projectRef());
    auto defaultFormat = QualitySettings::SampleFormatChoice();
    auto rate = ProjectRate::Get(projectRef()).GetRate();

    auto prj = globalContext()->currentTrackeditProject();

    for (const auto& trackId : trackIds) {
        Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
        auto emptyTrack = trackFactory.Create(waveTrack->Channels().size(), defaultFormat, rate);
        auto emptyClip = emptyTrack->CreateClip();
        emptyClip->SetIsPlaceholder(true);
        emptyClip->InsertSilence(0, duration);
        emptyTrack->InsertInterval(std::move(emptyClip), true, false);

        waveTrack->Paste(begin, *emptyTrack, true);
        prj->notifyAboutTrackChanged(DomConverter::track(waveTrack));
    }
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
    const auto newGroupId = determineNewGroupId(clipKeyList);

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

int64_t Au3Interaction::determineNewGroupId(const ClipKeyList& clipKeyList) const
{
    if (!clipKeyList.empty()) {
        //! NOTE: Check if any clip already belongs to a group.
        //        If there are multiple groups, the first group is used.

        for (const auto& selectedClip : clipKeyList) {
            if (clipGroupId(selectedClip) != -1) {
                return clipGroupId(selectedClip);
            }
        }
    }

    return globalContext()->currentTrackeditProject()->createNewGroupID();
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

void Au3Interaction::pushProjectHistoryChangeTrackTitle()
{
    projectHistory()->pushHistoryState("Track Title", "Changed Track Title");
}

void Au3Interaction::pushProjectHistoryChangeClipTitle()
{
    projectHistory()->pushHistoryState("Clip Title", "Changed Clip Title");
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

bool Au3Interaction::userIsOkWithDownmixing() const
{
    if (!configuration()->askBeforeConvertingToMonoOrStereo()) {
        return true;
    }

    const std::string title = muse::trc("trackedit", "Mix down to mono");
    const std::string body = muse::trc("trackedit",
                                       "This action requires one or more clips to be converted to mono. Would you like to proceed?");

    const muse::IInteractive::Result result = interactive()->warningSync(title, body, {
        muse::IInteractive::Button::Cancel,
        muse::IInteractive::Button::Yes
    }, muse::IInteractive::Button::Cancel, muse::IInteractive::Option::WithDontShowAgainCheckBox);

    if (!result.showAgain()) {
        configuration()->setAskBeforeConvertingToMonoOrStereo(false);
    }

    if (result.standardButton() == muse::IInteractive::Button::Cancel) {
        configuration()->setAskBeforeConvertingToMonoOrStereo(true);
        return false;
    }

    return result.standardButton() == muse::IInteractive::Button::Yes;
}
