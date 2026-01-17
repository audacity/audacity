/*
* Audacity: A Digital Audio Editor
*/

#include "au3clipsinteraction.h"

#include <algorithm>

#include "au3-track/Track.h"
#include "au3-stretching-sequence/TempoChange.h"
#include "au3-wave-track/WaveClip.h"
#include "au3-wave-track/WaveTrackUtilities.h"
#include "au3-wave-track/WaveTrack.h"
#include "au3-wave-track/TimeStretching.h"

#include "global/types/ret.h"
#include "global/types/number.h"

#include "au3wrap/internal/domaccessor.h"
#include "au3wrap/internal/domconverter.h"
#include "au3wrap/internal/wxtypes_convert.h"
#include "au3wrap/au3types.h"

#include "dom/track.h"
#include "trackeditutils.h"

#include "au3interactionutils.h"
#include "au3trackdata.h"

#include "defer.h"
#include "log.h"
#include "trackediterrors.h"
#include "translation.h"

using namespace au::trackedit;
using namespace au::au3;

namespace {
static const std::string mixingDownToMonoLabel = muse::trc("trackedit", "Mixing down to mono...");
}

Au3ClipsInteraction::Au3ClipsInteraction()
{
    m_progress.setMaxNumIncrements(200);
}

au::au3::Au3Project& Au3ClipsInteraction::projectRef() const
{
    Au3Project* project = reinterpret_cast<Au3Project*>(globalContext()->currentProject()->au3ProjectPtr());
    return *project;
}

au::context::IPlaybackStatePtr Au3ClipsInteraction::playbackState() const
{
    return globalContext()->playbackState();
}

muse::async::Channel<au::trackedit::ClipKey, secs_t, bool> Au3ClipsInteraction::clipStartTimeChanged() const
{
    return m_clipStartTimeChanged;
}

muse::secs_t Au3ClipsInteraction::clipStartTime(const trackedit::ClipKey& clipKey) const
{
    Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return -1.0;
    }

    std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.itemId);
    IF_ASSERT_FAILED(clip) {
        return -1.0;
    }

    return clip->GetPlayStartTime();
}

muse::secs_t Au3ClipsInteraction::clipEndTime(const ClipKey& clipKey) const
{
    Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return -1.0;
    }

    std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.itemId);
    IF_ASSERT_FAILED(clip) {
        return -1.0;
    }

    return clip->GetPlayEndTime();
}

muse::secs_t Au3ClipsInteraction::clipDuration(const trackedit::ClipKey& clipKey) const
{
    Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return -1.0;
    }

    std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.itemId);
    IF_ASSERT_FAILED(clip) {
        return -1.0;
    }

    return clip->End() - clip->Start();
}

bool Au3ClipsInteraction::changeClipStartTime(const trackedit::ClipKey& clipKey, secs_t newStartTime, bool completed)
{
    Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.itemId);
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

bool Au3ClipsInteraction::changeClipTitle(const trackedit::ClipKey& clipKey, const muse::String& newTitle)
{
    Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.itemId);
    IF_ASSERT_FAILED(clip) {
        return false;
    }

    clip->SetName(wxFromString(newTitle));
    LOGD() << "changed name of clip: " << clipKey.itemId << ", track: " << clipKey.trackId;

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->notifyAboutClipChanged(DomConverter::clip(waveTrack, clip.get()));

    return true;
}

bool Au3ClipsInteraction::changeClipPitch(const ClipKey& clipKey, int pitch)
{
    WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    std::shared_ptr<WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.itemId);
    IF_ASSERT_FAILED(clip) {
        return false;
    }

    clip->SetCentShift(pitch);
    LOGD() << "changed pitch of clip: " << clipKey.itemId << ", track: " << clipKey.trackId << ", pitch: " << pitch;

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->notifyAboutClipChanged(DomConverter::clip(waveTrack, clip.get()));

    return true;
}

bool Au3ClipsInteraction::resetClipPitch(const ClipKey& clipKey)
{
    WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    std::shared_ptr<WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.itemId);
    IF_ASSERT_FAILED(clip) {
        return false;
    }

    clip->SetCentShift(0);
    LOGD() << "reseted pitch of clip: " << clipKey.itemId << ", track: " << clipKey.trackId;

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->notifyAboutClipChanged(DomConverter::clip(waveTrack, clip.get()));

    return true;
}

bool Au3ClipsInteraction::changeClipSpeed(const ClipKey& clipKey, double speed)
{
    return doChangeClipSpeed(clipKey, speed);
}

bool Au3ClipsInteraction::resetClipSpeed(const ClipKey& clipKey)
{
    return doChangeClipSpeed(clipKey, 1);
}

bool Au3ClipsInteraction::changeClipColor(const ClipKey& clipKey, const std::string& newColor)
{
    WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    std::shared_ptr<WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.itemId);
    IF_ASSERT_FAILED(clip) {
        return false;
    }

    clip->SetColor(newColor);

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->notifyAboutClipChanged(DomConverter::clip(waveTrack, clip.get()));

    return true;
}

bool Au3ClipsInteraction::changeClipOptimizeForVoice(const ClipKey& clipKey, bool optimize)
{
    WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    std::shared_ptr<WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.itemId);
    IF_ASSERT_FAILED(clip) {
        return false;
    }

    clip->SetPitchAndSpeedPreset(optimize ? PitchAndSpeedPreset::OptimizeForVoice : PitchAndSpeedPreset::Default);
    LOGD() << "changed optimize for voice of clip: " << clipKey.itemId << ", track: " << clipKey.trackId << ", optimize: " << optimize;

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->notifyAboutClipChanged(DomConverter::clip(waveTrack, clip.get()));

    return true;
}

bool Au3ClipsInteraction::renderClipPitchAndSpeed(const ClipKey& clipKey)
{
    interactive()->showProgress(muse::trc("trackedit", "Rendering pitch and speed..."), m_progress);
    m_progress.start();

    muse::ProgressResult result;

    DEFER {
        m_progress.finish(result);
    };

    WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    std::shared_ptr<WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.itemId);
    IF_ASSERT_FAILED(clip) {
        return false;
    }

    auto progressCallBack = [this](double progressFraction) {
        if (m_progress.isCanceled()) {
            throw std::runtime_error("");
        }
        if (m_progress.progress(progressFraction * 1000, 1000, "")) {
            QCoreApplication::processEvents();
        }
    };

    try {
        waveTrack->ApplyPitchAndSpeed({ { clip->GetPlayStartTime(), clip->GetPlayEndTime() } }, progressCallBack);
    } catch (const std::runtime_error&) {
        result = muse::ProgressResult::make_ret(muse::Ret::Code::Cancel);
        return false;
    }

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->notifyAboutTrackChanged(DomConverter::track(waveTrack));         //! todo: replace with onClipChanged

    return true;
}

ITrackDataPtr Au3ClipsInteraction::cutClip(const ClipKey& clipKey)
{
    Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return nullptr;
    }

    std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.itemId);
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

ITrackDataPtr Au3ClipsInteraction::copyClip(const ClipKey& clipKey)
{
    Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return nullptr;
    }

    std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.itemId);
    IF_ASSERT_FAILED(clip) {
        return nullptr;
    }

    auto track = waveTrack->Copy(clip->Start(), clip->End());

    return std::make_shared<Au3TrackData>(std::move(track));
}

std::optional<TimeSpan> Au3ClipsInteraction::removeClip(const trackedit::ClipKey& clipKey)
{
    Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return std::nullopt;
    }

    std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.itemId);
    IF_ASSERT_FAILED(clip) {
        return std::nullopt;
    }

    const double start = clip->Start();
    const double end = clip->End();
    waveTrack->Clear(start, end, false);

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->notifyAboutTrackChanged(DomConverter::track(waveTrack));

    return TimeSpan{ start, end };
}

bool Au3ClipsInteraction::removeClips(const ClipKeyList& clipKeyList, bool moveClips)
{
    if (clipKeyList.empty()) {
        return false;
    }

    for (const auto& clipKey : clipKeyList) {
        Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(clipKey.trackId));
        IF_ASSERT_FAILED(waveTrack) {
            return false;
        }

        std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.itemId);
        IF_ASSERT_FAILED(clip) {
            return false;
        }

        waveTrack->Clear(clip->Start(), clip->End(), moveClips);

        trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
        prj->notifyAboutTrackChanged(DomConverter::track(waveTrack));
    }

    return true;
}

muse::RetVal<ClipKeyList> Au3ClipsInteraction::moveClips(const ClipKeyList& clipKeyList, secs_t timePositionOffset,
                                                         int trackPositionOffset, bool completed,
                                                         bool& clipsMovedToOtherTracks)
{
    ClipKeyList newClipKeyList = clipKeyList;

    //! NOTE: cannot start moving until previous move is handled
    if (m_busy) {
        return muse::RetVal<ClipKeyList>::make_ok(newClipKeyList);
    }
    m_busy = true;
    const muse::Defer defer([&] { m_busy = false; });

    trackPositionOffset = std::clamp(trackPositionOffset, -1, 1);

    const trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();

    if (!m_tracksWhenDragStarted) {
        m_tracksWhenDragStarted.emplace(utils::getTrackListInfo(Au3TrackList::Get(projectRef())));
    }

    //! NOTE: check if offset is applicable to every clip and recalculate if needed
    std::optional<secs_t> leftmostClipStartTime = getLeftmostClipStartTime(clipKeyList);

    if (leftmostClipStartTime.has_value()) {
        if (muse::RealIsEqualOrLess(leftmostClipStartTime.value() + timePositionOffset, 0.0)) {
            timePositionOffset = -leftmostClipStartTime.value();
        }
    }

    if (selectionController()->timeSelectionIsNotEmpty() && !selectionController()->clipsIntersectingRangeSelection().empty()) {
        selectionController()->setDataSelectedStartTime(selectionController()->dataSelectedStartTime() + timePositionOffset, false);
        selectionController()->setDataSelectedEndTime(selectionController()->dataSelectedEndTime() + timePositionOffset, false);
    }

    for (const auto& selectedClip : clipKeyList) {
        Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(selectedClip.trackId));
        IF_ASSERT_FAILED(waveTrack) {
            continue;
        }

        std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, selectedClip.itemId);
        IF_ASSERT_FAILED(clip) {
            continue;
        }

        changeClipStartTime(selectedClip, clip->GetPlayStartTime() + timePositionOffset, completed);
    }

    if (trackPositionOffset != 0) {
        // Update m_moveClipsNeedsDownmixing only when moving up/down
        m_moveClipsNeedsDownmixing = moveSelectedClipsUpOrDown(newClipKeyList, trackPositionOffset) == NeedsDownmixing::Yes;
        clipsMovedToOtherTracks = true;
    }

    if (!completed) {
        return muse::RetVal<ClipKeyList>::make_ok(newClipKeyList);
    }

    m_tracksWhenDragStarted.reset();

    const muse::Defer defer2([&] {
        m_moveClipsNeedsDownmixing = false;
    });

    if (m_moveClipsNeedsDownmixing && !userIsOkWithDownmixing()) {
        return muse::RetVal<ClipKeyList>::make_ret(make_ret(Err::DownmixingIsNotAllowed));
    }

    muse::RetVal<ClipKeyList> result;
    //! TODO AU4: later when having keyboard arrow shortcut for moving clips
    //! make use of UndoPush::CONSOLIDATE arg in UndoManager
    result.ret = utils::withProgress(*interactive(), mixingDownToMonoLabel, [&](utils::ProgressCb progressCb, utils::CancelCb cancelCb)
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

    if (result.ret) {
        result.val = newClipKeyList;
    }

    return result;
}

void Au3ClipsInteraction::cancelClipDragEdit()
{
    // If false, then the edit wasn't a clip drag (could have been trim or stretch)
    if (m_tracksWhenDragStarted.has_value()) {
        if (const auto prj = globalContext()->currentTrackeditProject()) {
            // Doesn't matter it tracks are now empty or not - we're canceling the action.
            constexpr auto emptyOnly = false;
            tracksInteraction()->removeDragAddedTracks(m_tracksWhenDragStarted->size, emptyOnly);
        }
        m_tracksWhenDragStarted.reset();
    }
    m_moveClipsNeedsDownmixing = false;
}

bool Au3ClipsInteraction::splitClipsAtSilences(const ClipKeyList& clipKeyList)
{
    for (const auto& clipKey : clipKeyList) {
        Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(clipKey.trackId));
        IF_ASSERT_FAILED(waveTrack) {
            continue;
        }

        std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.itemId);
        IF_ASSERT_FAILED(clip) {
            continue;
        }

        waveTrack->Disjoin(clip->Start(), clip->End());

        trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
        prj->notifyAboutTrackChanged(DomConverter::track(waveTrack));
    }

    return true;
}

bool Au3ClipsInteraction::splitClipsIntoNewTracks(const ClipKeyList& clipKeyList)
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
            std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.itemId);
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

    return true;
}

bool Au3ClipsInteraction::duplicateClip(const ClipKey& clipKey)
{
    return duplicateClips({ clipKey });
}

bool Au3ClipsInteraction::duplicateClips(const ClipKeyList& clipKeyList)
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
            std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(track, clipKey.itemId);

            IF_ASSERT_FAILED(clip) {
                continue;
            }

            newTrack->InsertInterval(track->CopyClip(*clip, true), false);
        }
        projectTracks.Add(newTrack);
        prj->notifyAboutTrackAdded(DomConverter::track(newTrack.get()));
    }

    return true;
}

ITrackDataPtr Au3ClipsInteraction::clipSplitCut(const ClipKey& clipKey)
{
    Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return nullptr;
    }

    std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.itemId);
    IF_ASSERT_FAILED(clip) {
        return nullptr;
    }

    auto track = waveTrack->SplitCut(clip->Start(), clip->End());
    const auto data = std::make_shared<Au3TrackData>(std::move(track));

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->notifyAboutTrackChanged(DomConverter::track(waveTrack));

    return data;
}

bool Au3ClipsInteraction::clipSplitDelete(const ClipKey& clipKey)
{
    Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.itemId);
    IF_ASSERT_FAILED(clip) {
        return false;
    }

    waveTrack->SplitDelete(clip->Start(), clip->End());

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->notifyAboutTrackChanged(DomConverter::track(waveTrack));

    return true;
}

bool Au3ClipsInteraction::trimClipLeft(const ClipKey& clipKey, secs_t deltaSec, secs_t minClipDuration, bool completed)
{
    //! NOTE: other clips must follow if selected
    ClipKeyList clips = determineClipsForInteraction(clipKey);

    secs_t adjustedDelta = clampLeftTrimDelta(clips, deltaSec, minClipDuration);

    //! NOTE: don't be tempted to early return if delta is 0.0 or by any other reason:
    //! we still need to trigger cannibalistic clip behaviour and save project state
    //! to the history so this function has to execute till the end
    return trimClipsLeft(clips, adjustedDelta, completed);
}

bool Au3ClipsInteraction::trimClipRight(const ClipKey& clipKey, secs_t deltaSec, secs_t minClipDuration, bool completed)
{
    //! NOTE: other clips must follow if selected
    ClipKeyList clips = determineClipsForInteraction(clipKey);
    secs_t adjustedDelta = clampRightTrimDelta(clips, deltaSec, minClipDuration);

    //! NOTE: don't be tempted to early return if delta is 0.0 or by any other reason:
    //! we still need to trigger cannibalistic clip behaviour and save project state
    //! to the history so this function has to execute till the end
    return trimClipsRight(clips, adjustedDelta, completed);
}

bool Au3ClipsInteraction::stretchClipLeft(const ClipKey& clipKey, secs_t deltaSec, secs_t minClipDuration, bool completed)
{
    //! NOTE: other clips must follow if selected
    ClipKeyList clips = determineClipsForInteraction(clipKey);

    secs_t adjustedDelta = clampLeftStretchDelta(clips, deltaSec, minClipDuration);

    //! NOTE: don't be tempted to early return if delta is 0.0 or by any other reason:
    //! we still need to trigger cannibalistic clip behaviour and save project state
    //! to the history so this function has to execute till the end
    return stretchClipsLeft(clips, adjustedDelta, completed);
}

bool Au3ClipsInteraction::stretchClipRight(const ClipKey& clipKey, secs_t deltaSec, secs_t minClipDuration, bool completed)
{
    //! NOTE: other clips must follow if selected
    ClipKeyList clips = determineClipsForInteraction(clipKey);
    secs_t adjustedDelta = clampRightStretchDelta(clips, deltaSec, minClipDuration);

    //! NOTE: don't be tempted to early return if delta is 0.0 or by any other reason:
    //! we still need to trigger cannibalistic clip behaviour and save project state
    //! to the history so this function has to execute till the end
    return stretchClipsRight(clips, adjustedDelta, completed);
}

std::optional<secs_t> Au3ClipsInteraction::getLeftmostClipStartTime(const ClipKeyList& clipKeys) const
{
    std::optional<secs_t> leftmostClipStartTime;
    for (const auto& selectedClip : clipKeys) {
        Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(selectedClip.trackId));
        IF_ASSERT_FAILED(waveTrack) {
            continue;
        }

        std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, selectedClip.itemId);
        IF_ASSERT_FAILED(clip) {
            continue;
        }

        if (!leftmostClipStartTime.has_value() || !muse::RealIsEqualOrMore(clip->GetPlayStartTime(), leftmostClipStartTime.value())) {
            leftmostClipStartTime = clip->GetPlayStartTime();
        }
    }

    return leftmostClipStartTime;
}

std::optional<secs_t> Au3ClipsInteraction::getRightmostClipEndTime(const ClipKeyList& clipKeys) const
{
    std::optional<secs_t> rightmostClipEndTime;
    for (const auto& selectedClip : clipKeys) {
        Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(selectedClip.trackId));
        IF_ASSERT_FAILED(waveTrack) {
            continue;
        }

        std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, selectedClip.itemId);
        IF_ASSERT_FAILED(clip) {
            continue;
        }

        if (!rightmostClipEndTime.has_value() || !muse::RealIsEqualOrLess(clip->GetPlayEndTime(), rightmostClipEndTime.value())) {
            rightmostClipEndTime = clip->GetPlayEndTime();
        }
    }

    return rightmostClipEndTime;
}

muse::Ret Au3ClipsInteraction::makeRoomForClip(const ClipKey& clipKey)
{
    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();

    WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return make_ret(trackedit::Err::TrackNotFound);
    }

    std::shared_ptr<WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.itemId);
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

ClipKeyList Au3ClipsInteraction::clipsOnTrack(const TrackId trackId)
{
    Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));

    IF_ASSERT_FAILED(waveTrack) {
        return {};
    }

    std::list<std::shared_ptr<WaveClip> > clips = DomAccessor::waveClipsAsList(waveTrack);

    ClipKeyList result;
    for (const auto& clip : clips) {
        result.push_back(ClipKey(trackId, clip->GetId()));
    }

    return result;
}

bool Au3ClipsInteraction::toggleStretchToMatchProjectTempo(const ClipKey& clipKey)
{
    Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.itemId);
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

int64_t Au3ClipsInteraction::clipGroupId(const ClipKey& clipKey) const
{
    Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return -1.0;
    }

    std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.itemId);
    IF_ASSERT_FAILED(clip) {
        return -1.0;
    }

    return clip->GetGroupId();
}

void Au3ClipsInteraction::setClipGroupId(const ClipKey& clipKey, int64_t id)
{
    Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return;
    }

    std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.itemId);
    IF_ASSERT_FAILED(clip) {
        return;
    }

    clip->SetGroupId(id);

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->notifyAboutClipChanged(DomConverter::clip(waveTrack, clip.get()));
}

void Au3ClipsInteraction::groupClips(const ClipKeyList& clipKeyList)
{
    const auto newGroupId = determineNewGroupId(clipKeyList);

    for (const auto& clipKey : clipKeyList) {
        setClipGroupId(clipKey, newGroupId);
    }
}

void Au3ClipsInteraction::ungroupClips(const ClipKeyList& clipKeyList)
{
    for (const auto& clipKey : clipKeyList) {
        setClipGroupId(clipKey, -1);
    }
}

ClipKeyList Au3ClipsInteraction::clipsInGroup(int64_t id) const
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

int64_t Au3ClipsInteraction::determineNewGroupId(const ClipKeyList& clipKeyList) const
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

NeedsDownmixing Au3ClipsInteraction::moveSelectedClipsUpOrDown(ClipKeyList& clipKeyList, int offset)
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

    const auto dragDirection = offset == -1 ? utils::VerticalDrag::Up : utils::VerticalDrag::Down;
    // Make sure clips aren't dragged past the topmost track:
    if (dragDirection == utils::VerticalDrag::Up && std::any_of(clipKeyList.begin(), clipKeyList.end(), [&](const ClipKey& clip) {
        return utils::getTrackIndex(orig, clip.trackId) == 0;
    })) {
        return NeedsDownmixing::No;
    }

    const NeedsDownmixing needsDownmixing = utils::moveClipsVertically(dragDirection, orig,
                                                                       *copy, clipKeyList);

    // Clean-up after ourselves, preserving original track formats:
    // Tracks that were empty at the start of the interaction, are empty now and differ in format must be restored.
    const TrackListInfo copyInfo = utils::getTrackListInfo(*copy);
    for (const size_t index : copyInfo.emptyTrackIndices) {
        if (index >= m_tracksWhenDragStarted->size) {
            continue;
        }
        const auto isStereoNow = muse::contains(copyInfo.stereoTrackIndices, index);
        const auto wasStereoBefore = muse::contains(m_tracksWhenDragStarted->stereoTrackIndices, index);
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
            prj->trackChanged().send(DomConverter::track(newWaveTrack));
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
    // Search in the updated mutOrig (original track list that now has the moved clips)
    for (auto& clipKey : clipKeyList) {
        // Find which WaveTrack in mutOrig contains this clip
        Au3WaveTrack* newTrack = nullptr;
        for (auto track : mutOrig) {
            Au3WaveTrack* waveTrack = dynamic_cast<Au3WaveTrack*>(track);
            if (waveTrack) {
                // Check if this track contains the clip
                if (au3::DomAccessor::findWaveClip(waveTrack, clipKey.itemId)) {
                    newTrack = waveTrack;
                    break;
                }
            }
        }

        if (newTrack) {
            clipKey.trackId = newTrack->GetId();
        }
    }

    if (offset < 0) {
        // The user dragged up. It's possible that the bottom-most tracks were created during this interaction,
        // in which case we make it nice to the user and remove them automatically.
        // `m_tracksWhenDragStarted` tells use what the tracks looks like at the start of the interaction. We check all extra tracks.
        constexpr auto emptyOnly = true;
        tracksInteraction()->removeDragAddedTracks(m_tracksWhenDragStarted->size, emptyOnly);
    }

    return needsDownmixing;
}

std::optional<secs_t> Au3ClipsInteraction::shortestClipDuration(const ClipKeyList& clipKeys) const
{
    std::optional<secs_t> shortestClipDuration;
    for (const auto& selectedClip : clipKeys) {
        Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(selectedClip.trackId));
        IF_ASSERT_FAILED(waveTrack) {
            continue;
        }

        std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, selectedClip.itemId);
        IF_ASSERT_FAILED(clip) {
            continue;
        }
        if (!shortestClipDuration.has_value() || !muse::RealIsEqualOrMore(clip->GetPlayDuration(), shortestClipDuration.value())) {
            shortestClipDuration = clip->GetPlayDuration();
        }
    }

    return shortestClipDuration;
}

bool Au3ClipsInteraction::anyLeftFullyUntrimmed(const ClipKeyList& clipKeys) const
{
    for (const auto& selectedClip : clipKeys) {
        Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(selectedClip.trackId));
        IF_ASSERT_FAILED(waveTrack) {
            continue;
        }

        std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, selectedClip.itemId);
        IF_ASSERT_FAILED(clip) {
            continue;
        }
        if (muse::RealIsEqualOrLess(clip->GetTrimLeft(), 0.0)) {
            return true;
        }
    }

    return false;
}

bool Au3ClipsInteraction::anyRightFullyUntrimmed(const ClipKeyList& clipKeys) const
{
    for (const auto& selectedClip : clipKeys) {
        Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(selectedClip.trackId));
        IF_ASSERT_FAILED(waveTrack) {
            continue;
        }

        std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, selectedClip.itemId);
        IF_ASSERT_FAILED(clip) {
            continue;
        }
        if (muse::RealIsEqualOrLess(clip->GetTrimRight(), 0.0)) {
            return true;
        }
    }

    return false;
}

ClipKeyList Au3ClipsInteraction::determineClipsForInteraction(const ClipKey& clipKey) const
{
    if (!muse::contains(selectionController()->selectedClips(), clipKey)) {
        //! NOTE: hover handle single clip trim
        return ClipKeyList{ clipKey };
    } else {
        return selectionController()->selectedClips();
    }
}

secs_t Au3ClipsInteraction::clampLeftTrimDelta(const ClipKeyList& clipKeys,
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

        std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, selectedClip.itemId);
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

secs_t Au3ClipsInteraction::clampRightTrimDelta(const ClipKeyList& clipKeys,
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

        std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, selectedClip.itemId);
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

secs_t Au3ClipsInteraction::clampLeftStretchDelta(const ClipKeyList& clipKeys,
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

secs_t Au3ClipsInteraction::clampRightStretchDelta(const ClipKeyList& clipKeys,
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

bool Au3ClipsInteraction::trimClipsLeft(const ClipKeyList& clipKeys, secs_t deltaSec, bool completed)
{
    for (const auto& selectedClip : clipKeys) {
        Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(selectedClip.trackId));
        IF_ASSERT_FAILED(waveTrack) {
            return false;
        }

        std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, selectedClip.itemId);
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

bool Au3ClipsInteraction::trimClipsRight(const ClipKeyList& clipKeys, secs_t deltaSec, bool completed)
{
    for (const auto& selectedClip : clipKeys) {
        Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(selectedClip.trackId));
        IF_ASSERT_FAILED(waveTrack) {
            return false;
        }

        std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, selectedClip.itemId);
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

bool Au3ClipsInteraction::stretchClipsLeft(const ClipKeyList& clipKeys, secs_t deltaSec, bool completed)
{
    for (const auto& selectedClip : clipKeys) {
        Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(selectedClip.trackId));
        IF_ASSERT_FAILED(waveTrack) {
            return false;
        }

        std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, selectedClip.itemId);
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

bool Au3ClipsInteraction::stretchClipsRight(const ClipKeyList& clipKeys, secs_t deltaSec, bool completed)
{
    for (const auto& selectedClip : clipKeys) {
        Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(selectedClip.trackId));
        IF_ASSERT_FAILED(waveTrack) {
            return false;
        }

        std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, selectedClip.itemId);
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

bool Au3ClipsInteraction::doChangeClipSpeed(const ClipKey& clipKey, double speed)
{
    WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    std::shared_ptr<WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.itemId);
    IF_ASSERT_FAILED(clip) {
        return false;
    }

    TimeStretching::SetClipStretchRatio(*waveTrack, *clip, speed);
    makeRoomForClip(clipKey);

    LOGD() << "changed speed of clip: " << clipKey.itemId << ", track: " << clipKey.trackId;

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->notifyAboutClipChanged(DomConverter::clip(waveTrack, clip.get()));

    return true;
}

muse::Progress Au3ClipsInteraction::progress() const
{
    return m_progress;
}

// ========== Helper methods (TODO: consider refactoring) ==========

bool Au3ClipsInteraction::clipTransferNeedsDownmixing(const std::vector<ITrackDataPtr>& srcTracks,
                                                      const TrackIdList& dstTracks) const
{
    IF_ASSERT_FAILED(srcTracks.size() >= dstTracks.size()) {
        return false;
    }

    for (size_t i = 0; i < dstTracks.size(); ++i) {
        auto au3TrackData = std::dynamic_pointer_cast<Au3TrackData>(srcTracks.at(i));
        if (!au3TrackData) {
            continue;
        }

        const auto srcTrack = std::static_pointer_cast<Au3WaveTrack>(au3TrackData->track());
        const auto dstTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(dstTracks[i]));
        if (!dstTrack || dstTrack->IsEmpty()) {
            continue;
        }

        if (srcTrack->NChannels() == 2 && dstTrack->NChannels() == 1) {
            return true;
        }
    }
    return false;
}

bool Au3ClipsInteraction::userIsOkWithDownmixing() const
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

bool Au3ClipsInteraction::singleClipOnTrack(const TrackId trackId) const
{
    WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    return waveTrack->Intervals().size() == 1;
}

void Au3ClipsInteraction::trimOrDeleteOverlapping(::WaveTrack* waveTrack,
                                                  muse::secs_t begin,
                                                  muse::secs_t end,
                                                  std::shared_ptr<::WaveClip> otherClip)
{
    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();

    if (muse::RealIsEqualOrLess(begin, otherClip->GetPlayStartTime())
        && muse::RealIsEqualOrMore(end, otherClip->GetPlayEndTime())) {
        waveTrack->RemoveInterval(otherClip);
        prj->notifyAboutTrackChanged(DomConverter::track(waveTrack));
        return;
    }

    if (!muse::RealIsEqualOrLess(begin, otherClip->GetPlayStartTime())
        && !muse::RealIsEqualOrMore(end, otherClip->GetPlayEndTime())) {
        muse::secs_t otherClipStartTime = otherClip->GetPlayStartTime();
        muse::secs_t otherClipEndTime = otherClip->GetPlayEndTime();

        auto leftClip = waveTrack->CopyClip(*otherClip, true);
        waveTrack->InsertInterval(std::move(leftClip), false);
        prj->notifyAboutTrackChanged(DomConverter::track(waveTrack));

        muse::secs_t rightClipOverlap = (end - otherClip->GetPlayStartTime());
        otherClip->TrimLeft(rightClipOverlap);
        prj->notifyAboutClipChanged(DomConverter::clip(waveTrack, otherClip.get()));

        leftClip->SetPlayStartTime(otherClipStartTime);
        muse::secs_t leftClipOverlap = (otherClipEndTime - begin);
        leftClip->TrimRight(leftClipOverlap);
        prj->notifyAboutClipChanged(DomConverter::clip(waveTrack, leftClip.get()));

        prj->notifyAboutTrackChanged(DomConverter::track(waveTrack));
        return;
    }

    if (muse::RealIsEqualOrLess(begin, otherClip->GetPlayStartTime())
        && !muse::RealIsEqualOrMore(end, otherClip->GetPlayEndTime())
        && muse::RealIsEqualOrMore(end, otherClip->GetPlayStartTime())) {
        muse::secs_t overlap = (end - otherClip->GetPlayStartTime());
        otherClip->TrimLeft(overlap);
        prj->notifyAboutClipChanged(DomConverter::clip(waveTrack, otherClip.get()));
        return;
    }

    if (!muse::RealIsEqualOrLess(begin, otherClip->GetPlayStartTime())
        && muse::RealIsEqualOrLess(begin, otherClip->GetPlayEndTime())
        && muse::RealIsEqualOrMore(end, otherClip->GetPlayEndTime())) {
        muse::secs_t overlap = (otherClip->GetPlayEndTime() - begin);
        otherClip->TrimRight(overlap);
        prj->notifyAboutClipChanged(DomConverter::clip(waveTrack, otherClip.get()));
        return;
    }
}

muse::Ret Au3ClipsInteraction::makeRoomForDataOnTrack(const TrackId trackId,
                                                      muse::secs_t begin,
                                                      muse::secs_t end)
{
    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();

    WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return make_ret(trackedit::Err::TrackNotFound);
    }

    std::list<std::shared_ptr<WaveClip> > clips = DomAccessor::waveClipsAsList(waveTrack);

    clips.sort([](const std::shared_ptr<WaveClip>& c1, const std::shared_ptr<WaveClip>& c2) {
        return c1->GetPlayStartTime() < c2->GetPlayStartTime();
    });

    for (const auto& otherClip : clips) {
        trimOrDeleteOverlapping(waveTrack, begin, end, otherClip);
    }

    return make_ret(muse::Ret::Code::Ok);
}

muse::Ret Au3ClipsInteraction::makeRoomForClipsOnTracks(const std::vector<TrackId>& tracksIds,
                                                        const std::vector<ITrackDataPtr>& trackData,
                                                        muse::secs_t begin)
{
    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();

    IF_ASSERT_FAILED(tracksIds.size() <= trackData.size()) {
        return make_ret(trackedit::Err::NotEnoughDataInClipboard);
    }

    for (size_t i = 0; i < tracksIds.size(); ++i) {
        WaveTrack* dstWaveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(tracksIds.at(i)));
        IF_ASSERT_FAILED(dstWaveTrack) {
            return make_ret(trackedit::Err::TrackNotFound);
        }

        muse::secs_t snappedBegin = dstWaveTrack->SnapToSample(begin);

        auto au3TrackData = std::dynamic_pointer_cast<Au3TrackData>(trackData.at(i));
        if (!au3TrackData) {
            continue;
        }

        const WaveTrack* wt = dynamic_cast<const Au3WaveTrack*>(au3TrackData->track().get());
        for (const auto& interval : wt->Intervals()) {
            auto ok = makeRoomForDataOnTrack(tracksIds.at(i),
                                             snappedBegin + interval->GetPlayStartTime(),
                                             snappedBegin + interval->GetPlayEndTime());
            if (!ok) {
                return make_ret(trackedit::Err::FailedToMakeRoomForClip);
            }
        }
    }

    return muse::make_ok();
}
