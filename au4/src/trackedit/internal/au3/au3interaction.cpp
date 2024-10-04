#include "au3interaction.h"

#include <algorithm>

#include "ProjectRate.h"
#include "TempoChange.h"
#include "QualitySettings.h"
#include "global/types/number.h"

#include "libraries/lib-project/Project.h"
#include "libraries/lib-wave-track/WaveTrack.h"
#include "libraries/lib-track/Track.h"
#include "libraries/lib-wave-track/WaveClip.h"

#include "au3wrap/internal/domaccessor.h"
#include "au3wrap/internal/domconverter.h"
#include "au3wrap/internal/wxtypes_convert.h"

#include "trackedit/dom/track.h"

#include "log.h"
#include "trackediterrors.h"
#include "translation.h"

using namespace au::trackedit;
using namespace au::au3;

AudacityProject& Au3Interaction::projectRef() const
{
    AudacityProject* project = reinterpret_cast<AudacityProject*>(globalContext()->currentProject()->au3ProjectPtr());
    return *project;
}

muse::Ret Au3Interaction::pasteIntoNewTrack()
{
    auto& project = projectRef();
    auto& tracks = ::TrackList::Get(project);
    auto prj = globalContext()->currentTrackeditProject();
    secs_t selectedStartTime = globalContext()->playbackState()->playbackPosition();

    ::Track* pFirstNewTrack = NULL;
    for (auto data : clipboard()->trackData()) {
        auto pNewTrack = createNewTrackAndPaste(data.track, tracks, selectedStartTime);
        if (!pFirstNewTrack) {
            pFirstNewTrack = pNewTrack.get();
        }

        prj->onTrackAdded(DomConverter::track(pNewTrack.get()));
        for (const auto& clip : prj->clipList(DomConverter::trackId(pNewTrack->GetId()))) {
            prj->onClipAdded(clip);
        }
    }
    selectionController()->setSelectedTrack(DomConverter::trackId(pFirstNewTrack->GetId()));

    return muse::make_ok();
}

::Track::Holder Au3Interaction::createNewTrackAndPaste(std::shared_ptr<::Track> track, ::TrackList& list, secs_t begin)
{
    auto& trackFactory = WaveTrackFactory::Get(projectRef());
    auto& pSampleBlockFactory = trackFactory.GetSampleBlockFactory();

    WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(DomConverter::trackId(track->GetId())));
    IF_ASSERT_FAILED(waveTrack) {
        return nullptr;
    }
    auto pFirstTrack = waveTrack->EmptyCopy(pSampleBlockFactory);
    list.Add(pFirstTrack->SharedPointer());
    pFirstTrack->Paste(begin, *track);
    return pFirstTrack->SharedPointer();
}

std::vector<au::trackedit::TrackId> Au3Interaction::determineDestinationTracksIds(const std::vector<Track>& tracks,
                                                                                  TrackId destinationTrackId, size_t tracksNum) const
{
    std::vector<TrackId> tracksIds;
    bool addingEnabled = false;

    for (const auto& track : tracks) {
        if (track.id == destinationTrackId) {
            addingEnabled = true;
        }
        if (addingEnabled) {
            tracksIds.push_back(track.id);
            if (tracksIds.size() == tracksNum) {
                break;
            }
        }
    }

    return tracksIds;
}

muse::Ret Au3Interaction::canPasteClips(const std::vector<TrackId>& dstTracksIds, secs_t begin) const
{
    for (size_t i = 0; i < dstTracksIds.size(); ++i) {
        WaveTrack* dstWaveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(dstTracksIds[i]));
        IF_ASSERT_FAILED(dstWaveTrack) {
            return make_ret(trackedit::Err::WaveTrackNotFound);
        }

        secs_t insertDuration = clipboard()->trackData(i).track.get()->GetEndTime() - clipboard()->trackData(i).track.get()->GetStartTime();

        // throws incosistency exception if project tempo is not set, see:
        // au4/src/au3wrap/internal/au3project.cpp: 92
        // Paste func throws exception if there's not enough space to paste in (exception handler calls BasicUI logic)
        if (!dstWaveTrack->IsEmpty(begin, begin + insertDuration)) {
            LOGD() << "Not enough space to paste clip into";
            return make_ret(trackedit::Err::NotEnoughSpaceForPaste);
        }

        if (dstWaveTrack->NChannels() == 1 && clipboard()->trackData(i).track.get()->NChannels() == 2) {
            return make_ret(trackedit::Err::StereoClipIntoMonoTrack);
        }
    }

    return muse::make_ok();
}

au::audio::secs_t Au3Interaction::clipStartTime(const trackedit::ClipKey& clipKey) const
{
    WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return -1.0;
    }

    std::shared_ptr<WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.clipId);
    IF_ASSERT_FAILED(clip) {
        return -1.0;
    }

    return clip->Start();
}

bool Au3Interaction::changeClipStartTime(const trackedit::ClipKey& clipKey, secs_t newStartTime, bool completed)
{
    WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    std::shared_ptr<WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.clipId);
    IF_ASSERT_FAILED(clip) {
        return false;
    }

    //! NOTE Let's check for intersection with the previous and next clips
    //! Clips in AU3 model may not be sorted by time, so we need to get all clips and sort them by time
    {
        std::list<std::shared_ptr<WaveClip> > clips = DomAccessor::waveClipsAsList(waveTrack);

        clips.sort([](const std::shared_ptr<WaveClip>& c1, const std::shared_ptr<WaveClip>& c2) {
            return c1->GetPlayStartTime() < c2->GetPlayStartTime();
        });

        auto it = std::find_if(clips.begin(), clips.end(), [&clip](const std::shared_ptr<WaveClip>& c) {
            return clip.get() == c.get();
        });
        IF_ASSERT_FAILED(it != clips.end()) {
            return false;
        }

        //! NOTE If the new time is greater than the previous one, we check for an intersection with the next one
        if (newStartTime > clip->GetPlayStartTime()) {
            auto nextIt = ++it;
            //! NOTE Let's check if the clip is not the last one
            if (nextIt != clips.end()) {
                std::shared_ptr<WaveClip> nextClip = *nextIt;
                secs_t nextStartTime = nextClip->GetPlayStartTime();
                secs_t deltaSec = newStartTime - clip->GetPlayStartTime();

                // check collision
                if ((clip->GetPlayEndTime() + deltaSec) > nextStartTime) {
                    //! NOTE If we have reached the limit, we do nothing.
                    //! TODO maybe there is a problem here, the end time of the clip may coincide with the start time of the next clip
                    if (muse::is_equal(clip->GetPlayEndTime(), nextStartTime.to_double())) {
                        LOGW() << "You can't change the clip time because it overlaps with the next one.";
                        return false;
                    }
                    //! NOTE If we haven't reached the limit yet, then it shifts as much as possible
                    else {
                        deltaSec = nextStartTime - clip->GetPlayEndTime();
                        newStartTime = clip->GetPlayStartTime() + deltaSec;
                    }
                }
            }
        }

        if (newStartTime < clip->GetPlayStartTime()) {
            //! NOTE For the first clip, the end time is 0.0
            secs_t prevEndTime = 0.0;
            if (it != clips.begin()) {
                auto prevIt = --it;
                std::shared_ptr<WaveClip> pervClip = *prevIt;
                prevEndTime = pervClip->GetPlayEndTime();
            }

            // check collision
            if (newStartTime < prevEndTime) {
                //! NOTE If we have reached the limit, we do nothing.
                //! TODO maybe there is a problem here, the start time of the clip may coincide with the end time of the prev clip
                if (muse::is_equal(clip->GetPlayStartTime(), prevEndTime.to_double())) {
                    LOGW() << "You can't change the clip time because it overlaps with the prev one.";
                    return false;
                }
                //! NOTE If we haven't reached the limit yet, then it shifts as much as possible
                else {
                    newStartTime = prevEndTime;
                }
            }
        }
    }

    //! TODO Not sure what this method needs to be called to change the position, will need to clarify
    clip->SetPlayStartTime(newStartTime);
    // LOGD() << "changed PlayStartTime of track: " << clipKey.trackId
    //        << " clip: " << clipKey.index
    //        << " new PlayStartTime: " << newStartTime;

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->onClipChanged(DomConverter::clip(waveTrack, clip.get()));

    m_clipStartTimeChanged.send(clipKey, newStartTime, completed);

    if (completed) {
        //! TODO AU4: later when having keyboard arrow shortcut for moving clips
        //! make use of UndoPush::CONSOLIDATE arg in UndoManager
        projectHistory()->pushHistoryState("Clip moved", "Move clip");
    }

    return true;
}

muse::async::Channel<au::trackedit::ClipKey, secs_t /*newStartTime*/, bool /*completed*/>
Au3Interaction::clipStartTimeChanged() const
{
    return m_clipStartTimeChanged;
}

bool Au3Interaction::trimTrackData(TrackId trackId, secs_t begin, secs_t end)
{
    WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    waveTrack->Trim(begin, end);

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->onTrackChanged(DomConverter::track(waveTrack));

    return true;
}

bool Au3Interaction::silenceTrackData(TrackId trackId, secs_t begin, secs_t end)
{
    WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    waveTrack->Silence(begin, end, {});

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->onTrackChanged(DomConverter::track(waveTrack));

    return true;
}

bool Au3Interaction::changeClipTitle(const trackedit::ClipKey& clipKey, const muse::String& newTitle)
{
    WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    std::shared_ptr<WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.clipId);
    IF_ASSERT_FAILED(clip) {
        return false;
    }

    clip->SetName(wxFromString(newTitle));
    LOGD() << "changed name of clip: " << clipKey.clipId << ", track: " << clipKey.trackId;

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->onClipChanged(DomConverter::clip(waveTrack, clip.get()));

    return true;
}

void Au3Interaction::clearClipboard()
{
    clipboard()->clearTrackData();
}

muse::Ret Au3Interaction::pasteFromClipboard(secs_t begin, TrackId destinationTrackId)
{
    if (clipboard()->trackDataEmpty()) {
        return make_ret(trackedit::Err::TrackEmpty);
    }

    if (destinationTrackId == -1) {
        return pasteIntoNewTrack();
    }

    project::IAudacityProjectPtr project = globalContext()->currentProject();
    //! TODO: we need to make sure that we get a trackList with order
    //! the same as in the TrackPanel
    auto tracks = project->trackeditProject()->trackList();
    size_t tracksNum = clipboard()->trackDataSize();

    // for multiple tracks copying
    std::vector<TrackId> dstTracksIds = determineDestinationTracksIds(tracks, destinationTrackId, tracksNum);

    bool newTracksNeeded = false;
    size_t newTracksCount = 0;
    if (dstTracksIds.size() != tracksNum) {
        newTracksNeeded = true;
        newTracksCount = tracksNum - dstTracksIds.size();
    }

    // check if copied data fits into selected area
    auto ret = canPasteClips(dstTracksIds, begin);
    if (!ret) {
        return ret;
    }

    for (size_t i = 0; i < dstTracksIds.size(); ++i) {
        WaveTrack* dstWaveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(dstTracksIds[i]));
        IF_ASSERT_FAILED(dstWaveTrack) {
            return make_ret(trackedit::Err::WaveTrackNotFound);
        }
        auto prj = globalContext()->currentTrackeditProject();

        // use an unordered_set to store the IDs of the clips before the paste
        std::unordered_set<int> clipIdsBefore;
        for (const auto& clip : prj->clipList(dstTracksIds[i])) {
            clipIdsBefore.insert(clip.key.clipId);
        }

        if (clipboard()->trackData(i).track.get()->NChannels() == 1 && dstWaveTrack->NChannels() == 2) {
            // When the source is mono, may paste its only channel
            // repeatedly into a stereo track
            const auto pastedTrack = std::static_pointer_cast<WaveTrack>(clipboard()->trackData(i).track);
            pastedTrack->MonoToStereo();
            dstWaveTrack->Paste(begin, *pastedTrack);
        } else {
            dstWaveTrack->Paste(begin, *clipboard()->trackData(i).track);
        }

        // Check which clips were added and trigger the onClipAdded event
        for (const auto& clip : prj->clipList(dstTracksIds[i])) {
            if (clipIdsBefore.find(clip.key.clipId) == clipIdsBefore.end()) {
                prj->onClipAdded(clip);
            }
        }
    }

    if (newTracksNeeded) {
        // remove already pasted elements from the clipboard and paste the rest into the new tracks
        clipboard()->eraseTrackData(clipboard()->trackData().begin(), clipboard()->trackData().begin() + dstTracksIds.size());
        return pasteIntoNewTrack();
    }

    return muse::make_ok();
}

bool Au3Interaction::cutClipIntoClipboard(const ClipKey &clipKey)
{
    WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    std::shared_ptr<WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.clipId);
    IF_ASSERT_FAILED(clip) {
        return false;
    }

    auto track = waveTrack->Cut(clip->Start(), clip->End());
    clipboard()->addTrackData(TrackData { track, clipKey });

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->onClipRemoved(DomConverter::clip(waveTrack, clip.get()));
    projectHistory()->pushHistoryState("Cut to the clipboard", "Cut");

    return true;
}

bool Au3Interaction::cutClipDataIntoClipboard(const std::vector<TrackId> &tracksIds, secs_t begin, secs_t end)
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
    WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    auto track = waveTrack->Cut(begin, end);
    trackedit::ClipKey dummyClipKey = trackedit::ClipKey();
    clipboard()->addTrackData(TrackData { track, dummyClipKey });

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->onTrackChanged(DomConverter::track(waveTrack));

    return true;
}

bool Au3Interaction::copyClipIntoClipboard(const ClipKey& clipKey)
{
    WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    std::shared_ptr<WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.clipId);
    IF_ASSERT_FAILED(clip) {
        return false;
    }

    auto track = waveTrack->Copy(clip->Start(), clip->End());
    clipboard()->addTrackData(TrackData { track, clipKey });

    return true;
}

bool Au3Interaction::copyClipDataIntoClipboard(const ClipKey& clipKey, secs_t begin, secs_t end)
{
    WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    std::shared_ptr<WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.clipId);
    IF_ASSERT_FAILED(clip) {
        return false;
    }

    auto track = waveTrack->Copy(begin, end);
    clipboard()->addTrackData(TrackData { track, clipKey });

    return true;
}

bool Au3Interaction::copyTrackDataIntoClipboard(const TrackId trackId, secs_t begin, secs_t end)
{
    WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(trackId));
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
    WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    std::shared_ptr<WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.clipId);
    IF_ASSERT_FAILED(clip) {
        return false;
    }

    waveTrack->Clear(clip->Start(), clip->End());

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->onTrackChanged(DomConverter::track(waveTrack));

    return true;
}

bool Au3Interaction::removeClipData(const trackedit::ClipKey& clipKey, secs_t begin, secs_t end)
{
    WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    std::shared_ptr<WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.clipId);
    IF_ASSERT_FAILED(clip) {
        return false;
    }

    waveTrack->Clear(begin, end);

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->onTrackChanged(DomConverter::track(waveTrack));

    return true;
}

bool Au3Interaction::splitAt(TrackId trackId, secs_t pivot)
{
    WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    waveTrack->SplitAt(pivot);

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->onTrackChanged(DomConverter::track(waveTrack));

    projectHistory()->pushHistoryState("Split", "Split");

    return true;
}

bool Au3Interaction::mergeSelectedOnTrack(const TrackId trackId, secs_t begin, secs_t end)
{
    WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    //! TODO fix this so it displays progress if there's
    //! a need to change pitch/speed
    ProgressReporter dummyProgressReporter;
    waveTrack->Join(begin, end, dummyProgressReporter);

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->onTrackChanged(DomConverter::track(waveTrack));

    return true;
}

bool Au3Interaction::duplicateSelectedOnTrack(const TrackId trackId, secs_t begin, secs_t end)
{
    auto& tracks = ::TrackList::Get(projectRef());
    WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    auto dest = waveTrack->Copy(begin, end, false);
    dest->MoveTo(std::max(static_cast<double>(begin), waveTrack->GetStartTime()));
    tracks.Add(dest);

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->onTrackAdded(DomConverter::track(dest.get()));
}

bool Au3Interaction::splitCutSelectedOnTrack(const TrackId trackId, secs_t begin, secs_t end)
{
    WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    auto track = waveTrack->SplitCut(begin, end);
    trackedit::ClipKey dummyClipKey = trackedit::ClipKey();
    clipboard()->addTrackData(TrackData { track, dummyClipKey });

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->onTrackChanged(DomConverter::track(waveTrack));

    return true;
}

bool Au3Interaction::splitDeleteSelectedOnTrack(const TrackId trackId, secs_t begin, secs_t end)
{
    WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    waveTrack->SplitDelete(begin, end);

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->onTrackChanged(DomConverter::track(waveTrack));

    return true;
}

bool Au3Interaction::mergeSelectedOnTracks(const std::vector<TrackId> tracksIds, secs_t begin, secs_t end)
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

bool Au3Interaction::duplicateSelectedOnTracks(const std::vector<TrackId> tracksIds, secs_t begin, secs_t end)
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

bool Au3Interaction::duplicateClip(const ClipKey &clipKey)
{
    WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    std::shared_ptr<WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.clipId);
    IF_ASSERT_FAILED(clip) {
        return false;
    }

    trackedit::secs_t begin = clip->Start();
    trackedit::secs_t end = clip->End();

    bool ok = duplicateSelectedOnTrack(clipKey.trackId, begin, end);
    if (!ok) {
        return false;
    }

    pushProjectHistoryDuplicateState();
}

bool Au3Interaction::clipSplitCut(const ClipKey &clipKey)
{
    WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    std::shared_ptr<WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.clipId);
    IF_ASSERT_FAILED(clip) {
        return false;
    }

    auto track = waveTrack->SplitCut(clip->Start(), clip->End());
    trackedit::ClipKey dummyClipKey = trackedit::ClipKey();
    clipboard()->addTrackData(TrackData { track, dummyClipKey });

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->onTrackChanged(DomConverter::track(waveTrack));

    projectHistory()->pushHistoryState("Split-cut to the clipboard", "Split cut");

    return true;
}

bool Au3Interaction::clipSplitDelete(const ClipKey &clipKey)
{
    WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    std::shared_ptr<WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.clipId);
    IF_ASSERT_FAILED(clip) {
        return false;
    }

    waveTrack->SplitDelete(clip->Start(), clip->End());

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->onTrackChanged(DomConverter::track(waveTrack));

    pushProjectHistorySplitDeleteState(clip->Start(), clip->End() - clip->Start());

    return true;
}

bool Au3Interaction::splitCutSelectedOnTracks(const std::vector<TrackId> tracksIds, secs_t begin, secs_t end)
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

bool Au3Interaction::splitDeleteSelectedOnTracks(const std::vector<TrackId> tracksIds, secs_t begin, secs_t end)
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

bool Au3Interaction::trimClipLeft(const ClipKey& clipKey, secs_t deltaSec, bool completed)
{
    WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    std::shared_ptr<WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.clipId);
    IF_ASSERT_FAILED(clip) {
        return false;
    }

    clip->TrimLeft(deltaSec);

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->onClipChanged(DomConverter::clip(waveTrack, clip.get()));

    if (completed) {
        projectHistory()->pushHistoryState("Clip trimmed", "Trim clip");
    }

    return true;
}

bool Au3Interaction::trimClipRight(const ClipKey& clipKey, secs_t deltaSec, bool completed)
{
    WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    std::shared_ptr<WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.clipId);
    IF_ASSERT_FAILED(clip) {
        return false;
    }

    clip->TrimRight(deltaSec);

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->onClipChanged(DomConverter::clip(waveTrack, clip.get()));

    if (completed) {
        projectHistory()->pushHistoryState("Clip trimmed", "Trim clip");
    }

    return true;
}

void Au3Interaction::newMonoTrack()
{
    auto& project = projectRef();
    auto& tracks = ::TrackList::Get(project);
    auto& trackFactory = ::WaveTrackFactory::Get(project);

    sampleFormat defaultFormat = QualitySettings::SampleFormatChoice();
    auto rate = ::ProjectRate::Get(project).GetRate();

    auto track = trackFactory.Create(defaultFormat, rate);
    track->SetName(tracks.MakeUniqueTrackName(WaveTrack::GetDefaultAudioTrackNamePreference()));

    tracks.Add(track);

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->onTrackAdded(DomConverter::track(track.get()));

    selectionController()->setSelectedTrack(DomConverter::trackId(track->GetId()));
}

void Au3Interaction::newStereoTrack()
{
    auto& project = projectRef();
    auto& tracks = ::TrackList::Get(project);
    auto& trackFactory = ::WaveTrackFactory::Get(project);

    sampleFormat defaultFormat = QualitySettings::SampleFormatChoice();
    auto rate = ::ProjectRate::Get(project).GetRate();

    tracks.Add(trackFactory.Create(2, defaultFormat, rate));
    auto& newTrack = **tracks.rbegin();
    newTrack.SetName(tracks.MakeUniqueTrackName(WaveTrack::GetDefaultAudioTrackNamePreference()));

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    auto track = *tracks.rbegin();
    prj->onTrackAdded(DomConverter::track(track));

    selectionController()->setSelectedTrack(DomConverter::trackId(track->GetId()));
}

void Au3Interaction::newLabelTrack()
{
    NOT_IMPLEMENTED;
}

au::audio::secs_t Au3Interaction::clipDuration(const trackedit::ClipKey& clipKey) const
{
    WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return -1.0;
    }

    std::shared_ptr<WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.clipId);
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

void Au3Interaction::undo()
{
    if (!projectHistory()->undoAvailable()) {
        interactive()->error(muse::trc("undo", "Undo"), std::string("Undo not available"));
        return;
    }

    auto trackeditProject = globalContext()->currentProject()->trackeditProject();

    // Find the index of the currently selected track in the old list
    auto trackIdList = trackeditProject->trackIdList();
    int selectedIndex = std::distance(trackIdList.begin(),
                                      std::find(trackIdList.begin(),
                                                trackIdList.end(),
                                                selectionController()->selectedTrack()));

    projectHistory()->undo();

    // Undo removes all tracks from current state and
    // inserts tracks from the previous state so we need
    // to reload whole model
    trackeditProject->reload();

    // Update selected track id
    auto newTrackIdList = trackeditProject->trackIdList();
    if (selectedIndex >= 0 && selectedIndex < newTrackIdList.size()) {
        selectionController()->setSelectedTrack(newTrackIdList[selectedIndex]);
    } else {
        selectionController()->resetSelectedTrack();
    }
}

void Au3Interaction::redo()
{   
    if (!projectHistory()->redoAvailable()) {
        interactive()->error(muse::trc("redo", "Redo"), std::string("Redo not available"));
        return;
    }

    auto trackeditProject = globalContext()->currentProject()->trackeditProject();

    // Find the index of the currently selected track in the old list
    auto trackIdList = trackeditProject->trackIdList();
    int selectedIndex = std::distance(trackIdList.begin(),
                                      std::find(trackIdList.begin(),
                                                trackIdList.end(),
                                                selectionController()->selectedTrack()));

    projectHistory()->redo();

    // Redo removes all tracks from current state and
    // inserts tracks from the previous state so we need
    // to reload whole model
    trackeditProject->reload();

    // Update selected track id
    auto newTrackIdList = trackeditProject->trackIdList();
    if (selectedIndex >= 0 && selectedIndex < newTrackIdList.size()) {
        selectionController()->setSelectedTrack(newTrackIdList[selectedIndex]);
    } else {
        selectionController()->resetSelectedTrack();
    }
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
