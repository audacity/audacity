/*
* Audacity: A Digital Audio Editor
*/
#include "au3tracksinteraction.h"

#include <algorithm>
#include <cmath>

#include "au3-track/Track.h"
#include "au3-track/TimeWarper.h"
#include "au3-track/PendingTracks.h"
#include "au3-wave-track/WaveTrackUtilities.h"
#include "au3-wave-track/WaveTrack.h"
#include "au3-wave-track/WaveClip.h"
#include "au3-label-track/LabelTrack.h"
#include "au3-project-rate/ProjectRate.h"
#include "au3-project-rate/QualitySettings.h"
#include "au3-effects/MixAndRender.h"
#include "au3-realtime-effects/RealtimeEffectList.h"
#include "au3-exceptions/UserException.h"

#include "framework/global/types/number.h"

#include "au3wrap/internal/domaccessor.h"
#include "au3wrap/internal/domconverter.h"
#include "au3wrap/internal/trackcolor.h"
#include "au3wrap/internal/trackrulertypeattachment.h"
#include "au3wrap/internal/trackviewtypeattachment.h"
#include "au3wrap/internal/waveformscale.h"
#include "au3wrap/internal/wxtypes_convert.h"
#include "au3wrap/au3types.h"

#include "dom/track.h"
#include "playback/playbacktypes.h"
#include "realfn.h"
#include "thirdparty/kors_logger/src/log_base.h"
#include "trackediterrors.h"

#include "au3interactionutils.h"
#include "au3trackdata.h"

#include "defer.h"
#include "log.h"
#include "translation.h"

using namespace au::trackedit;
using namespace au::au3;

namespace {
const std::string mixingDownToMonoLabel = muse::trc("trackedit", "Mixing down to mono...");

constexpr float DEFAULT_VERTICAL_RANGE = 1.0f;
constexpr float MAX_VERTICAL_RANGE = 2.0f;
constexpr float MIN_VERTICAL_RANGE = 1.0f / (1 << 9);
}

Au3TracksInteraction::Au3TracksInteraction()
{
    m_progress.setMaxNumIncrements(200);
}

au::au3::Au3Project& Au3TracksInteraction::projectRef() const
{
    Au3Project* project = reinterpret_cast<Au3Project*>(globalContext()->currentProject()->au3ProjectPtr());
    return *project;
}

au::context::IPlaybackStatePtr Au3TracksInteraction::playbackState() const
{
    return globalContext()->playbackState();
}

bool Au3TracksInteraction::trimTracksData(const std::vector<TrackId>& tracksIds, secs_t begin, secs_t end)
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

    return true;
}

bool Au3TracksInteraction::silenceTracksData(const std::vector<trackedit::TrackId>& tracksIds, secs_t begin, secs_t end)
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

    return true;
}

bool Au3TracksInteraction::changeTrackTitle(const TrackId trackId, const muse::String& title)
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

bool Au3TracksInteraction::changeTracksColor(const TrackIdList& tracksIds, const std::string& color)
{
    for (const TrackId& trackId : tracksIds) {
        Au3Track* track = DomAccessor::findTrack(projectRef(), ::TrackId(trackId));
        IF_ASSERT_FAILED(track) {
            return false;
        }

        auto& trackColor = TrackColor::Get(track);
        trackColor.SetColor(muse::draw::Color::fromString(color));

        trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
        prj->notifyAboutTrackChanged(DomConverter::track(track));

        if (Au3WaveTrack* waveTrack = dynamic_cast<Au3WaveTrack*>(track)) {
            for (auto& clips: DomAccessor::waveClipsAsList(waveTrack)) {
                //Set it back to auto
                clips->SetColor("");
                prj->notifyAboutClipChanged(DomConverter::clip(waveTrack, clips.get()));
            }
        } else if (Au3LabelTrack* labelTrack = dynamic_cast<Au3LabelTrack*>(track)) {
            const auto& au3labels = labelTrack->GetLabels();
            for (size_t i = 0; i < au3labels.size(); ++i) {
                prj->notifyAboutLabelChanged(DomConverter::label(labelTrack, &au3labels[i]));
            }
        }
    }

    return true;
}

bool Au3TracksInteraction::changeTrackRulerType(const trackedit::TrackId& trackId, trackedit::TrackRulerType rulerType)
{
    Au3Track* track = DomAccessor::findTrack(projectRef(), Au3TrackId(trackId));
    IF_ASSERT_FAILED(track) {
        return false;
    }

    au::au3::TrackRulerTypeAttachment::Get(track).SetRulerType(rulerType);
    adjustVerticalZoom(trackId);
    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->notifyAboutTrackChanged(DomConverter::track(track));

    return true;
}

bool Au3TracksInteraction::changeAudioTrackViewType(const trackedit::TrackId& trackId, trackedit::TrackViewType viewType)
{
    Au3Track* track = DomAccessor::findTrack(projectRef(), Au3TrackId(trackId));
    IF_ASSERT_FAILED(track) {
        return false;
    }

    au3::TrackViewTypeAttachment::Get(track).SetTrackViewType(viewType);
    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->notifyAboutTrackChanged(DomConverter::track(track));

    return true;
}

muse::Ret Au3TracksInteraction::paste(const std::vector<ITrackDataPtr>& data, secs_t begin, bool moveClips, bool moveAllTracks,
                                      bool isMultiSelectionCopy, bool& projectWasModified)
{
    if (data.empty()) {
        return make_ret(trackedit::Err::TrackEmpty);
    }

    std::vector<std::shared_ptr<Au3TrackData> > copiedData(data.size());
    for (size_t i = 0; i < data.size(); ++i) {
        copiedData[i] = std::static_pointer_cast<Au3TrackData>(data[i]);
    }

    // Separate labels and clips
    std::vector<Au3TrackDataPtr> labelData;
    std::vector<Au3TrackDataPtr> clipData;

    for (const auto& trackData : copiedData) {
        if (trackData->track()) {
            if (dynamic_cast<Au3LabelTrack*>(trackData->track().get()) != nullptr) {
                labelData.push_back(trackData);
            } else {
                clipData.push_back(trackData);
            }
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
        projectWasModified = true;
    }

    TrackIdList selectedTracks = selectionController()->selectedTracks();
    if (selectedTracks.empty()) {
        const TrackIdList tracksIdsToSelect = pasteIntoNewTracks(copiedData);
        selectionController()->setSelectedTracks(tracksIdsToSelect);
        selectionController()->setFocusedTrack(tracksIdsToSelect.front());
        projectWasModified = true;
        return muse::make_ok();
    }

    muse::Ret result = muse::make_ok();
    TrackIdList finalSelectedTracks;
    TrackIdList originalSelectedTracks = selectedTracks; // Save original selection for clips

    // Paste labels if any
    if (!labelData.empty()) {
        TrackIdList dstTracksIds = determineDestinationTracksIds(tracks, originalSelectedTracks, copiedData, true /*for labels*/);
        result = pasteLabels(labelData, dstTracksIds, begin, moveClips, projectWasModified);
        if (!result) {
            return result;
        }
        // pasteLabels sets selection to the label track, save it
        finalSelectedTracks = selectionController()->selectedTracks();
    }

    // Paste clips if any
    if (!clipData.empty()) {
        // Use original selected tracks to determine destination for clips
        TrackIdList dstTracksIds = determineDestinationTracksIds(tracks, originalSelectedTracks, copiedData, false /*for labels*/);
        const bool pasteIntoExistingClip = !configuration()->pasteAsNewClip() && !moveAllTracks;
        result = pasteClips(clipData, dstTracksIds, begin, moveClips, isMultiSelectionCopy, pasteIntoExistingClip, projectWasModified);
        if (!result) {
            return result;
        }
        // If we also pasted labels, combine the selections
        if (!labelData.empty()) {
            TrackIdList clipSelectedTracks = selectionController()->selectedTracks();
            finalSelectedTracks.insert(finalSelectedTracks.end(), clipSelectedTracks.begin(), clipSelectedTracks.end());
            selectionController()->setSelectedTracks(finalSelectedTracks);
            selectionController()->setFocusedTrack(finalSelectedTracks.front());
        }
    }

    return result;
}

muse::Ret Au3TracksInteraction::pasteClips(const std::vector<Au3TrackDataPtr>& copiedData,
                                           const TrackIdList& dstTracksIds,
                                           secs_t begin,
                                           bool moveClips,
                                           bool isMultiSelectionCopy,
                                           bool pasteIntoExistingClip,
                                           bool& projectWasModified)
{
    // Convert Au3TrackDataPtr to ITrackDataPtr for helper method
    std::vector<ITrackDataPtr> copiedDataBase(copiedData.begin(), copiedData.end());

    if (clipsInteraction()->clipTransferNeedsDownmixing(copiedDataBase, dstTracksIds)
        && !clipsInteraction()->userIsOkWithDownmixing()) {
        return trackedit::make_ret(trackedit::Err::Cancel);
    }

    const bool newTracksNeeded = dstTracksIds.size() != copiedData.size();

    auto ret = canPasteTrackData(dstTracksIds, copiedData);
    if (!ret) {
        return ret;
    }

    muse::Ret ok { muse::make_ok() };
    projectWasModified = true;

    if (!moveClips) {
        if (isMultiSelectionCopy) {
            ok = clipsInteraction()->makeRoomForClipsOnTracks(dstTracksIds, copiedDataBase, begin);
        } else {
            ok = makeRoomForDataOnTracks(dstTracksIds, copiedData, begin, pasteIntoExistingClip);
        }
    }

    for (size_t i = 0; i < dstTracksIds.size(); ++i) {
        Au3WaveTrack* dstWaveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(dstTracksIds[i]));
        IF_ASSERT_FAILED(dstWaveTrack) {
            return make_ret(trackedit::Err::TrackNotFound);
        }
        auto prj = globalContext()->currentTrackeditProject();

        // use an unordered_set to store the IDs of the clips before the paste
        std::unordered_set<int> clipIdsBefore;
        for (const auto& clip : prj->clipList(dstTracksIds[i])) {
            clipIdsBefore.insert(clip.key.itemId);
        }

        const auto trackToPaste = std::static_pointer_cast<Au3WaveTrack>(copiedData.at(i)->track());

        if (dstWaveTrack->IsEmpty() && trackToPaste->NChannels() != dstWaveTrack->NChannels()) {
            auto& trackList = au3::Au3TrackList::Get(projectRef());
            dstWaveTrack = utils::toggleStereo(trackList, *dstWaveTrack);
            prj->trackChanged().send(DomConverter::track(dstWaveTrack));
        } else if (trackToPaste->NChannels() == 1 && dstWaveTrack->NChannels() == 2) {
            trackToPaste->MonoToStereo();
        } else if (trackToPaste->NChannels() == 2 && dstWaveTrack->NChannels() == 1) {
            ok = utils::withProgress(*interactive(), mixingDownToMonoLabel, [&](utils::ProgressCb progressCb, utils::CancelCb cancelCb)
            {
                return trackToPaste->MixDownToMono(progressCb, cancelCb);
            });
            if (!ok) {
                return ok;
            }
        }

        // If we have multiple clips, we want to insert them one by one
        // to keep any existing clips that may fall between them intact
        if (!moveClips && isMultiSelectionCopy) {
            trackToPaste->MoveTo(begin + trackToPaste->GetStartTime());
            for (const auto& interval : trackToPaste->Intervals()) {
                dstWaveTrack->InsertInterval(interval, false);
            }
        } else if (pasteIntoExistingClip
                   && clipsInteraction()->singleClipOnTrack(trackToPaste->GetId())
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
            if (clipIdsBefore.find(clip.key.itemId) == clipIdsBefore.end()) {
                prj->notifyAboutClipAdded(clip);
            }
        }
        prj->notifyAboutTrackChanged(DomConverter::track(dstWaveTrack));
    }

    if (newTracksNeeded) {
        // remove already pasted elements from the clipboard and paste the rest into the new tracks
        std::vector<Au3TrackDataPtr> remainingData(copiedData.begin() + dstTracksIds.size(), copiedData.end());
        const auto tracksIdsToSelect = pasteIntoNewTracks(remainingData);
        TrackIdList allDstTracksIds = dstTracksIds;
        allDstTracksIds.insert(allDstTracksIds.end(), tracksIdsToSelect.begin(), tracksIdsToSelect.end());

        selectionController()->setSelectedTracks(allDstTracksIds);
        selectionController()->setFocusedTrack(allDstTracksIds.front());
    } else {
        selectionController()->setSelectedTracks(dstTracksIds);
        selectionController()->setFocusedTrack(dstTracksIds.front());
    }

    return ok;
}

muse::Ret Au3TracksInteraction::pasteLabels(const std::vector<Au3TrackDataPtr>& copiedData,
                                            const TrackIdList& dstTracksIds,
                                            secs_t begin,
                                            bool moveClips,
                                            bool& projectWasModified)
{
    projectWasModified = true;
    auto prj = globalContext()->currentTrackeditProject();
    auto& tracks = Au3TrackList::Get(projectRef());

    const bool newTracksNeeded = dstTracksIds.size() != copiedData.size();

    for (size_t i = 0; i < dstTracksIds.size(); ++i) {
        Au3LabelTrack* targetLabelTrack = DomAccessor::findLabelTrack(projectRef(), Au3TrackId(dstTracksIds[i]));

        if (!targetLabelTrack) {
            targetLabelTrack = ::LabelTrack::Create(tracks);
            prj->notifyAboutTrackAdded(DomConverter::labelTrack(targetLabelTrack));
        }

        const auto trackToPaste = std::static_pointer_cast<Au3LabelTrack>(copiedData.at(i)->track());
        IF_ASSERT_FAILED(trackToPaste) {
            continue;
        }

        const auto& labelsBefore = targetLabelTrack->GetLabels();
        targetLabelTrack->Paste(begin, *trackToPaste, moveClips);

        const auto& labelsAfter = targetLabelTrack->GetLabels();
        for (size_t labelIdx = 0; labelIdx < labelsAfter.size(); ++labelIdx) {
            const auto it = std::find_if(labelsBefore.begin(), labelsBefore.end(), [&](const auto& label) {
                return label.GetId() == labelsAfter[labelIdx].GetId();
            });

            if (it == labelsBefore.end()) {
                prj->notifyAboutLabelAdded(DomConverter::label(targetLabelTrack, &labelsAfter[labelIdx]));
            }
        }

        prj->notifyAboutTrackChanged(DomConverter::track(targetLabelTrack));
    }

    if (newTracksNeeded) {
        // remove already pasted elements from the clipboard and paste the rest into the new tracks
        std::vector<Au3TrackDataPtr> remainingData(copiedData.begin() + dstTracksIds.size(), copiedData.end());
        const auto tracksIdsToSelect = pasteIntoNewTracks(remainingData);
        TrackIdList allDstTracksIds = dstTracksIds;
        allDstTracksIds.insert(allDstTracksIds.end(), tracksIdsToSelect.begin(), tracksIdsToSelect.end());

        selectionController()->setSelectedTracks(allDstTracksIds);
        selectionController()->setFocusedTrack(allDstTracksIds.front());
    } else {
        selectionController()->setSelectedTracks(dstTracksIds);
        selectionController()->setFocusedTrack(dstTracksIds.front());
    }

    return muse::make_ok();
}

ITrackDataPtr Au3TracksInteraction::cutTrackData(const TrackId trackId, secs_t begin, secs_t end, bool moveClips)
{
    std::shared_ptr<Au3Track> track;
    Au3Track* originTrack = nullptr;
    if (Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId))) {
        track = waveTrack->Cut(begin, end, moveClips);
        originTrack = waveTrack;
    } else if (Au3LabelTrack* labelTrack = DomAccessor::findLabelTrack(projectRef(), Au3TrackId(trackId))) {
        track = labelTrack->Cut(begin, end, moveClips);
        originTrack = labelTrack;
    }

    const auto data = std::make_shared<Au3TrackData>(std::move(track));

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->notifyAboutTrackChanged(DomConverter::track(originTrack));

    return data;
}

ITrackDataPtr Au3TracksInteraction::copyNonContinuousTrackData(const TrackId trackId, const TrackItemKeyList& itemKeys, secs_t offset)
{
    // Try to find wave track first
    if (Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId))) {
        auto& trackFactory = WaveTrackFactory::Get(projectRef());
        auto& pSampleBlockFactory = trackFactory.GetSampleBlockFactory();
        auto clipboardTrack = waveTrack->EmptyCopy(pSampleBlockFactory);

        for (const auto& itemKey : itemKeys) {
            if (std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, itemKey.itemId)) {
                clipboardTrack->InsertInterval(waveTrack->CopyClip(*clip, true), false);
            }
        }

        for (const auto& clip : clipboardTrack->SortedIntervalArray()) {
            clip->SetPlayStartTime(clip->GetPlayStartTime() + offset);
        }

        return std::make_shared<Au3TrackData>(std::move(clipboardTrack));
    }
    // Try to find label track
    else if (Au3LabelTrack* labelTrack = DomAccessor::findLabelTrack(projectRef(), Au3TrackId(trackId))) {
        auto& trackList = Au3TrackList::Get(projectRef());
        auto clipboardTrack = ::LabelTrack::CreatePtr(trackList);

        for (const auto& itemKey : itemKeys) {
            if (Au3Label* label = DomAccessor::findLabel(labelTrack, itemKey.itemId)) {
                SelectedRegion region;
                region.setTimes(label->getT0() + offset, label->getT1() + offset);
                clipboardTrack->AddLabel(region, label->title);
            }
        }

        return std::make_shared<Au3TrackData>(std::move(clipboardTrack));
    }

    return nullptr;
}

ITrackDataPtr Au3TracksInteraction::copyContinuousTrackData(const TrackId trackId, secs_t begin, secs_t end)
{
    std::shared_ptr<Au3Track> track;
    if (Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId))) {
        track = waveTrack->Copy(begin, end);
    } else if (Au3LabelTrack* labelTrack = DomAccessor::findLabelTrack(projectRef(), Au3TrackId(trackId))) {
        track = labelTrack->Copy(begin, end);
    }

    return std::make_shared<Au3TrackData>(std::move(track));
}

bool Au3TracksInteraction::removeTracksData(const TrackIdList& tracksIds, secs_t begin, secs_t end, bool moveClips)
{
    for (const TrackId& trackId : tracksIds) {
        Au3Track* track = nullptr;
        if (Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId))) {
            waveTrack->Clear(begin, end, moveClips);
            track = waveTrack;
        } else if (Au3LabelTrack* labelTrack = DomAccessor::findLabelTrack(projectRef(), Au3TrackId(trackId))) {
            labelTrack->Clear(begin, end, moveClips);
            track = labelTrack;
        }

        trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
        prj->notifyAboutTrackChanged(DomConverter::track(track));
    }

    return true;
}

bool Au3TracksInteraction::splitTracksAt(const TrackIdList& tracksIds, std::vector<secs_t> pivots)
{
    selectionController()->resetSelectedClips();

    for (const auto& trackId : tracksIds) {
        Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
        IF_ASSERT_FAILED(waveTrack) {
            return false;
        }

        bool didAnySplitOccur = false;

        for (const auto& pivot : pivots) {
            if (waveTrack->GetIntervalAtTime(pivot)) {
                waveTrack->SplitAt(pivot);
                didAnySplitOccur = true;
            }
        }

        if (didAnySplitOccur) {
            // Select the leftmost clip
            secs_t time = pivots.front();
            const auto sampleLength = 1. / waveTrack->GetRate();
            time -= sampleLength;

            auto clip = waveTrack->GetClipAtTime(time);

            if (clip) {
                ClipKey clipKey = DomConverter::clip(waveTrack, clip.get()).key;
                selectionController()->setSelectedClips({ clipKey }, true);
            }

            trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
            prj->notifyAboutTrackChanged(DomConverter::track(waveTrack));
        }
    }

    return true;
}

bool Au3TracksInteraction::splitRangeSelectionAtSilences(const TrackIdList& tracksIds, secs_t begin, secs_t end)
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

    return true;
}

bool Au3TracksInteraction::splitRangeSelectionIntoNewTracks(const TrackIdList& tracksIds, secs_t begin, secs_t end)
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

    return true;
}

bool Au3TracksInteraction::mergeSelectedOnTracks(const TrackIdList& tracksIds, secs_t begin, secs_t end)
{
    for (const auto& trackId : tracksIds) {
        bool ok = mergeSelectedOnTrack(trackId, begin, end);
        if (!ok) {
            return false;
        }
    }

    return true;
}

bool Au3TracksInteraction::duplicateSelectedOnTracks(const TrackIdList& tracksIds, secs_t begin, secs_t end)
{
    for (const auto& trackId : tracksIds) {
        bool ok = duplicateSelectedOnTrack(trackId, begin, end);
        if (!ok) {
            return false;
        }
    }

    return true;
}

std::vector<ITrackDataPtr> Au3TracksInteraction::splitCutSelectedOnTracks(const TrackIdList tracksIds, secs_t begin, secs_t end)
{
    std::vector<ITrackDataPtr> dataVector;
    dataVector.reserve(tracksIds.size());
    for (const auto& trackId : tracksIds) {
        auto data = splitCutSelectedOnTrack(trackId, begin, end);
        if (!data) {
            return {};
        }
        dataVector.push_back(data);
    }

    return dataVector;
}

bool Au3TracksInteraction::splitDeleteSelectedOnTracks(const TrackIdList tracksIds, secs_t begin, secs_t end)
{
    for (const auto& trackId : tracksIds) {
        bool ok = splitDeleteSelectedOnTrack(trackId, begin, end);
        if (!ok) {
            return false;
        }
    }

    return true;
}

bool Au3TracksInteraction::newMonoTrack()
{
    addWaveTrack(1);
    projectHistory()->pushHistoryState("Created new mono track", "New Mono Track");
    return true;
}

bool Au3TracksInteraction::newStereoTrack()
{
    addWaveTrack(2);
    projectHistory()->pushHistoryState("Created new stereo track", "New Stereo Track");
    return true;
}

bool Au3TracksInteraction::newLabelTrack()
{
    auto& tracks = Au3TrackList::Get(projectRef());
    Au3LabelTrack* track = ::LabelTrack::Create(tracks);

    const auto prj = globalContext()->currentTrackeditProject();
    prj->notifyAboutTrackAdded(DomConverter::labelTrack(track));

    selectionController()->setSelectedTracks({ track->GetId() });
    selectionController()->setFocusedTrack(track->GetId());

    return true;
}

bool Au3TracksInteraction::deleteTracks(const TrackIdList& trackIds)
{
    auto& project = projectRef();
    auto& tracks = Au3TrackList::Get(project);

    TrackId focusedTrack = selectionController()->focusedTrack();

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

    if (muse::contains(trackIds, focusedTrack)) {
        trackedit::ITrackeditProjectPtr trackEdit = globalContext()->currentTrackeditProject();
        const auto notRemovedTracks = trackEdit->trackIdList();
        selectionController()->setFocusedTrack(notRemovedTracks.empty() ? -1 : notRemovedTracks.front());
    }

    return true;
}

bool Au3TracksInteraction::duplicateTracks(const TrackIdList& trackIds)
{
    if (trackIds.empty()) {
        projectHistory()->pushHistoryState("Duplicated tracks", "Duplicate Tracks");
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

    return true;
}

bool Au3TracksInteraction::moveTracks(const TrackIdList& trackIds, const TrackMoveDirection direction)
{
    if (trackIds.empty()) {
        return false;
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

    return true;
}

bool Au3TracksInteraction::moveTracksTo(const TrackIdList& trackIds, int to)
{
    if (trackIds.empty()) {
        return false;
    }

    TrackIdList sortedTrackIds = trackIds;
    auto isAscending = (to > trackPosition(trackIds.front()));
    std::sort(sortedTrackIds.begin(), sortedTrackIds.end(), [this, isAscending](const TrackId& a, const TrackId& b) {
        return isAscending ? trackPosition(a) < trackPosition(b) : trackPosition(a) > trackPosition(b);
    });

    for (const auto& trackId : sortedTrackIds) {
        moveTrackTo(trackId, to);
    }

    return true;
}

bool Au3TracksInteraction::insertSilence(const TrackIdList& trackIds, secs_t begin, secs_t end, secs_t duration)
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

    return true;
}

bool Au3TracksInteraction::changeTracksFormat(const TrackIdList& tracksIds, trackedit::TrackFormat format)
{
    const size_t totalSamples = std::accumulate(tracksIds.begin(), tracksIds.end(), 0u, [this](size_t sum, const TrackId& trackId) {
        Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(trackId));
        IF_ASSERT_FAILED(waveTrack) {
            return sum;
        }
        return sum + WaveTrackUtilities::GetSequenceSamplesCount(*waveTrack).as_size_t();
    });
    size_t convertedSamples = 0;

    m_progress.start();

    muse::ProgressResult result;
    DEFER {
        m_progress.finish(result);
    };

    try {
        for (const TrackId& trackId : tracksIds) {
            Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(trackId));
            IF_ASSERT_FAILED(waveTrack) {
                return false;
            }

            sampleFormat newFormat;
            switch (format) {
            case au::trackedit::TrackFormat::Int16:
                newFormat = sampleFormat::int16Sample;
                break;
            case au::trackedit::TrackFormat::Int24:
                newFormat = sampleFormat::int24Sample;
                break;
            case au::trackedit::TrackFormat::Float32:
                newFormat = sampleFormat::floatSample;
                break;
            case au::trackedit::TrackFormat::Undefined:
            default:
                return false;
            }

            if (!(waveTrack->GetSampleFormat() == newFormat)) {
                waveTrack->ConvertToSampleFormat(newFormat,  [&](size_t sampleCnt) {
                    if (m_progress.isCanceled()) {
                        throw UserException();
                    }

                    convertedSamples += sampleCnt;
                    m_progress.progress(convertedSamples, totalSamples, "");
                });

                trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
                prj->notifyAboutTrackChanged(DomConverter::track(waveTrack));
            }
        }
    }
    catch (const UserException&) {
        PendingTracks::Get(projectRef()).ClearPendingTracks();
        return false;
    }
    return true;
}

bool Au3TracksInteraction::changeTracksRate(const TrackIdList& tracksIds, int rate)
{
    for (const TrackId& trackId : tracksIds) {
        Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(trackId));
        IF_ASSERT_FAILED(waveTrack) {
            return false;
        }

        if (waveTrack->GetRate() == rate) {
            continue;
        }
        waveTrack->SetRate(rate);
        trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
        prj->notifyAboutTrackChanged(DomConverter::track(waveTrack));
    }

    return true;
}

bool Au3TracksInteraction::swapStereoChannels(const TrackIdList& tracksIds)
{
    for (const TrackId& trackId : tracksIds) {
        Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(trackId));
        IF_ASSERT_FAILED(waveTrack) {
            return false;
        }

        if (waveTrack->Channels().size() != 2) {
            LOGW() << "Cannot swap stereo channels on a non-stereo track: " << trackId;
            continue;
        }

        waveTrack->SwapChannels();
        trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
        prj->notifyAboutTrackChanged(DomConverter::track(waveTrack));
    }

    return true;
}

bool Au3TracksInteraction::splitStereoTracksToLRMono(const TrackIdList& tracksIds)
{
    for (const TrackId& trackId : tracksIds) {
        Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(trackId));
        IF_ASSERT_FAILED(waveTrack) {
            return false;
        }

        if (waveTrack->Channels().size() != 2) {
            LOGW() << "Cannot split stereo channels on a non-stereo track: " << trackId;
            continue;
        }

        const std::vector<WaveTrack::Holder> unlinkedTracks = waveTrack->SplitChannels();
        IF_ASSERT_FAILED(unlinkedTracks.size() == 2) {
            LOGW() << "Failed to split stereo channels on track: " << trackId;
            continue;
        }

        unlinkedTracks[0]->SetPan(-1.0f);
        unlinkedTracks[1]->SetPan(1.0f);

        trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
        prj->notifyAboutTrackAdded(DomConverter::track(unlinkedTracks[0].get()));
        prj->notifyAboutTrackAdded(DomConverter::track(unlinkedTracks[1].get()));

        if (selectionController()->focusedTrack() == trackId) {
            selectionController()->setFocusedTrack(unlinkedTracks[0]->GetId());
        }

        const auto viewState = globalContext()->currentProject()->viewState();
        const int newHeight = std::max(viewState->trackHeight(trackId).val / 2, viewState->trackDefaultHeight());
        viewState->setTrackHeight(unlinkedTracks[0]->GetId(), newHeight);
        viewState->setTrackHeight(unlinkedTracks[1]->GetId(), newHeight);

        moveTracksTo({ unlinkedTracks[0]->GetId(), unlinkedTracks[1]->GetId() }, trackPosition(trackId));

        const auto originalTrack = DomConverter::track(waveTrack);
        auto& tracks = Au3TrackList::Get(projectRef());
        tracks.Remove(*waveTrack);
        prj->notifyAboutTrackRemoved(originalTrack);
    }

    return true;
}

bool Au3TracksInteraction::splitStereoTracksToCenterMono(const TrackIdList& tracksIds)
{
    for (const TrackId& trackId : tracksIds) {
        Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(trackId));
        IF_ASSERT_FAILED(waveTrack) {
            return false;
        }

        if (waveTrack->Channels().size() != 2) {
            LOGW() << "Cannot split stereo channels on a non-stereo track: " << trackId;
            continue;
        }

        const std::vector<WaveTrack::Holder> unlinkedTracks = waveTrack->SplitChannels();
        IF_ASSERT_FAILED(unlinkedTracks.size() == 2) {
            LOGW() << "Failed to split stereo channels on track: " << trackId;
            continue;
        }

        trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
        prj->notifyAboutTrackAdded(DomConverter::track(unlinkedTracks[0].get()));
        prj->notifyAboutTrackAdded(DomConverter::track(unlinkedTracks[1].get()));

        if (selectionController()->focusedTrack() == trackId) {
            selectionController()->setFocusedTrack(unlinkedTracks[0]->GetId());
        }

        const auto viewState = globalContext()->currentProject()->viewState();
        const int newHeight = std::max(viewState->trackHeight(trackId).val / 2, viewState->trackDefaultHeight());
        viewState->setTrackHeight(unlinkedTracks[0]->GetId(), newHeight);
        viewState->setTrackHeight(unlinkedTracks[1]->GetId(), newHeight);

        moveTracksTo({ unlinkedTracks[0]->GetId(), unlinkedTracks[1]->GetId() }, trackPosition(trackId));

        auto& tracks = Au3TrackList::Get(projectRef());
        const auto originalTrack = DomConverter::track(waveTrack);
        tracks.Remove(*waveTrack);
        prj->notifyAboutTrackRemoved(originalTrack);
    }

    return true;
}

bool Au3TracksInteraction::makeStereoTrack(const TrackId left, const TrackId right)
{
    const auto au3LeftTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(left));
    IF_ASSERT_FAILED(au3LeftTrack) {
        return false;
    }

    const auto au3RightTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(right));
    IF_ASSERT_FAILED(au3RightTrack) {
        return false;
    }

    if (au3LeftTrack->Channels().size() != 1 || au3RightTrack->Channels().size() != 1) {
        LOGW() << "Cannot combine non-mono tracks into stereo: " << left << ", " << right;
        return false;
    }

    if (!canMergeMonoTracksToStereo(left, right)) {
        if (!userIsOkCombineMonoToStereo()) {
            return false;
        }
    }

    float origPanLeft = au3LeftTrack->GetPan();
    float origPanRight = au3RightTrack->GetPan();

    au3LeftTrack->SetPan(-1.0f);
    au3RightTrack->SetPan(1.0f);

    Au3TrackList& tracks = Au3TrackList::Get(projectRef());
    const auto mix = MixAndRender(
        TrackIterRange {
        tracks.Any<const WaveTrack>().find(au3LeftTrack),
        ++tracks.Any<const WaveTrack>().find(au3RightTrack)
    },
        Mixer::WarpOptions { tracks.GetOwner() },
        au3LeftTrack->GetName(),
        &WaveTrackFactory::Get(projectRef()),
        //use highest sample rate
        std::max(au3LeftTrack->GetRate(), au3RightTrack->GetRate()),
        //use widest sample format
        std::max(au3LeftTrack->GetSampleFormat(), au3RightTrack->GetSampleFormat()),
        0.0, 0.0);

    if (!mix) {
        au3LeftTrack->SetPan(origPanLeft);
        au3RightTrack->SetPan(origPanRight);
        LOGW() << "Failed to mix and render stereo track from: " << left << ", " << right;
        return false;
    }

    const projectscene::IProjectViewStatePtr viewState = globalContext()->currentProject()->viewState();
    const int newTrackHeight = viewState->trackHeight(left).val + viewState->trackHeight(right).val;

    const Track leftTrack = DomConverter::track(au3LeftTrack);
    const Track rightTrack = DomConverter::track(au3RightTrack);

    ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    tracks.Append(mix, true);
    prj->notifyAboutTrackAdded(DomConverter::track(mix.get()));

    moveTracksTo({ mix->GetId() }, trackPosition(left));

    tracks.Remove(*au3LeftTrack);
    tracks.Remove(*au3RightTrack);

    viewState->setTrackHeight(mix->GetId(), newTrackHeight);

    prj->notifyAboutTrackRemoved(leftTrack);
    prj->notifyAboutTrackRemoved(rightTrack);

    return true;
}

bool Au3TracksInteraction::resampleTracks(const TrackIdList& tracksIds, int rate)
{
    size_t convertedSamples = 0;
    const size_t totalSamples = std::accumulate(tracksIds.begin(), tracksIds.end(), 0u, [this](size_t sum, const TrackId& trackId) {
        Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(trackId));
        IF_ASSERT_FAILED(waveTrack) {
            return sum;
        }
        return sum + WaveTrackUtilities::GetSequenceSamplesCount(*waveTrack).as_size_t();
    });

    m_progress.start();

    muse::ProgressResult result;
    DEFER {
        m_progress.finish(result);
    };

    try {
        for (const TrackId& trackId : tracksIds) {
            Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(trackId));
            IF_ASSERT_FAILED(waveTrack) {
                return false;
            }

            waveTrack->Resample(rate, [&](size_t sampleCnt) {
                if (m_progress.isCanceled()) {
                    throw UserException();
                }

                convertedSamples += sampleCnt;
                m_progress.progress(convertedSamples, totalSamples, "");
            });
            trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
            prj->notifyAboutTrackChanged(DomConverter::track(waveTrack));
        }
    }
    catch (const UserException&) {
        PendingTracks::Get(projectRef()).ClearPendingTracks();
        return false;
    }

    return true;
}

float Au3TracksInteraction::maxVerticalZoom(const trackedit::Track& track) const
{
    constexpr int MIN_DISTANCE_FROM_RANGE = 6;
    constexpr int DB_PER_STEP = 6;

    if (track.rulerType == trackedit::TrackRulerType::DbLog) {
        const int dBRange = static_cast<int>(playback::PlaybackMeterDbRange::toDouble(playbackConfiguration()->playbackMeterDbRange()));
        const int steps = (-dBRange - MIN_DISTANCE_FROM_RANGE) / DB_PER_STEP;
        return std::max(MIN_VERTICAL_RANGE, 1.0f / (1 << steps));
    }

    return MIN_VERTICAL_RANGE;
}

void Au3TracksInteraction::zoomInVertically(const trackedit::TrackId& trackId)
{
    Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(trackId));
    if (waveTrack == nullptr) {
        return;
    }

    trackedit::Track track = DomConverter::track(waveTrack);
    float maxZoom = maxVerticalZoom(track);

    float min = 0.0f;
    float max = 0.0f;
    auto& cache = WaveformScale::Get(*waveTrack);
    cache.GetDisplayBounds(min, max);
    if (muse::is_equal(max, maxZoom)) {
        return;
    }

    cache.SetDisplayBounds(min / 2, max / 2);

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->notifyAboutTrackChanged(DomConverter::track(waveTrack));
}

void Au3TracksInteraction::zoomOutVertically(const trackedit::TrackId& trackId)
{
    Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(trackId));
    if (waveTrack == nullptr) {
        return;
    }

    float min = 0.0f;
    float max = 0.0f;
    auto& cache = WaveformScale::Get(*waveTrack);
    cache.GetDisplayBounds(min, max);
    if (muse::is_equal(max, MAX_VERTICAL_RANGE)) {
        return;
    }

    cache.SetDisplayBounds(min * 2, max * 2);

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->notifyAboutTrackChanged(DomConverter::track(waveTrack));
}

void Au3TracksInteraction::resetVerticalZoom(const trackedit::TrackId& trackId)
{
    Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(trackId));
    if (waveTrack == nullptr) {
        return;
    }

    float min = 0.0f;
    float max = 0.0f;
    auto& cache = WaveformScale::Get(*waveTrack);
    cache.GetDisplayBounds(min, max);
    cache.SetDisplayBounds(muse::RealIsEqual(min, 0.0f) ? 0.0f : -DEFAULT_VERTICAL_RANGE, DEFAULT_VERTICAL_RANGE);

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->notifyAboutTrackChanged(DomConverter::track(waveTrack));
}

bool Au3TracksInteraction::isDefaultVerticalZoom(const trackedit::TrackId& trackId) const
{
    Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(trackId));
    if (waveTrack == nullptr) {
        return false;
    }

    float min = 0.0f;
    float max = 0.0f;
    auto& cache = WaveformScale::Get(*waveTrack);
    cache.GetDisplayBounds(min, max);
    return muse::is_equal(max, DEFAULT_VERTICAL_RANGE);
}

bool Au3TracksInteraction::isMaxVerticalZoom(const trackedit::TrackId& trackId) const
{
    Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(trackId));
    if (waveTrack == nullptr) {
        return false;
    }

    trackedit::Track track = DomConverter::track(waveTrack);
    float maxZoom = maxVerticalZoom(track);

    float min;
    float max;
    auto& cache = WaveformScale::Get(*waveTrack);
    cache.GetDisplayBounds(min, max);
    return muse::is_equal(max, maxZoom);
}

bool Au3TracksInteraction::isMinVerticalZoom(const trackedit::TrackId& trackId) const
{
    Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(trackId));
    if (waveTrack == nullptr) {
        return false;
    }

    float min;
    float max;
    auto& cache = WaveformScale::Get(*waveTrack);
    cache.GetDisplayBounds(min, max);
    return muse::is_equal(max, MAX_VERTICAL_RANGE);
}

void Au3TracksInteraction::adjustVerticalZoom(const trackedit::TrackId& trackId)
{
    Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(trackId));
    if (waveTrack == nullptr) {
        return;
    }

    trackedit::Track track = DomConverter::track(waveTrack);
    float maxZoom = maxVerticalZoom(track);

    float min;
    float max;
    auto& cache = WaveformScale::Get(*waveTrack);
    cache.GetDisplayBounds(min, max);
    if (muse::is_equal(max, maxZoom)) {
        return;
    }

    if (max < maxZoom) {
        cache.SetDisplayBounds(-maxZoom, maxZoom);
    }

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->notifyAboutTrackChanged(DomConverter::track(waveTrack));
}

void Au3TracksInteraction::toggleHalfWave(const trackedit::TrackId& trackId)
{
    Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(trackId));
    if (waveTrack == nullptr) {
        return;
    }

    float min;
    float max;
    auto& cache = WaveformScale::Get(*waveTrack);
    cache.GetDisplayBounds(min, max);

    cache.SetDisplayBounds(muse::is_equal(min, 0.0f) ? -max : 0.0f, max);

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->notifyAboutTrackChanged(DomConverter::track(waveTrack));
}

bool Au3TracksInteraction::isHalfWave(const trackedit::TrackId& trackId) const
{
    Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(trackId));
    if (waveTrack == nullptr) {
        return false;
    }

    float min;
    float max;
    auto& cache = WaveformScale::Get(*waveTrack);
    cache.GetDisplayBounds(min, max);

    return muse::is_equal(min, 0.0f);
}

double Au3TracksInteraction::nearestZeroCrossing(double t0) const
{
    auto& project = projectRef();
    auto rate = ProjectRate::Get(project).GetRate();
    auto& tracks = ::TrackList::Get(project);

    // Window is 1/100th of a second.
    auto windowSize = size_t(std::max(1.0, rate / 100));
    Floats dist{ windowSize, true };

    int nTracks = 0;
    for (auto one : tracks.Selected<const WaveTrack>()) {
        const auto nChannels = one->NChannels();
        auto oneWindowSize = size_t(std::max(1.0, one->GetRate() / 100));
        Floats buffer1{ oneWindowSize };
        Floats buffer2{ oneWindowSize };
        float* const buffers[]{ buffer1.get(), buffer2.get() };
        auto s = one->TimeToLongSamples(t0);

        // fillTwo to ensure that missing values are treated as 2, and hence do
        // not get used as zero crossings.
        one->GetFloats(0, nChannels, buffers,
                       s - (int)oneWindowSize / 2, oneWindowSize, false, FillFormat::fillTwo);

        // Looking for actual crossings.  Update dist
        for (size_t iChannel = 0; iChannel < nChannels; ++iChannel) {
            const auto oneDist = buffers[iChannel];
            double prev = 2.0;
            for (size_t i = 0; i < oneWindowSize; ++i) {
                float fDist = fabs(oneDist[i]); // score is absolute value
                if (prev * oneDist[i] > 0) { // both same sign?  No good.
                    fDist = fDist + 0.4; // No good if same sign.
                } else if (prev > 0.0) {
                    fDist = fDist + 0.1; // medium penalty for downward crossing.
                }
                prev = oneDist[i];
                oneDist[i] = fDist;
            }

            // TODO: The mixed rate zero crossing code is broken,
            // if oneWindowSize > windowSize we'll miss out some
            // samples - so they will still be zero, so we'll use them.
            for (size_t i = 0; i < windowSize; i++) {
                size_t j;
                if (windowSize != oneWindowSize) {
                    j = i * (oneWindowSize - 1) / (windowSize - 1);
                } else {
                    j = i;
                }

                dist[i] += oneDist[j];
                // Apply a small penalty for distance from the original endpoint
                // We'll always prefer an upward
                dist[i] +=0.1 * (abs(int(i) - int(windowSize / 2))) / float(windowSize / 2);
            }
        }
        nTracks++;
    }

    // Find minimum
    int argmin = 0;
    float min = 3.0;
    for (size_t i = 0; i < windowSize; ++i) {
        if (dist[i] < min) {
            argmin = i;
            min = dist[i];
        }
    }

    // If we're worse than 0.2 on average, on one track, then no good.
    if ((nTracks == 1) && (min > (0.2 * nTracks))) {
        return t0;
    }
    // If we're worse than 0.6 on average, on multi-track, then no good.
    if ((nTracks > 1) && (min > (0.6 * nTracks))) {
        return t0;
    }

    return t0 + (argmin - (int)windowSize / 2) / rate;
}

TrackIdList Au3TracksInteraction::pasteIntoNewTracks(const std::vector<Au3TrackDataPtr>& tracksData)
{
    auto& project = projectRef();
    auto& tracks = Au3TrackList::Get(project);
    auto prj = globalContext()->currentTrackeditProject();
    const muse::secs_t selectedStartTime = playbackState()->playbackPosition();

    TrackIdList tracksIdsPastedInto;

    Au3Track* pFirstNewTrack = nullptr;
    for (const auto& data : tracksData) {
        auto pNewTrack = createNewTrackAndPaste(data->track(), tracks, selectedStartTime);
        if (!pFirstNewTrack) {
            pFirstNewTrack = pNewTrack.get();
        }

        auto newTrack = DomConverter::track(pNewTrack.get());
        prj->notifyAboutTrackAdded(newTrack);

        // Handle wave track clips
        if (dynamic_cast<Au3WaveTrack*>(pNewTrack.get())) {
            for (const auto& clip : prj->clipList(pNewTrack->GetId())) {
                prj->notifyAboutClipAdded(clip);
            }
        }
        // Handle label track labels
        else if (Au3LabelTrack* labelTrack = dynamic_cast<Au3LabelTrack*>(pNewTrack.get())) {
            const auto& labels = labelTrack->GetLabels();
            for (size_t i = 0; i < labels.size(); ++i) {
                prj->notifyAboutLabelAdded(DomConverter::label(labelTrack, &labels[i]));
            }
        }

        tracksIdsPastedInto.push_back(newTrack.id);
    }

    return tracksIdsPastedInto;
}

std::shared_ptr<au::au3::Au3Track> Au3TracksInteraction::createNewTrackAndPaste(std::shared_ptr<Au3Track> track, Au3TrackList& list,
                                                                                secs_t begin)
{
    // Handle wave track clips
    if (Au3WaveTrack* waveTrack = dynamic_cast<Au3WaveTrack*>(track.get())) {
        auto& trackFactory = WaveTrackFactory::Get(projectRef());
        auto& pSampleBlockFactory = trackFactory.GetSampleBlockFactory();

        auto pFirstTrack = waveTrack->EmptyCopy(pSampleBlockFactory);
        list.Add(pFirstTrack->SharedPointer());
        pFirstTrack->Paste(begin, *track, false);
        return pFirstTrack->SharedPointer();
    }
    // Handle label track labels
    else if (dynamic_cast<Au3LabelTrack*>(track.get())) {
        Au3LabelTrack* newLabelTrack = ::LabelTrack::Create(list);
        newLabelTrack->Paste(begin, *track, false);
        return newLabelTrack->SharedPointer();
    }

    return nullptr;
}

TrackIdList Au3TracksInteraction::determineDestinationTracksIds(const std::vector<Track>& tracks, const TrackIdList& destinationTrackIds,
                                                                const std::vector<Au3TrackDataPtr>& clipboardData,
                                                                bool forLabels) const
{
    //! NOTE: determine tracks to which clipboard content will be pasted,
    //! depending on clipboard size, types and currently selected tracks

    if (clipboardData.empty()) {
        return TrackIdList();
    }

    bool hasLabelTrack = std::any_of(clipboardData.begin(), clipboardData.end(),
                                     [](const auto& data) {
        return data->track()
               && DomConverter::track(data->track().get()).type == TrackType::Label;
    });

    auto matchesFilter = [forLabels](TrackType trackType) -> bool {
        return forLabels ? trackType == TrackType::Label
               : trackType == TrackType::Mono || trackType == TrackType::Stereo;
    };

    //! NOTE: If there's a label track in clipboard, match tracks strictly by position and type
    if (hasLabelTrack && tracks.size() == clipboardData.size()) {
        TrackIdList result;

        for (const auto& track : tracks) {
            if (matchesFilter(track.type)) {
                result.push_back(track.id);
            }
        }

        return result;
    }

    //! NOTE: Filter destinationTrackIds to only include tracks that match the filter
    TrackIdList filteredDestinationTrackIds;
    for (const auto& trackId : destinationTrackIds) {
        auto it = std::find_if(tracks.begin(), tracks.end(), [trackId](const Track& track) {
            return track.id == trackId;
        });

        if (it != tracks.end() && matchesFilter(it->type)) {
            filteredDestinationTrackIds.push_back(trackId);
        }
    }

    size_t clipboardTracksSize = clipboardData.size();
    if (filteredDestinationTrackIds.size() > clipboardTracksSize) {
        //! NOTE: more tracks selected than needed, return sub-vector
        return TrackIdList(filteredDestinationTrackIds.begin(), filteredDestinationTrackIds.begin() + clipboardTracksSize);
    }

    if (filteredDestinationTrackIds.size() == clipboardTracksSize) {
        //! NOTE: selected tracks size matches clipboard size
        return filteredDestinationTrackIds;
    }

    //! NOTE: not enough tracks selected, add more consecutively
    TrackIdList result = filteredDestinationTrackIds;
    bool collecting = false;

    for (const auto& track : tracks) {
        if (!collecting && track.id == destinationTrackIds.back()) {
            collecting = true;
            continue;
        }
        if (collecting && matchesFilter(track.type)) {
            result.push_back(track.id);
            if (result.size() >= clipboardTracksSize) {
                break;
            }
        }
    }

    //! NOTE: if insufficient tracks are available in `tracks`, result may still be shorter than clipboard size
    return result;
}

bool Au3TracksInteraction::userIsOkCombineMonoToStereo() const
{
    const std::string title = muse::trc("trackedit", "Combine mono tracks to stereo");
    const std::string body = muse::trc("trackedit",
                                       "The tracks you are attempting to merge to stereo contain clips at "
                                       "different positions, or otherwise mismatching clips. Merging them "
                                       "will render the tracks.\n\n"
                                       "This causes any realtime effects to be applied to the waveform and "
                                       "hidden data to be removed. Additionally, the entire track will "
                                       "become one large clip.\n\n"
                                       "Do you wish to continue?");

    const muse::IInteractive::Result result = interactive()->warningSync(title, body, {
        muse::IInteractive::Button::Cancel,
        muse::IInteractive::Button::Yes
    }, muse::IInteractive::Button::Cancel);

    return result.standardButton() == muse::IInteractive::Button::Yes;
}

bool Au3TracksInteraction::canMergeMonoTracksToStereo(const TrackId left, const TrackId right)
{
    const auto au3LeftTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(left));
    IF_ASSERT_FAILED(au3LeftTrack) {
        return false;
    }

    const auto au3RightTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(right));
    IF_ASSERT_FAILED(au3RightTrack) {
        return false;
    }

    const auto checkAligned = [](const WaveTrack& left, const WaveTrack& right)
    {
        auto eqTrims = [](double a, double b)
        {
            return std::abs(a - b)
                   <= std::numeric_limits<double>::epsilon() * std::max(a, b);
        };
        const auto eps = 0.5 / left.GetRate();
        const auto& rightIntervals = right.Intervals();
        for (const auto& a : left.Intervals()) {
            auto it = std::find_if(
                rightIntervals.begin(),
                rightIntervals.end(),
                [&](const auto& b)
            {
                //Start() and End() are always snapped to a sample grid
                return std::abs(a->Start() - b->Start()) < eps
                       && std::abs(a->End() - b->End()) < eps
                       && eqTrims(a->GetTrimLeft(), b->GetTrimLeft())
                       && eqTrims(a->GetTrimRight(), b->GetTrimRight())
                       && a->HasEqualPitchAndSpeed(*b);
            });
            if (it == rightIntervals.end()) {
                return false;
            }
        }
        return true;
    };

    return RealtimeEffectList::Get(*au3LeftTrack).GetStatesCount() == 0
           && RealtimeEffectList::Get(*au3RightTrack).GetStatesCount() == 0
           && checkAligned(*au3LeftTrack, *au3RightTrack);
}

muse::Ret Au3TracksInteraction::canPasteTrackData(const TrackIdList& dstTracksIds, const std::vector<Au3TrackDataPtr>& clipsToPaste) const
{
    IF_ASSERT_FAILED(dstTracksIds.size() <= clipsToPaste.size()) {
        return make_ret(trackedit::Err::NotEnoughDataInClipboard);
    }

    for (size_t i = 0; i < dstTracksIds.size(); ++i) {
        Au3WaveTrack* dstWaveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(dstTracksIds[i]));
        IF_ASSERT_FAILED(dstWaveTrack) {
            return make_ret(trackedit::Err::TrackNotFound);
        }
    }

    return muse::make_ok();
}

muse::Ret Au3TracksInteraction::makeRoomForDataOnTracks(const std::vector<TrackId>& tracksIds,
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
            return make_ret(trackedit::Err::TrackNotFound);
        }

        const auto trackToPaste = std::static_pointer_cast<Au3WaveTrack>(trackData.at(i)->track());

        //! NOTE need to snap begin just like Paste() function do
        secs_t snappedBegin = dstWaveTrack->SnapToSample(begin);
        secs_t insertDuration = trackData.at(i)->track()->GetEndTime();

        // if paste into existing clip and there is a single clip to paste,
        // we need to make room for the clip to be extended
        if (pasteIntoExistingClip
            && clipsInteraction()->singleClipOnTrack(trackToPaste->GetId())
            && dstWaveTrack->GetClipAtTime(begin) != nullptr) {
            secs_t currentClipEnd = dstWaveTrack->GetClipAtTime(begin)->GetPlayEndTime();
            snappedBegin = dstWaveTrack->SnapToSample(currentClipEnd);
        }

        auto ok = clipsInteraction()->makeRoomForDataOnTrack(tracksIds.at(i), snappedBegin, snappedBegin + insertDuration);
        if (!ok) {
            return make_ret(trackedit::Err::FailedToMakeRoomForClip);
        }
    }

    return muse::make_ok();
}

bool Au3TracksInteraction::mergeSelectedOnTrack(const TrackId trackId, secs_t begin, secs_t end)
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

bool Au3TracksInteraction::duplicateSelectedOnTrack(const TrackId trackId, secs_t begin, secs_t end)
{
    auto& tracks = Au3TrackList::Get(projectRef());

    std::shared_ptr<Au3Track> dest;

    if (Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId))) {
        dest = waveTrack->Copy(begin, end, false);
        dest->MoveTo(std::max(static_cast<double>(begin), waveTrack->GetStartTime()));
        tracks.Add(dest);
    } else if (Au3LabelTrack* labelTrack = DomAccessor::findLabelTrack(projectRef(), Au3TrackId(trackId))) {
        dest = labelTrack->Copy(begin, end, false);
        dest->MoveTo(std::max(static_cast<double>(begin), labelTrack->GetStartTime()));
        tracks.Add(dest);
    }

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->notifyAboutTrackAdded(DomConverter::track(dest.get()));

    return true;
}

void Au3TracksInteraction::doInsertSilence(const TrackIdList& trackIds, secs_t begin, secs_t end, secs_t duration)
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

void Au3TracksInteraction::insertBlankSpace(const TrackIdList& trackIds, secs_t begin, secs_t duration)
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

ITrackDataPtr Au3TracksInteraction::splitCutSelectedOnTrack(const TrackId trackId, secs_t begin, secs_t end)
{
    std::shared_ptr<Au3TrackData> data;
    Au3Track* changedTrack = nullptr;

    if (Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId))) {
        const auto& newTrack = waveTrack->SplitCut(begin, end);
        data = std::make_shared<Au3TrackData>(std::move(newTrack));
        changedTrack = waveTrack;
    } else if (Au3LabelTrack* labelTrack = DomAccessor::findLabelTrack(projectRef(), Au3TrackId(trackId))) {
        const auto& newTrack = labelTrack->SplitCut(begin, end);
        data = std::make_shared<Au3TrackData>(std::move(newTrack));
        changedTrack = labelTrack;
    }

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->notifyAboutTrackChanged(DomConverter::track(changedTrack));

    return data;
}

bool Au3TracksInteraction::splitDeleteSelectedOnTrack(const TrackId trackId, secs_t begin, secs_t end)
{
    Au3Track* changedTrack = nullptr;

    if (Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId))) {
        waveTrack->SplitDelete(begin, end);
        changedTrack = waveTrack;
    } else if (Au3LabelTrack* labelTrack = DomAccessor::findLabelTrack(projectRef(), Au3TrackId(trackId))) {
        labelTrack->SplitDelete(begin, end);
        changedTrack = labelTrack;
    }

    if (!changedTrack) {
        return false;
    }

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->notifyAboutTrackChanged(DomConverter::track(changedTrack));

    return true;
}

bool Au3TracksInteraction::canMoveTrack(const TrackId trackId, const TrackMoveDirection direction)
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

void Au3TracksInteraction::moveTrack(const TrackId trackId, const TrackMoveDirection direction)
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

void Au3TracksInteraction::moveTrackTo(const TrackId trackId, int to)
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

void Au3TracksInteraction::addWaveTrack(int numChannels)
{
    const auto track = utils::appendWaveTrack(Au3TrackList::Get(projectRef()), numChannels);

    const auto prj = globalContext()->currentTrackeditProject();
    prj->notifyAboutTrackAdded(DomConverter::track(track));

    selectionController()->setSelectedTracks({ track->GetId() });
    selectionController()->setFocusedTrack(track->GetId());
}

int Au3TracksInteraction::trackPosition(const TrackId trackId)
{
    auto& project = projectRef();
    auto track = DomAccessor::findTrack(project, Au3TrackId(trackId));
    IF_ASSERT_FAILED(track) {
        return -1;
    }

    auto& tracks = Au3TrackList::Get(project);
    return static_cast<int>(utils::getTrackIndex(tracks, Au3TrackId(trackId)));
}

muse::Progress Au3TracksInteraction::progress() const
{
    return m_progress;
}
