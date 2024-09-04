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

using namespace au::trackedit;
using namespace au::au3;

Clipboard Au3Interaction::s_clipboard;

AudacityProject& Au3Interaction::projectRef() const
{
    AudacityProject* project = reinterpret_cast<AudacityProject*>(globalContext()->currentProject()->au3ProjectPtr());
    return *project;
}

bool Au3Interaction::pasteIntoNewTrack()
{
    auto &project = projectRef();
    auto &tracks = ::TrackList::Get( project );
    auto prj = globalContext()->currentTrackeditProject();
    secs_t selectedStartTime = globalContext()->playbackState()->playbackPosition();

    ::Track* pFirstNewTrack = NULL;
    for (auto data : s_clipboard.data) {
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
}

::Track::Holder Au3Interaction::createNewTrackAndPaste(std::shared_ptr<::Track> track, ::TrackList &list, secs_t begin)
{
    auto &trackFactory = WaveTrackFactory::Get(projectRef());
    auto &pSampleBlockFactory = trackFactory.GetSampleBlockFactory();

    WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(DomConverter::trackId(track->GetId())));
    IF_ASSERT_FAILED(waveTrack) {
        return nullptr;
    }
    auto pFirstTrack = waveTrack->EmptyCopy(pSampleBlockFactory);
    list.Add(pFirstTrack->SharedPointer());
    pFirstTrack->Paste(begin, *track);
    return pFirstTrack->SharedPointer();
}

std::vector<au::trackedit::TrackId> Au3Interaction::determineDestinationTracksIds(const std::vector<Track> &tracks, TrackId destinationTrackId, size_t tracksNum) const
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

bool Au3Interaction::canPasteClips(const std::vector<TrackId> &dstTracksIds, secs_t begin) const
{
    for (size_t i = 0; i < dstTracksIds.size(); ++i) {
        WaveTrack* dstWaveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(dstTracksIds[i]));
        IF_ASSERT_FAILED(dstWaveTrack) {
            return false;
        }

        WaveTrack* origWaveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(s_clipboard.data[i].track->GetId()));
        IF_ASSERT_FAILED(origWaveTrack) {
            return false;
        }

        secs_t insertDuration = 0;

        // intentional comparison, see clipId default value
        if (s_clipboard.data[i].clipKey.clipId == -1) {
            // handle multiple clips
            std::shared_ptr<WaveClip> leftClip = origWaveTrack->GetLeftmostClip();
            std::shared_ptr<WaveClip> rightClip = origWaveTrack->GetRightmostClip();
            insertDuration = rightClip->End() - leftClip->Start();
        } else {
            //handle single clip
            std::shared_ptr<WaveClip> clip = DomAccessor::findWaveClip(origWaveTrack, s_clipboard.data[i].clipKey.clipId);
            IF_ASSERT_FAILED(clip) {
                return false;
            }

            insertDuration = clip->End() - clip->Start();
        }

        // throws incosistency exception if project tempo is not set, see:
        // au4/src/au3wrap/internal/au3project.cpp: 92
        // Paste func throws exception if there's not enough space to paste in (exception handler calls BasicUI logic)
        if (!dstWaveTrack->IsEmpty(begin, begin + insertDuration)) {
            LOGDA() << "not enough space to paste clip into";
            //! TODO AU4: show dialog
            return false;
        }
    }

    return true;
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
    s_clipboard.data.clear();
}

bool Au3Interaction::pasteFromClipboard(secs_t begin, TrackId destinationTrackId)
{
    if (s_clipboard.data.empty()) {
        return false;
    }

    if (destinationTrackId == -1) {
        return pasteIntoNewTrack();
    }

    project::IAudacityProjectPtr project = globalContext()->currentProject();
    //! TODO: we need to make sure that we get a trackList with order
    //! the same as in the TrackPanel
    auto tracks = project->trackeditProject()->trackList();
    size_t tracksNum = s_clipboard.data.size();

    // for multiple tracks copying
    std::vector<TrackId> dstTracksIds = determineDestinationTracksIds(tracks, destinationTrackId, tracksNum);

    bool newTracksNeeded = false;
    size_t newTracksCount = 0;
    if (dstTracksIds.size() != tracksNum) {
        newTracksNeeded = true;
        newTracksCount = tracksNum - dstTracksIds.size();
    }

    // check if copied data fits into selected area
    if (!canPasteClips(dstTracksIds, begin)) {
        return false;
    }

    for (size_t i = 0; i < dstTracksIds.size(); ++i) {
        WaveTrack* dstWaveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(dstTracksIds[i]));
        IF_ASSERT_FAILED(dstWaveTrack) {
            return false;
        }
        auto prj = globalContext()->currentTrackeditProject();

        // use an unordered_set to store the IDs of the clips before the paste
        std::unordered_set<int> clipIdsBefore;
        for (const auto& clip : prj->clipList(dstTracksIds[i])) {
            clipIdsBefore.insert(clip.key.clipId);
        }

        dstWaveTrack->Paste(begin, *s_clipboard.data[i].track);

        // Check which clips were added and trigger the onClipAdded event
        for (const auto& clip : prj->clipList(dstTracksIds[i])) {
            if (clipIdsBefore.find(clip.key.clipId) == clipIdsBefore.end()) {
                prj->onClipAdded(clip);
            }
        }
    }

    if (newTracksNeeded) {
        // remove already pasted elements from the clipboard and paste the rest into the new tracks
        s_clipboard.data.erase(s_clipboard.data.begin(), s_clipboard.data.begin() + dstTracksIds.size());
        return pasteIntoNewTrack();
    }

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
    s_clipboard.data.push_back(TrackData { track, clipKey });

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
    s_clipboard.data.push_back(TrackData { track, clipKey });

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
    s_clipboard.data.push_back(TrackData { track, dummyClipKey });
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

    clip->Clear(clip->Start(), clip->End());

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->onClipRemoved(DomConverter::clip(waveTrack, clip.get()));

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

    trackedit::secs_t initialClipStart = clip->Start();
    trackedit::secs_t initialClipEnd = clip->End();

    clip->Clear(begin, end);

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    if (begin <= initialClipStart && end >= initialClipEnd) {
        prj->onClipRemoved(DomConverter::clip(waveTrack, clip.get()));
    } else {
        prj->onClipChanged(DomConverter::clip(waveTrack, clip.get()));
    }

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

    prj->pushHistoryState("Split", "Split");

    return true;
}

bool Au3Interaction::trimClipLeft(const ClipKey &clipKey, secs_t deltaSec)
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

    return true;
}

bool Au3Interaction::trimClipRight(const ClipKey &clipKey, secs_t deltaSec)
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

    return true;
}

void Au3Interaction::newMonoTrack()
{
    auto &project = projectRef();
    auto &tracks = ::TrackList::Get(project);
    auto &trackFactory = ::WaveTrackFactory::Get(project);

    sampleFormat defaultFormat = QualitySettings::SampleFormatChoice();
    auto rate = ::ProjectRate::Get(project).GetRate();

    auto track = trackFactory.Create(defaultFormat, rate);
    track->SetName(tracks.MakeUniqueTrackName(WaveTrack::GetDefaultAudioTrackNamePreference()));
    //! TODO AU4: find out why have to set tempo for the track right there
    //! should be set automatically to project tempo value
    DoProjectTempoChange(*track, 120);
    tracks.Add(track);

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->onTrackAdded(DomConverter::track(track.get()));

    selectionController()->setSelectedTrack(DomConverter::trackId(track->GetId()));
}

void Au3Interaction::newStereoTrack()
{
    auto &project = projectRef();
    auto &tracks = ::TrackList::Get(project);
    auto &trackFactory = ::WaveTrackFactory::Get(project);

    sampleFormat defaultFormat = QualitySettings::SampleFormatChoice();
    auto rate = ::ProjectRate::Get(project).GetRate();

    tracks.Add(trackFactory.Create(2, defaultFormat, rate));
    auto &newTrack = **tracks.rbegin();
    newTrack.SetName(tracks.MakeUniqueTrackName(WaveTrack::GetDefaultAudioTrackNamePreference()));
    //! TODO AU4: find out why have to set tempo for the track right there
    //! should be set automatically to project tempo value
    DoProjectTempoChange(newTrack, 120);

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
