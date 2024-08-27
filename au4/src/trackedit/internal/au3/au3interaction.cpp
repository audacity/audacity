#include "au3interaction.h"

#include <algorithm>

#include "ProjectRate.h"
#include "QualitySettings.h"
#include "global/types/number.h"

#include "libraries/lib-project/Project.h"
#include "libraries/lib-wave-track/WaveTrack.h"
#include "libraries/lib-track/Track.h"
#include "libraries/lib-wave-track/WaveClip.h"

#include "au3wrap/internal/domaccessor.h"
#include "au3wrap/internal/domconverter.h"
#include "au3wrap/internal/wxtypes_convert.h"

#include "log.h"

using namespace au::trackedit;
using namespace au::au3;

Clipboard Au3Interaction::s_clipboard;

AudacityProject& Au3Interaction::projectRef() const
{
    AudacityProject* project = reinterpret_cast<AudacityProject*>(globalContext()->currentProject()->au3ProjectPtr());
    return *project;
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

bool Au3Interaction::changeClipStartTime(const trackedit::ClipKey& clipKey, double newStartTime, bool completed)
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
                double nextStartTime = nextClip->GetPlayStartTime();
                double deltaSec = newStartTime - clip->GetPlayStartTime();

                // check collision
                if ((clip->GetPlayEndTime() + deltaSec) > nextStartTime) {
                    //! NOTE If we have reached the limit, we do nothing.
                    //! TODO maybe there is a problem here, the end time of the clip may coincide with the start time of the next clip
                    if (muse::is_equal(clip->GetPlayEndTime(), nextStartTime)) {
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
            double prevEndTime = 0.0;
            if (it != clips.begin()) {
                auto prevIt = --it;
                std::shared_ptr<WaveClip> pervClip = *prevIt;
                prevEndTime = pervClip->GetPlayEndTime();
            }

            // check collision
            if (newStartTime < prevEndTime) {
                //! NOTE If we have reached the limit, we do nothing.
                //! TODO maybe there is a problem here, the start time of the clip may coincide with the end time of the prev clip
                if (muse::is_equal(clip->GetPlayStartTime(), prevEndTime)) {
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

muse::async::Channel<au::trackedit::ClipKey, double /*newStartTime*/, bool /*completed*/>
Au3Interaction::clipStartTimeChanged() const
{
    return m_clipStartTimeChanged;
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

bool Au3Interaction::pasteIntoClipboard(double begin, TrackId trackId)
{
    WaveTrack* dstWaveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(trackId));
    IF_ASSERT_FAILED(dstWaveTrack) {
        return false;
    }

    if (s_clipboard.data.empty()) {
        return false;
    }

    // check if every clip fits into track first
    for (const auto& data : s_clipboard.data) {
        WaveTrack* origWaveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(data.track->GetId()));
        IF_ASSERT_FAILED(dstWaveTrack) {
            return false;
        }

        double insertDuration = 0;

        // intentional comparison, see clipId default value
        if (data.clipKey.clipId == -1) {
            // handle multiple clips
            std::shared_ptr<WaveClip> leftClip = origWaveTrack->GetLeftmostClip();
            std::shared_ptr<WaveClip> rightClip = origWaveTrack->GetRightmostClip();
            insertDuration = rightClip->End() - leftClip->Start();
        } else {
            //handle single clip
            std::shared_ptr<WaveClip> clip = DomAccessor::findWaveClip(origWaveTrack, data.clipKey.clipId);
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

    // paste and update model
    for (const auto& data : s_clipboard.data) {
        auto prj = globalContext()->currentTrackeditProject();

        // use an unordered_set to store the IDs of the clips before the paste
        std::unordered_set<int> clipIdsBefore;
        for (const auto& clip : prj->clipList(trackId)) {
            clipIdsBefore.insert(clip.key.clipId);
        }

        dstWaveTrack->Paste(begin, *data.track);

        // Check which clips were added and trigger the onClipAdded event
        for (const auto& clip : prj->clipList(trackId)) {
            if (clipIdsBefore.find(clip.key.clipId) == clipIdsBefore.end()) {
                prj->onClipAdded(clip);
            }
        }
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

bool Au3Interaction::copyClipDataIntoClipboard(const ClipKey& clipKey, double begin, double end)
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

bool Au3Interaction::copyTrackDataIntoClipboard(const TrackId trackId, double begin, double end)
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

bool Au3Interaction::removeClipData(const trackedit::ClipKey& clipKey, double begin, double end)
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

void Au3Interaction::newMonoTrack()
{
    auto &project = projectRef();
    auto &tracks = ::TrackList::Get(project);
    auto &trackFactory = ::WaveTrackFactory::Get(project);

    sampleFormat defaultFormat = QualitySettings::SampleFormatChoice();
    auto rate = ::ProjectRate::Get(project).GetRate();

    auto track = trackFactory.Create(defaultFormat, rate);
    track->SetName(tracks.MakeUniqueTrackName(WaveTrack::GetDefaultAudioTrackNamePreference()));
    tracks.Add(track);

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->onTrackAdded(DomConverter::track(track.get()));

    //! TODO AU4: set selected?
    //! TODO AU4: add action to the history
    // ProjectHistory::Get( project )
    //     .PushState(XO("Created new audio track"), XO("New Track"));
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

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->onTrackAdded(DomConverter::track(*tracks.rbegin()));

    //! TODO AU4: set selected?
    //! TODO AU4: add action to the history
    // ProjectHistory::Get( project )
        // .PushState(XO("Created new stereo audio track"), XO("New Track"));
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
