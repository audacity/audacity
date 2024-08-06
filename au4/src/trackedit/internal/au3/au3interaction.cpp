#include "au3interaction.h"

#include <algorithm>

#include "global/types/number.h"

#include "libraries/lib-project/Project.h"
#include "libraries/lib-track/Track.h"
#include "libraries/lib-wave-track/WaveClip.h"

#include "au3wrap/internal/domaccessor.h"
#include "au3wrap/internal/domconverter.h"
#include "au3wrap/internal/wxtypes_convert.h"

#include "log.h"

using namespace au::trackedit;
using namespace au::au3;

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

    std::shared_ptr<WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.index);
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

    std::shared_ptr<WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.index);
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
    prj->onClipChanged(DomConverter::clip(waveTrack, clip.get(), clipKey.index));

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

    std::shared_ptr<WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.index);
    IF_ASSERT_FAILED(clip) {
        return false;
    }

    clip->SetName(wxFromString(newTitle));
    LOGD() << "changed name of clip: " << clipKey.index << ", track: " << clipKey.trackId;

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->onClipChanged(DomConverter::clip(waveTrack, clip.get(), clipKey.index));

    return true;
}

bool Au3Interaction::removeClip(const trackedit::ClipKey& clipKey)
{
    WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    std::shared_ptr<WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.index);
    IF_ASSERT_FAILED(clip) {
        return false;
    }

    clip->Clear(clip->Start(), clip->End());

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->onClipRemoved(DomConverter::clip(waveTrack, clip.get(), clipKey.index));

    return true;
}

bool Au3Interaction::removeClipData(const trackedit::ClipKey& clipKey, double begin, double end)
{
    WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    std::shared_ptr<WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.index);
    IF_ASSERT_FAILED(clip) {
        return false;
    }

    trackedit::secs_t initialClipStart = clip->Start();
    trackedit::secs_t initialClipEnd = clip->End();

    clip->Clear(begin, end);

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    if (begin <= initialClipStart && end >= initialClipEnd) {
        prj->onClipRemoved(DomConverter::clip(waveTrack, clip.get(), clipKey.index));
    } else {
        prj->onClipChanged(DomConverter::clip(waveTrack, clip.get(), clipKey.index));
    }

    return true;
}

au::audio::secs_t Au3Interaction::clipDuration(const trackedit::ClipKey& clipKey) const
{
    WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return -1.0;
    }

    std::shared_ptr<WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.index);
    IF_ASSERT_FAILED(clip) {
        return -1.0;
    }

    return clip->End() - clip->Start();
}
