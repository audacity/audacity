/*
* Audacity: A Digital Audio Editor
*/

#include "processinginteraction.h"

#include "libraries/lib-project/Project.h"
#include "libraries/lib-track/Track.h"
#include "libraries/lib-wave-track/WaveClip.h"

#include "domaccessor.h"
#include "domconverter.h"
#include "wxtypes_convert.h"

#include "log.h"

using namespace au::au3;

au::au3::Clipboard ProcessingInteraction::s_clipboard;

AudacityProject& ProcessingInteraction::projectRef() const
{
    AudacityProject* project = reinterpret_cast<AudacityProject*>(globalContext()->currentProject()->au3ProjectPtr());
    return *project;
}

au::audio::secs_t ProcessingInteraction::clipStartTime(const processing::ClipKey& clipKey) const
{
    WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return -1.0;
    }

    std::shared_ptr<WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.index);
    IF_ASSERT_FAILED(clip) {
        return -1.0;
    }

    return clip->Start();
}

bool ProcessingInteraction::changeClipStartTime(const processing::ClipKey& clipKey, double sec)
{
    WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    std::shared_ptr<WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.index);
    IF_ASSERT_FAILED(clip) {
        return false;
    }

    //! TODO Not sure what this method needs to be called to change the position, will need to clarify
    clip->SetPlayStartTime(sec);
    LOGD() << "changed PlayStartTime of clip: " << clipKey.index << ", track: " << clipKey.trackId;

    processing::ProcessingProjectPtr prj = globalContext()->currentProcessingProject();
    prj->onClipChanged(DomConverter::clip(waveTrack, clip.get(), clipKey.index));

    return true;
}

bool ProcessingInteraction::changeClipTitle(const processing::ClipKey& clipKey, const muse::String& newTitle)
{
    WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    std::shared_ptr<WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.index);
    IF_ASSERT_FAILED(clip) {
        return false;
    }

    clip->SetName(wxFromString(newTitle));
    LOGD() << "changed name of clip: " << clipKey.index << ", track: " << clipKey.trackId;

    processing::ProcessingProjectPtr prj = globalContext()->currentProcessingProject();
    prj->onClipChanged(DomConverter::clip(waveTrack, clip.get(), clipKey.index));

    return true;
}

void ProcessingInteraction::clearClipboard()
{
    s_clipboard.data.clear();
}

bool ProcessingInteraction::paste(double begin, processing::TrackId trackId)
{
    WaveTrack* dstWaveTrack = DomAccessor::findWaveTrack(projectRef(), TrackId(trackId));
    IF_ASSERT_FAILED(dstWaveTrack) {
        return false;
    }

    if (s_clipboard.data.empty()) {
        return false;
    }

    // check if every clip fits into track first
    for (const auto& data : s_clipboard.data) {
        WaveTrack* origWaveTrack = DomAccessor::findWaveTrack(projectRef(), TrackId(data.track->GetId()));
        IF_ASSERT_FAILED(dstWaveTrack) {
            return false;
        }

        double insertDuration = 0;
        if (data.clipKey.index == muse::nidx) {
            // handle multiple clips
            std::shared_ptr<WaveClip> leftClip = origWaveTrack->GetLeftmostClip();
            std::shared_ptr<WaveClip> rightClip = origWaveTrack->GetRightmostClip();
            insertDuration = rightClip->End() - leftClip->Start();
        } else {
            //handle single clip
            std::shared_ptr<WaveClip> clip = DomAccessor::findWaveClip(origWaveTrack, data.clipKey.index);
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

    // paste
    for (const auto& data : s_clipboard.data) {
        dstWaveTrack->Paste(begin, *data.track);
        int numClips = dstWaveTrack->GetNumClips();

        processing::ProcessingProjectPtr prj = globalContext()->currentProcessingProject();
        auto waveTrack = dynamic_cast<const WaveTrack*>(data.track.get());

        for (int i = 0; i < numClips; ++i) {
            std::shared_ptr<WaveClip> clip = DomAccessor::findWaveClip(dstWaveTrack, i);
            if (static_cast<processing::secs_t>(clip->Start()) >= static_cast<processing::secs_t>(begin)
                && static_cast<processing::secs_t>(clip->End())
                <= static_cast<processing::secs_t>(begin) + static_cast<processing::secs_t>(waveTrack->GetEndTime())) {
                prj->onClipAdded(DomConverter::clip(dstWaveTrack, clip.get(), i));
            }
        }
    }

    return true;
}

bool ProcessingInteraction::copyClip(const processing::ClipKey& clipKey)
{
    WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    std::shared_ptr<WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.index);
    IF_ASSERT_FAILED(clip) {
        return false;
    }

    auto track = waveTrack->Copy(clip->Start(), clip->End());
    s_clipboard.data.push_back(TrackData { track, clipKey });

    return true;
}

bool ProcessingInteraction::copyClipData(const processing::ClipKey& clipKey, double begin, double end)
{
    WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    std::shared_ptr<WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.index);
    IF_ASSERT_FAILED(clip) {
        return false;
    }

    auto track = waveTrack->Copy(begin, end);
    s_clipboard.data.push_back(TrackData { track, clipKey });

    return true;
}

bool ProcessingInteraction::copyTrackData(const processing::TrackId trackId, double begin, double end)
{
    WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), TrackId(trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    auto track = waveTrack->Copy(begin, end);
    processing::ClipKey dummyClipKey = processing::ClipKey();
    s_clipboard.data.push_back(TrackData { track, dummyClipKey });
}

bool ProcessingInteraction::removeClip(const processing::ClipKey& clipKey)
{
    WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    std::shared_ptr<WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.index);
    IF_ASSERT_FAILED(clip) {
        return false;
    }

    clip->Clear(clip->Start(), clip->End());

    processing::ProcessingProjectPtr prj = globalContext()->currentProcessingProject();
    prj->onClipRemoved(DomConverter::clip(waveTrack, clip.get(), clipKey.index));

    return true;
}

bool ProcessingInteraction::removeClipData(const processing::ClipKey& clipKey, double begin, double end)
{
    WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    std::shared_ptr<WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.index);
    IF_ASSERT_FAILED(clip) {
        return false;
    }

    processing::secs_t initialClipStart = clip->Start();
    processing::secs_t initialClipEnd = clip->End();

    clip->Clear(begin, end);

    processing::ProcessingProjectPtr prj = globalContext()->currentProcessingProject();
    if (begin <= initialClipStart && end >= initialClipEnd) {
        prj->onClipRemoved(DomConverter::clip(waveTrack, clip.get(), clipKey.index));
    } else {
        prj->onClipChanged(DomConverter::clip(waveTrack, clip.get(), clipKey.index));
    }

    return true;
}

au::audio::secs_t ProcessingInteraction::clipDuration(const processing::ClipKey& clipKey) const
{
    WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return -1.0;
    }

    std::shared_ptr<WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.index);
    IF_ASSERT_FAILED(clip) {
        return -1.0;
    }

    return clip->End() - clip->Start();
}
