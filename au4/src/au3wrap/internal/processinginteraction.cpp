#include "processinginteraction.h"

#include "libraries/lib-project/Project.h"
#include "libraries/lib-track/Track.h"
#include "libraries/lib-wave-track/WaveClip.h"

#include "domaccessor.h"
#include "domconverter.h"
#include "wxtypes_convert.h"

#include "log.h"

using namespace au::au3;

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

    std::shared_ptr<WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.id);
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

    std::shared_ptr<WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.id);
    IF_ASSERT_FAILED(clip) {
        return false;
    }

    //! TODO Not sure what this method needs to be called to change the position, will need to clarify
    clip->SetPlayStartTime(sec);
    LOGD() << "changed PlayStartTime of clip: " << clipKey.id << ", track: " << clipKey.trackId;

    processing::ProcessingProjectPtr prj = globalContext()->currentProcessingProject();
    prj->onClipChanged(DomConverter::clip(waveTrack, clip.get()));

    return true;
}

bool ProcessingInteraction::changeClipTitle(const processing::ClipKey& clipKey, const muse::String& newTitle)
{
    WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    std::shared_ptr<WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.id);
    IF_ASSERT_FAILED(clip) {
        return false;
    }

    clip->SetName(wxFromString(newTitle));
    LOGD() << "changed name of clip: " << clipKey.id << ", track: " << clipKey.trackId;

    processing::ProcessingProjectPtr prj = globalContext()->currentProcessingProject();
    prj->onClipChanged(DomConverter::clip(waveTrack, clip.get()));

    return true;
}

bool ProcessingInteraction::removeClip(const processing::ClipKey& clipKey)
{
    WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    std::shared_ptr<WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.id);
    IF_ASSERT_FAILED(clip) {
        return false;
    }

    clip->Clear(clip->Start(), clip->End());

    processing::ProcessingProjectPtr prj = globalContext()->currentProcessingProject();
    prj->onClipRemoved(DomConverter::clip(waveTrack, clip.get()));

    return true;
}

bool ProcessingInteraction::removeClipData(const processing::ClipKey& clipKey, double begin, double end)
{
    WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    std::shared_ptr<WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.id);
    IF_ASSERT_FAILED(clip) {
        return false;
    }

    processing::secs_t initialClipStart = clip->Start();
    processing::secs_t initialClipEnd = clip->End();

    clip->Clear(begin, end);

    processing::ProcessingProjectPtr prj = globalContext()->currentProcessingProject();
    if (begin <= initialClipStart && end >= initialClipEnd) {
        prj->onClipRemoved(DomConverter::clip(waveTrack, clip.get()));
    } else {
        prj->onClipChanged(DomConverter::clip(waveTrack, clip.get()));
    }

    return true;
}

au::audio::secs_t ProcessingInteraction::clipDuration(const processing::ClipKey& clipKey) const
{
    WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return -1.0;
    }

    std::shared_ptr<WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.id);
    IF_ASSERT_FAILED(clip) {
        return -1.0;
    }

    return clip->End() - clip->Start();
}
