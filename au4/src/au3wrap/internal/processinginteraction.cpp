#include "processinginteraction.h"

#include "libraries/lib-project/Project.h"
#include "libraries/lib-track/Track.h"
#include "libraries/lib-wave-track/WaveClip.h"

#include "domaccessor.h"
#include "domconverter.h"
#include "../wxtypes_convert.h"

#include "log.h"

using namespace au::au3;

AudacityProject& ProcessingInteraction::projectRef() const
{
    AudacityProject* project = reinterpret_cast<AudacityProject*>(globalContext()->currentProject()->au3ProjectPtr());
    return *project;
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
