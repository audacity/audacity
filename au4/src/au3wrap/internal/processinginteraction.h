/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "Track.h"
#include "processing/iprocessinginteraction.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

class AudacityProject;
namespace au::au3 {
struct TrackData
{
    std::shared_ptr<Track> track;
    processing::ClipKey clipKey;
};

struct Clipboard
{
    std::vector<TrackData> data;
};

class ProcessingInteraction : public processing::IProcessingInteraction
{
    muse::Inject<au::context::IGlobalContext> globalContext;

public:
    ProcessingInteraction() = default;

    audio::secs_t clipStartTime(const processing::ClipKey& clipKey) const override;
    bool changeClipStartTime(const processing::ClipKey& clipKey, double sec) override;
    bool changeClipTitle(const processing::ClipKey& clipKey, const muse::String& newTitle) override;
    void clearClipboard() override;
    bool paste(double begin, processing::TrackId trackId) override;
    bool copyClip(const processing::ClipKey& clipKey) override;
    bool copyClipData(const processing::ClipKey& clipKey, double begin, double end) override;
    bool copyTrackData(const processing::TrackId trackId, double begin, double end) override;
    bool removeClip(const processing::ClipKey& clipKey) override;
    bool removeClipData(const processing::ClipKey& clipKey, double begin, double end) override;
    audio::secs_t clipDuration(const processing::ClipKey& clipKey) const override;

private:
    AudacityProject& projectRef() const;

    static Clipboard s_clipboard;
};
}
