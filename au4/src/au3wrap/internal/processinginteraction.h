#pragma once

#include "processing/iprocessinginteraction.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

class AudacityProject;
namespace au::au3 {
class ProcessingInteraction : public processing::IProcessingInteraction
{
    muse::Inject<au::context::IGlobalContext> globalContext;

public:
    ProcessingInteraction() = default;

    audio::secs_t clipStartTime(const processing::ClipKey& clipKey) const override;
    bool changeClipStartTime(const processing::ClipKey& clipKey, double sec) override;
    bool changeClipTitle(const processing::ClipKey& clipKey, const muse::String& newTitle) override;
    bool removeClip(const processing::ClipKey& clipKey) override;
    bool removeClipData(const processing::ClipKey& clipKey, double begin, double end) override;
    audio::secs_t clipDuration(const processing::ClipKey& clipKey) const override;

private:
    AudacityProject& projectRef() const;
};
}
