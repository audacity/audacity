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

    bool changeClipStartTime(const processing::ClipKey& clipKey, double sec) override;
    bool changeClipTitle(const processing::ClipKey& clipKey, const muse::String& newTitle) override;
    bool removeClip(const processing::ClipKey& clipKey) override;
    bool removeClipData(const processing::ClipKey& clipKey, double begin, double end) override;

private:
    AudacityProject& projectRef() const;
};
}
