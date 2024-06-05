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

    muse::ValCh<processing::ClipKey> selectedClip() const override;
    void selectClip(const processing::ClipKey& clipKey) override;

private:
    AudacityProject& projectRef() const;

    muse::ValCh<processing::ClipKey> m_selectedClip;
};
}
