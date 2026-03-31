/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "../ifrequencyselectionrestorer.h"
#include "ispectrogramservice.h"

#include "context/iglobalcontext.h"

#include "framework/global/modularity/ioc.h"

class AudacityProject;

namespace au::spectrogram {
class FrequencySelectionRestorer : public IFrequencySelectionRestorer, public muse::Contextable
{
    muse::ContextInject<au::context::IGlobalContext> globalContext { this };
    muse::ContextInject<ISpectrogramService> spectrogramService { this };

public:
    FrequencySelectionRestorer(const muse::modularity::ContextPtr& ctx)
        : muse::Contextable(ctx) {}
    ~FrequencySelectionRestorer() override = default;
    void storeFrequencySelectionState(const FrequencySelection& selection) const override;
    FrequencySelection loadFrequencySelectionState() const override;

private:
    AudacityProject* au3Project() const
    {
        const auto project = globalContext()->currentProject();
        if (!project) {
            return nullptr;
        }
        return reinterpret_cast<AudacityProject*>(project->au3ProjectPtr());
    }
};
}
