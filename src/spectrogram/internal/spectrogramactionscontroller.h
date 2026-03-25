/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "framework/actions/actionable.h"
#include "framework/actions/iactionsdispatcher.h"
#include "framework/interactive/iinteractive.h"
#include "framework/global/modularity/ioc.h"

namespace au::spectrogram {
class SpectrogramActionsController : public muse::actions::Actionable, public muse::Contextable
{
    muse::Inject<muse::actions::IActionsDispatcher> dispatcher { this };
    muse::Inject<muse::IInteractive> interactive { this };

public:
    SpectrogramActionsController(const muse::modularity::ContextPtr& ctx)
        : muse::Contextable(ctx) {}

    void init();

private:
    void openTrackSpectrogramSettings(const muse::actions::ActionData& args);
};
}
