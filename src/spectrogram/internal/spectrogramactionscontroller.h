/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "framework/interactive/iinteractive.h"

#include "framework/global/modularity/ioc.h"
#include "framework/rcommand/commandable.h"
#include "framework/rcommand/icommanddispatcher.h"
#include "context/iglobalcontext.h"

namespace au::spectrogram {
class SpectrogramActionsController : public muse::rcommand::Commandable, public muse::Contextable
{
    muse::ContextInject<muse::rcommand::ICommandDispatcher> commandDispatcher { this };
    muse::ContextInject<muse::IInteractive> interactive { this };
    muse::ContextInject<au::context::IGlobalContext> globalContext { this };

public:
    SpectrogramActionsController(const muse::modularity::ContextPtr& ctx)
        : muse::Contextable(ctx) {}

    void init();

private:
    muse::Ret openTrackSpectrogramSettings(const muse::rcommand::Params& params);
};
}
