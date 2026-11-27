/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "framework/global/modularity/ioc.h"
#include "framework/interactive/iinteractive.h"
#include "framework/rcommand/commandable.h"

namespace au::projectrecovery {
class ProjectRecoveryCommandsController : public muse::rcommand::Commandable, public muse::Contextable
{
    muse::ContextInject<muse::rcommand::ICommandDispatcher> commandDispatcher { this };
    muse::ContextInject<muse::IInteractive> interactive { this };

public:
    explicit ProjectRecoveryCommandsController(const muse::modularity::ContextPtr& ctx)
        : muse::Contextable(ctx) {}

    void init();
    void deinit();

private:
    muse::Ret recoverProject();
};
}
