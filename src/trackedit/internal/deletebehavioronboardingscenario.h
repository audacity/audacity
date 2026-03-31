/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "framework/global/modularity/ioc.h"

#include "framework/interactive/iinteractive.h"
#include "framework/actions/iactionsdispatcher.h"

#include "trackedit/itrackeditconfiguration.h"

#include "trackedit/trackedittypes.h"

namespace au::trackedit {
class DeleteBehaviorOnboardingScenario : public muse::Contextable
{
    muse::GlobalInject<ITrackeditConfiguration> configuration;

    muse::ContextInject<muse::IInteractive> interactive { this };
    muse::ContextInject<muse::actions::IActionsDispatcher> dispatcher { this };

public:
    DeleteBehaviorOnboardingScenario(const muse::modularity::ContextPtr& ctx)
        : muse::Contextable(ctx) {}

    bool showOnboardingDialog() const;
    void showFollowupDialog() const;

private:
    bool actionExists(const std::string& action) const;
};
}
