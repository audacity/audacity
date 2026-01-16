/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "modularity/ioc.h"
#include "global/iinteractive.h"
#include "actions/iactionsdispatcher.h"
#include "trackedit/itrackeditconfiguration.h"

#include "trackedit/trackedittypes.h"

namespace au::trackedit {
class DeleteBehaviorOnboardingScenario : public muse::Injectable
{
    muse::GlobalInject<ITrackeditConfiguration> configuration;

    muse::Inject<muse::IInteractive> interactive { this };
    muse::Inject<muse::actions::IActionsDispatcher> dispatcher { this };

public:
    DeleteBehaviorOnboardingScenario(const muse::modularity::ContextPtr& ctx)
        : muse::Injectable(ctx) {}

    bool showOnboardingDialog() const;
    void showFollowupDialog() const;

private:
    bool actionExists(const std::string& action) const;
};
}
