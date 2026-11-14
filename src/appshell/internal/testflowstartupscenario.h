/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "startupscenario.h"

namespace au::appshell {
//! NOTE Starts empty, with no startup dialog and no session restore unless `allowRecovery`
class TestflowStartupScenario : public StartupScenario
{
public:
    TestflowStartupScenario(const muse::modularity::ContextPtr& ctx, bool allowRecovery)
        : StartupScenario(ctx), m_allowRecovery(allowRecovery) {}

protected:
    StartupModeType resolveStartupModeType() const override;
    bool allowsFirstLaunchOverride() const override;
    bool allowsRecoveryOverride() const override;
    void showStartupDialogsIfNeed(StartupModeType modeType) override;

private:
    const bool m_allowRecovery;
};
}
