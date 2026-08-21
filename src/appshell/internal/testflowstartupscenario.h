/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "startupscenario.h"

namespace au::appshell {
//! NOTE Starts empty, with no startup dialog and no session restore
class TestflowStartupScenario : public StartupScenario
{
public:
    TestflowStartupScenario(const muse::modularity::ContextPtr& ctx)
        : StartupScenario(ctx) {}

protected:
    StartupModeType resolveStartupModeType() const override;
    bool allowsStartupModeOverride() const override;
    void showStartupDialogsIfNeed(StartupModeType modeType) override;
};
}
