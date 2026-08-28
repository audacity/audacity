/*
 * Audacity: A Digital Audio Editor
 */
#include "testflowstartupscenario.h"

#include "appshell/testflowstartup.h"

using namespace au::appshell;

void au::appshell::installTestflowStartupScenario(const muse::modularity::ContextPtr& ctx)
{
    muse::modularity::ModulesContextIoC* ioc = muse::modularity::ioc(ctx);
    ioc->unregister<IStartupScenario>("appshell");
    ioc->registerExport<IStartupScenario>("appshell", new TestflowStartupScenario(ctx));
}

StartupModeType TestflowStartupScenario::resolveStartupModeType() const
{
    return StartupModeType::StartEmpty;
}

bool TestflowStartupScenario::allowsStartupModeOverride() const
{
    return false;
}

void TestflowStartupScenario::showStartupDialogsIfNeed(StartupModeType)
{
}
