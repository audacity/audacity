/*
 * Audacity: A Digital Audio Editor
 */
#include "testflowstartupscenario.h"

#include "appshell/testflowstartup.h"

using namespace au::appshell;

void au::appshell::installTestflowStartupScenario(const muse::modularity::ContextPtr& ctx, bool allowRecovery)
{
    muse::modularity::ModulesContextIoC* ioc = muse::modularity::ioc(ctx);
    ioc->unregister<IStartupScenario>("appshell");
    ioc->registerExport<IStartupScenario>("appshell", new TestflowStartupScenario(ctx, allowRecovery));
}

StartupModeType TestflowStartupScenario::resolveStartupModeType() const
{
    return StartupModeType::StartEmpty;
}

bool TestflowStartupScenario::allowsFirstLaunchOverride() const
{
    return false;
}

bool TestflowStartupScenario::allowsRecoveryOverride() const
{
    return m_allowRecovery && multiwindowsProvider()->isFirstWindow();
}

void TestflowStartupScenario::showStartupDialogsIfNeed(StartupModeType)
{
}
