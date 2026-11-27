/*
 * Audacity: A Digital Audio Editor
 */
#include "projectrecoverymodule.h"

#include "framework/interactive/iinteractiveuriregister.h"
#include "framework/rcommand/icommandsregister.h"
#include "framework/rcommand/icommandsstate.h"

#include "projectrecoverycommands.h"
#include "internal/projectrecoverycommandscontroller.h"
#include "internal/projectrecoverycommandsregister.h"
#include "internal/projectrecoverycommandsstate.h"
#include "internal/projectrecoveryservice.h"

using namespace au::projectrecovery;

static const std::string mname("projectrecovery");

std::string ProjectRecoveryModule::moduleName() const
{
    return mname;
}

void ProjectRecoveryModule::registerExports()
{
    m_service = std::make_shared<ProjectRecoveryService>();
    globalIoc()->registerExport<IProjectRecoveryService>(mname, m_service);
}

void ProjectRecoveryModule::resolveImports()
{
    auto ir = globalIoc()->resolve<muse::interactive::IInteractiveUriRegister>(mname);
    if (ir) {
        ir->registerQmlUri(PROJECT_RECOVERY_URI, "Audacity.ProjectRecovery", "ProjectRecoveryDialog");
    }

    auto cr = globalIoc()->resolve<muse::rcommand::ICommandsRegister>(mname);
    if (cr) {
        m_commandsRegister = std::make_shared<ProjectRecoveryCommandsRegister>();
        cr->reg(m_commandsRegister);
    }
}

void ProjectRecoveryModule::onDeinit()
{
    // Join running jobs before global services are released
    m_service->stop();

    auto ir = globalIoc()->resolve<muse::interactive::IInteractiveUriRegister>(mname);
    if (ir) {
        ir->unregisterUri(PROJECT_RECOVERY_URI);
    }

    if (m_commandsRegister) {
        globalIoc()->resolve<muse::rcommand::ICommandsRegister>(mname)->unreg(m_commandsRegister);
        m_commandsRegister.reset();
    }
}

muse::modularity::IContextSetup* ProjectRecoveryModule::newContext(const muse::modularity::ContextPtr& ctx) const
{
    return new ProjectRecoveryContext(ctx);
}

void ProjectRecoveryContext::resolveImports()
{
    auto cs = ioc()->resolve<muse::rcommand::ICommandsState>(mname);
    if (cs) {
        m_commandsState = std::make_shared<ProjectRecoveryCommandsState>();
        cs->reg(m_commandsState);
    }
}

void ProjectRecoveryContext::onInit(const muse::IApplication::RunMode& mode)
{
    if (mode != muse::IApplication::RunMode::GuiApp) {
        return;
    }

    m_commandsController = std::make_shared<ProjectRecoveryCommandsController>(iocContext());
    m_commandsController->init();
}

void ProjectRecoveryContext::onDeinit()
{
    if (m_commandsController) {
        m_commandsController->deinit();
        m_commandsController.reset();
    }

    if (m_commandsState) {
        ioc()->resolve<muse::rcommand::ICommandsState>(mname)->unreg(m_commandsState);
        m_commandsState.reset();
    }
}
