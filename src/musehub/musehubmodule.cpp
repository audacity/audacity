/*
* Audacity: A Digital Audio Editor
*/
#include "musehubmodule.h"

#include "framework/interactive/iinteractiveuriregister.h"
#include "framework/rcommand/icommandsregister.h"
#include "framework/rcommand/icommandsstate.h"

#include "internal/musehubcommandscontroller.h"
#include "internal/musehubcommandsregister.h"
#include "internal/musehubcommandsstate.h"

using namespace au::musehub;
using namespace muse::modularity;

static const std::string mname("musehub");

std::string MuseHubModule::moduleName() const
{
    return mname;
}

void MuseHubModule::resolveImports()
{
    auto ir = globalIoc()->resolve<muse::interactive::IInteractiveUriRegister>(mname);
    if (ir) {
        ir->registerQmlUri(muse::Uri("audacity://projectscene/geteffects"), "Audacity.MuseHub", "GetEffectsDialog");
    }

    auto cr = globalIoc()->resolve<muse::rcommand::ICommandsRegister>(mname);
    if (cr) {
        m_commandsRegister = std::make_shared<MuseHubCommandsRegister>();
        cr->reg(m_commandsRegister);
    }
}

void MuseHubModule::onDeinit()
{
    auto ir = globalIoc()->resolve<muse::interactive::IInteractiveUriRegister>(mname);
    if (ir) {
        ir->unregisterUri(muse::Uri("audacity://projectscene/geteffects"));
    }

    if (m_commandsRegister) {
        globalIoc()->resolve<muse::rcommand::ICommandsRegister>(mname)->unreg(m_commandsRegister);
        m_commandsRegister.reset();
    }
}

IContextSetup* MuseHubModule::newContext(const ContextPtr& ctx) const
{
    return new MuseHubContext(ctx);
}

void MuseHubContext::resolveImports()
{
    auto cs = ioc()->resolve<muse::rcommand::ICommandsState>(mname);
    if (cs) {
        m_commandsState = std::make_shared<MuseHubCommandsState>();
        cs->reg(m_commandsState);
    }
}

void MuseHubContext::onInit(const muse::IApplication::RunMode& mode)
{
    if (mode != muse::IApplication::RunMode::GuiApp) {
        return;
    }

    m_commandsController = std::make_shared<MuseHubCommandsController>(iocContext());
    m_commandsController->init();
}

void MuseHubContext::onDeinit()
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
