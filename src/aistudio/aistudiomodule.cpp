/*
 * Audacity: A Digital Audio Editor
 */
#include "aistudiomodule.h"

#include "modularity/ioc.h"
#include "ui/iuiactionsregister.h"

#include <QtQml>

#include "internal/aistudiocontroller.h"
#include "internal/aistudiouiactions.h"
#include "view/aistudiostatusmodel.h"

using namespace au::aistudio;
using namespace muse;

static const std::string mname("aistudio");

std::string AIStudioModule::moduleName() const
{
    return mname;
}

void AIStudioModule::registerUiTypes()
{
    qmlRegisterSingletonType<AIStudioStatusModel>("Audacity.AIStudio", 1, 0, "AIStudioStatus", [](QQmlEngine*, QJSEngine*) {
        return AIStudioStatusModel::instance();
    });
}

muse::modularity::IContextSetup* AIStudioModule::newContext(const muse::modularity::ContextPtr& ctx) const
{
    return new AIStudioContext(ctx);
}

void AIStudioContext::registerExports()
{
    m_controller = std::make_shared<AIStudioController>(iocContext());
    m_uiActions = std::make_shared<AIStudioUiActions>(iocContext(), m_controller);
}

void AIStudioContext::onInit(const IApplication::RunMode& mode)
{
    if (mode == IApplication::RunMode::AudioPluginRegistration) {
        return;
    }

    m_controller->init();
    if (auto actionsRegister = ioc()->resolve<ui::IUiActionsRegister>(mname)) {
        actionsRegister->reg(m_uiActions);
    }
}
