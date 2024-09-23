/*
* Audacity: A Digital Audio Editor
*/
#include "effectsmodule.h"

#include "libraries/lib-module-manager/PluginManager.h"

#include "ui/iuiactionsregister.h"
#include "ui/iinteractiveuriregister.h"

#include "internal/effectsprovider.h"
#include "internal/effectsconfiguration.h"
#include "internal/effectsactionscontroller.h"
#include "internal/effectsuiactions.h"
#include "internal/effectinstancesregister.h"
#include "internal/effectexecutionscenario.h"

#include "effectsettings.h"

#include "view/effectsviewregister.h"
#include "view/effectbuilder.h"
#include "view/effectsuiengine.h"

using namespace au::effects;

static void effects_base_init_qrc()
{
    Q_INIT_RESOURCE(effects_base);
}

std::string EffectsModule::moduleName() const
{
    return "effects_base";
}

void EffectsModule::registerExports()
{
    m_provider = std::make_shared<EffectsProvider>();
    m_configuration = std::make_shared<EffectsConfiguration>();
    m_actionsController = std::make_shared<EffectsActionsController>();
    m_effectsUiActions = std::make_shared<EffectsUiActions>(m_actionsController);

    ioc()->registerExport<IEffectsProvider>(moduleName(), m_provider);
    ioc()->registerExport<IEffectsConfiguration>(moduleName(), m_configuration);
    ioc()->registerExport<IEffectsViewRegister>(moduleName(), new EffectsViewRegister());
    ioc()->registerExport<IEffectsUiEngine>(moduleName(), new EffectsUiEngine());
    ioc()->registerExport<IEffectInstancesRegister>(moduleName(), new EffectInstancesRegister());
    ioc()->registerExport<IEffectExecutionScenario>(moduleName(), new EffectExecutionScenario());
}

void EffectsModule::resolveImports()
{
    auto ar = ioc()->resolve<muse::ui::IUiActionsRegister>(moduleName());
    if (ar) {
        ar->reg(m_effectsUiActions);
    }
    auto ir = ioc()->resolve<muse::ui::IInteractiveUriRegister>(moduleName());
    if (ir) {
        ir->registerQmlUri(muse::Uri("audacity://effects/viewer"), "Audacity/Effects/EffectsViewerDialog.qml");
    }
}

void EffectsModule::registerResources()
{
    effects_base_init_qrc();
}

void EffectsModule::registerUiTypes()
{
    qmlRegisterType<EffectBuilder>("Audacity.Effects", 1, 0, "EffectBuilder");
}

void EffectsModule::onInit(const muse::IApplication::RunMode&)
{
    PluginManager::Get().Initialize([](const FilePath& localFileName) {
        return std::make_unique<au3::EffectSettings>(localFileName.ToStdString());
    });

    m_actionsController->init();
    m_effectsUiActions->init();

    m_provider->reloadEffects();
}

void EffectsModule::onDelayedInit()
{
    // m_provider->reloadEffects();
}
