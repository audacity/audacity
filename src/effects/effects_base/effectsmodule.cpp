/*
* Audacity: A Digital Audio Editor
*/
#include "effectsmodule.h"

#include "libraries/lib-module-manager/PluginManager.h"

#include "ui/iuiactionsregister.h"
#include "ui/iinteractiveuriregister.h"

#include "internal/effectconfigsettings.h"
#include "internal/effectsprovider.h"
#include "internal/effectsconfiguration.h"
#include "internal/effectsactionscontroller.h"
#include "internal/effectsuiactions.h"
#include "internal/effectinstancesregister.h"
#include "internal/effectexecutionscenario.h"
#include "internal/realtimeeffectservice.h"
#include "internal/effectpresetsprovider.h"
#include "internal/effectpresetsscenario.h"
#include "internal/effectviewlaunchregister.h"

#include "view/effectsviewregister.h"
#include "view/effectviewloader.h"
#include "view/effectsuiengine.h"
#include "view/realtimeeffectviewerdialogmodel.h"

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
    m_realtimeEffectService = std::make_shared<RealtimeEffectService>();

    ioc()->registerExport<IEffectsProvider>(moduleName(), m_provider);
    ioc()->registerExport<IEffectsConfiguration>(moduleName(), m_configuration);
    ioc()->registerExport<IEffectsViewRegister>(moduleName(), new EffectsViewRegister());
    ioc()->registerExport<IEffectsUiEngine>(moduleName(), new EffectsUiEngine());
    ioc()->registerExport<IEffectInstancesRegister>(moduleName(), new EffectInstancesRegister());
    ioc()->registerExport<IEffectExecutionScenario>(moduleName(), new EffectExecutionScenario());
    ioc()->registerExport<IRealtimeEffectService>(moduleName(), m_realtimeEffectService);
    ioc()->registerExport<IEffectPresetsProvider>(moduleName(), new EffectPresetsProvider());
    ioc()->registerExport<IEffectPresetsScenario>(moduleName(), new EffectPresetsScenario());
    ioc()->registerExport<IEffectViewLaunchRegister>(moduleName(), new EffectViewLaunchRegister());
}

void EffectsModule::resolveImports()
{
    auto ir = ioc()->resolve<muse::ui::IInteractiveUriRegister>(moduleName());
    if (ir) {
        ir->registerQmlUri(muse::Uri("audacity://effects/builtin_viewer"), "Audacity/Effects/EffectsViewerDialog.qml");
        ir->registerQmlUri(muse::Uri("audacity://effects/realtime_viewer"), "Audacity/Effects/RealtimeEffectViewerDialog.qml");
        ir->registerQmlUri(muse::Uri("audacity://effects/presets/input_name"), "Audacity/Effects/PresetNameDialog.qml");
    }
}

void EffectsModule::registerResources()
{
    effects_base_init_qrc();
}

void EffectsModule::registerUiTypes()
{
    qmlRegisterType<EffectViewLoader>("Audacity.Effects", 1, 0, "EffectViewLoader");
    qmlRegisterType<RealtimeEffectViewerDialogModel>("Audacity.Effects", 1, 0, "RealtimeEffectViewerDialogModel");
}

void EffectsModule::onInit(const muse::IApplication::RunMode&)
{
    PluginManager::Get().Initialize([](const FilePath& localFileName) {
        return std::make_unique<au3::EffectConfigSettings>(localFileName.ToStdString());
    });

    m_configuration->init();
    m_actionsController->init();
    m_realtimeEffectService->init();
}

void EffectsModule::onDelayedInit()
{
    //! NOTE On init, built-in, vst and other plugins are initialized.
    //! After all, the provider can load effects of different types.
    m_provider->reloadEffects();
}
