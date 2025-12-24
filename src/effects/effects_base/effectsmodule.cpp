/*
* Audacity: A Digital Audio Editor
*/
#include "effectsmodule.h"

#include "au3-module-manager/PluginManager.h"
#include "au3-files/FileNames.h"

#include "ui/iuiactionsregister.h"
#include "ui/iinteractiveuriregister.h"
#include "diagnostics/idiagnosticspathsregister.h"

#include "internal/effectconfigsettings.h"
#include "internal/effectsprovider.h"
#include "internal/effectsmenuprovider.h"
#include "internal/effectsconfiguration.h"
#include "internal/effectsactionscontroller.h"
#include "internal/effectsuiactions.h"
#include "internal/effectinstancesregister.h"
#include "internal/effectexecutionscenario.h"
#include "internal/realtimeeffectservice.h"
#include "internal/effectpresetsprovider.h"
#include "internal/effectpresetsscenario.h"
#include "internal/effectviewlaunchregister.h"
#include "internal/effectparametersprovider.h"

#include "view/effectmanagemenu.h"
#include "view/effectsuiengine.h"
#include "view/destructiveeffectviewerdialogmodel.h"
#include "view/realtimeeffectviewerdialogmodel.h"
#include "view/generatedeffectviewermodel.h"

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
    m_effectsProvider = std::make_shared<EffectsProvider>(iocContext());
    m_effectsMenuProvider = std::make_shared<EffectsMenuProvider>(iocContext());
    m_configuration = std::make_shared<EffectsConfiguration>();
    m_actionsController = std::make_shared<EffectsActionsController>(iocContext());
    m_realtimeEffectService = std::make_shared<RealtimeEffectService>(iocContext());

    ioc()->registerExport<IEffectsProvider>(moduleName(), m_effectsProvider);
    ioc()->registerExport<IEffectsMenuProvider>(moduleName(), m_effectsMenuProvider);
    ioc()->registerExport<IEffectsConfiguration>(moduleName(), m_configuration);
    ioc()->registerExport<IEffectsUiEngine>(moduleName(), std::make_shared<EffectsUiEngine>(iocContext()));
    ioc()->registerExport<IEffectInstancesRegister>(moduleName(), new EffectInstancesRegister());
    ioc()->registerExport<IEffectExecutionScenario>(moduleName(), std::make_shared<EffectExecutionScenario>(iocContext()));
    ioc()->registerExport<IRealtimeEffectService>(moduleName(), m_realtimeEffectService);
    ioc()->registerExport<IEffectPresetsProvider>(moduleName(), std::make_shared<EffectPresetsProvider>(iocContext()));
    ioc()->registerExport<IEffectPresetsScenario>(moduleName(), std::make_shared<EffectPresetsScenario>(iocContext()));
    ioc()->registerExport<IEffectViewLaunchRegister>(moduleName(), new EffectViewLaunchRegister());
    ioc()->registerExport<IEffectParametersProvider>(moduleName(), new EffectParametersProvider());
}

void EffectsModule::resolveImports()
{
    auto ir = ioc()->resolve<muse::ui::IInteractiveUriRegister>(moduleName());
    if (ir) {
        ir->registerQmlUri(muse::Uri("audacity://effects/destructive_viewer"), "Audacity/Effects/DestructiveEffectsViewerDialog.qml");
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
    qmlRegisterType<EffectManageMenu>("Audacity.Effects", 1, 0, "EffectManageMenu");
    qmlRegisterType<DestructiveEffectViewerDialogModel>("Audacity.Effects", 1, 0, "DestructiveEffectViewerDialogModel");
    qmlRegisterType<RealtimeEffectViewerDialogModel>("Audacity.Effects", 1, 0, "RealtimeEffectViewerDialogModel");
    qmlRegisterType<GeneratedEffectViewerModel>("Audacity.Effects", 1, 0, "GeneratedEffectViewerModel");
    qmlRegisterUncreatableType<EffectFamilies>("Audacity.Effects", 1, 0, "EffectFamily", "Not creatable from QML");
    qmlRegisterUncreatableType<ViewerComponentTypes>("Audacity.Effects", 1, 0, "ViewerComponentType", "Not creatable from QML");
}

void EffectsModule::onInit(const muse::IApplication::RunMode&)
{
    PluginManager::Get().Initialize([](const FilePath& localFileName) {
        return std::make_unique<au3::EffectConfigSettings>(localFileName.ToStdString());
    });

    m_effectsMenuProvider->init();
    m_configuration->init();
    m_actionsController->init();
    m_realtimeEffectService->init();
    m_effectsProvider->init();

    //! --- Diagnostics ---
    auto pr = ioc()->resolve<muse::diagnostics::IDiagnosticsPathsRegister>(moduleName());
    if (pr) {
        pr->reg("pluginsettings", FileNames::PluginSettings().ToStdString());
    }
}

void EffectsModule::onDelayedInit()
{
    //! NOTE On init, built-in, vst and other plugins are initialized.
    //! After all, the provider can load effects of different types.
    m_effectsProvider->reloadEffects();
}
