/*
* Audacity: A Digital Audio Editor
*/
#include "effectsmodule.h"

#include "au3-module-manager/PluginManager.h"
#include "au3-files/FileNames.h"

#include "framework/interactive/iinteractiveuriregister.h"
#include "framework/diagnostics/idiagnosticspathsregister.h"

#include "internal/effectconfigsettings.h"
#include "internal/effectsprovider.h"
#include "internal/effectsmenuprovider.h"
#include "internal/effectsconfiguration.h"
#include "internal/effectsactionscontroller.h"
#include "internal/effectinstancesregister.h"
#include "internal/effectexecutionscenario.h"
#include "internal/realtimeeffectservice.h"
#include "internal/effectpresetsprovider.h"
#include "internal/effectpresetsscenario.h"
#include "internal/effectviewlaunchregister.h"
#include "internal/effectparametersprovider.h"
#include "internal/parameterextractorregistry.h"

#include "view/effectmanagemenu.h"
#include "view/effectsuiengine.h"
#include "view/effectsviewutils.h"
#include "view/destructiveeffectviewerdialogmodel.h"
#include "view/realtimeeffectviewerdialogmodel.h"
#include "view/generatedeffectviewermodel.h"

using namespace au::effects;

static const std::string mname("effects_base");

static void effects_base_init_qrc()
{
    Q_INIT_RESOURCE(effects_base);
}

std::string EffectsModule::moduleName() const
{
    return mname;
}

void EffectsModule::registerExports()
{
    m_configuration = std::make_shared<EffectsConfiguration>();

    globalIoc()->registerExport<IEffectsConfiguration>(mname, m_configuration);
    globalIoc()->registerExport<IParameterExtractorRegistry>(mname, new ParameterExtractorRegistry());
}

void EffectsModule::resolveImports()
{
    auto ir = globalIoc()->resolve<muse::interactive::IInteractiveUriRegister>(mname);
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
    REGISTER_AUDACITY_EFFECTS_SINGLETON_TYPE(GeneratedEffectViewerModelFactory);
    qmlRegisterUncreatableType<EffectFamilies>("Audacity.Effects", 1, 0, "EffectFamily", "Not creatable from QML");
    qmlRegisterUncreatableType<ViewerComponentTypes>("Audacity.Effects", 1, 0, "ViewerComponentType", "Not creatable from QML");
}

void EffectsModule::onInit(const muse::IApplication::RunMode&)
{
    PluginManager::Get().Initialize([](const FilePath& localFileName) {
        return std::make_unique<au3::EffectConfigSettings>(localFileName.ToStdString());
    });

    m_configuration->init();

    //! --- Diagnostics ---
    auto pr = globalIoc()->resolve<muse::diagnostics::IDiagnosticsPathsRegister>(mname);
    if (pr) {
        pr->reg("pluginsettings", FileNames::PluginSettings().ToStdString());
    }
}

void EffectsModule::onDelayedInit()
{
}

muse::modularity::IContextSetup* EffectsModule::newContext(const muse::modularity::ContextPtr& ctx) const
{
    return new EffectsContext(ctx);
}

// =====================================================
// EffectsContext
// =====================================================

void EffectsContext::registerExports()
{
    m_effectsProvider = std::make_shared<EffectsProvider>(iocContext());
    m_effectsMenuProvider = std::make_shared<EffectsMenuProvider>(iocContext());
    m_actionsController = std::make_shared<EffectsActionsController>(iocContext());
    m_realtimeEffectService = std::make_shared<RealtimeEffectService>(iocContext());

    ioc()->registerExport<IEffectsProvider>(mname, m_effectsProvider);
    ioc()->registerExport<IEffectsMenuProvider>(mname, m_effectsMenuProvider);
    ioc()->registerExport<IEffectsUiEngine>(mname, std::make_shared<EffectsUiEngine>(iocContext()));
    ioc()->registerExport<IEffectPresetsProvider>(mname, std::make_shared<EffectPresetsProvider>(iocContext()));
    ioc()->registerExport<IEffectPresetsScenario>(mname, std::make_shared<EffectPresetsScenario>(iocContext()));
    ioc()->registerExport<IEffectViewLaunchRegister>(mname, new EffectViewLaunchRegister());
    ioc()->registerExport<IEffectParametersProvider>(mname, new EffectParametersProvider(iocContext()));
    ioc()->registerExport<IEffectInstancesRegister>(mname, new EffectInstancesRegister(iocContext()));
    ioc()->registerExport<IEffectExecutionScenario>(mname, std::make_shared<EffectExecutionScenario>(iocContext()));
    ioc()->registerExport<IRealtimeEffectService>(mname, m_realtimeEffectService);
}

void EffectsContext::onInit(const muse::IApplication::RunMode&)
{
    m_effectsMenuProvider->init();
    m_effectsProvider->init();
    m_actionsController->init();
    m_realtimeEffectService->init();
}

void EffectsContext::onAllInited(const muse::IApplication::RunMode&)
{
    //! NOTE On init, built-in, vst and other plugins are initialized.
    //! After all, the provider can load effects of different types.
    m_effectsProvider->reloadEffects();
}

void EffectsContext::onDeinit()
{
}
