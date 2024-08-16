/*
* Audacity: A Digital Audio Editor
*/
#include "effectsmodule.h"

#include "libraries/lib-files/FileNames.h"
#include "libraries/lib-module-manager/PluginManager.h"

#include "ui/iuiactionsregister.h"
#include "ui/iinteractiveuriregister.h"

#include "audioplugins/iaudiopluginsscannerregister.h"
#include "audioplugins/iaudiopluginmetareaderregister.h"

#include "internal/effectsprovider.h"
#include "internal/effectsconfiguration.h"
#include "internal/effectsactionscontroller.h"
#include "internal/effectsuiengine.h"
#include "internal/effectsuiactions.h"

#ifdef AU_MODULE_VST
#include "internal/au3/vst3pluginsscanner.h"
#include "internal/au3/vst3pluginsmetareader.h"
#endif

#include "effectsettings.h"

#include "view/effectbuilder.h"

using namespace au::effects;

static void effects_init_qrc()
{
    Q_INIT_RESOURCE(effects);
}

std::string EffectsModule::moduleName() const
{
    return "effects";
}

void EffectsModule::registerExports()
{
    m_provider = std::make_shared<EffectsProvider>();
    m_configuration = std::make_shared<EffectsConfiguration>();
    m_actionsController = std::make_shared<EffectsActionsController>();

    ioc()->registerExport<IEffectsProvider>(moduleName(), m_provider);
    ioc()->registerExport<IEffectsConfiguration>(moduleName(), m_configuration);
    ioc()->registerExport<IEffectsUiEngine>(moduleName(), new EffectsUiEngine());
}

void EffectsModule::resolveImports()
{
    auto ar = ioc()->resolve<muse::ui::IUiActionsRegister>(moduleName());
    if (ar) {
        ar->reg(std::make_shared<EffectsUiActions>());
    }
    auto ir = ioc()->resolve<muse::ui::IInteractiveUriRegister>(moduleName());
    if (ir) {
        ir->registerQmlUri(muse::Uri("audacity://effects/viewer"), "Audacity/Effects/EffectsViewerDialog.qml");
    }

    auto scannerRegister = ioc()->resolve<muse::audioplugins::IAudioPluginsScannerRegister>(moduleName());
    if (scannerRegister) {
#ifdef AU_MODULE_VST
        scannerRegister->registerScanner(std::make_shared<Vst3PluginsScanner>());
#endif
    }

    auto metaReaderRegister = ioc()->resolve<muse::audioplugins::IAudioPluginMetaReaderRegister>(moduleName());
    if (metaReaderRegister) {
#ifdef AU_MODULE_VST
        metaReaderRegister->registerReader(std::make_shared<Vst3PluginsMetaReader>());
#endif
    }
}

void EffectsModule::registerResources()
{
    effects_init_qrc();
}

void EffectsModule::registerUiTypes()
{
    qmlRegisterType<EffectBuilder>("Audacity.Effects", 1, 0, "EffectBuilder");
}

void EffectsModule::onInit(const muse::IApplication::RunMode& mode)
{
    if (mode != muse::IApplication::RunMode::GuiApp) {
        return;
    }

    PluginManager::Get().Initialize([](const FilePath& localFileName) {
        return std::make_unique<au3::EffectSettings>(localFileName.ToStdString());
    });

    m_actionsController->init();
}

void EffectsModule::onDelayedInit()
{
    m_provider->reloadEffects();
}
