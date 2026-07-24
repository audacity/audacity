/*
* Audacity: A Digital Audio Editor
*/
#include "lv2effectsmodule.h"

#include "framework/audioplugins/iaudiopluginsscannerregister.h"
#include "framework/audioplugins/iaudiopluginmetareaderregister.h"

#include "effects/effects_base/ieffectloadersregister.h"
#include "effects/effects_base/ieffectviewlaunchregister.h"
#include "effects/effects_base/iparameterextractorregistry.h"
#include "effects/effects_base/view/effectsviewutils.h"

#include "internal/lv2effectloader.h"
#include "internal/lv2parameterextractorservice.h"
#include "internal/lv2pluginmetareader.h"
#include "internal/lv2pluginsscanner.h"
#include "internal/lv2viewlauncher.h"

#include "view/lv2viewmodel.h"

#include "log.h"

#include "au3-lv2/LoadLV2.h"

using namespace muse;
using namespace au::effects;

static const std::string mname("effects_lv2");

static void lv2_init_qrc()
{
    Q_INIT_RESOURCE(lv2);
}

Lv2EffectsModule::Lv2EffectsModule()
    : m_metaReader{std::make_shared<Lv2PluginMetaReader>()}, m_effectLoader{std::make_shared<Lv2EffectLoader>()},
    m_pluginsScanner{std::make_shared<Lv2PluginsScanner>()}
{
}

std::string Lv2EffectsModule::moduleName() const
{
    return mname;
}

void Lv2EffectsModule::registerExports()
{
}

void Lv2EffectsModule::resolveImports()
{
    auto scannerRegister = globalIoc()->resolve<muse::audioplugins::IAudioPluginsScannerRegister>(mname);
    if (scannerRegister) {
        scannerRegister->registerScanner(m_pluginsScanner);
    }

    auto metaReaderRegister = globalIoc()->resolve<muse::audioplugins::IAudioPluginMetaReaderRegister>(mname);
    if (metaReaderRegister) {
        metaReaderRegister->registerReader(m_metaReader);
    }

    auto paramExtractorRegistry = globalIoc()->resolve<IParameterExtractorRegistry>(mname);
    if (paramExtractorRegistry) {
        paramExtractorRegistry->registerExtractor(std::make_shared<Lv2ParameterExtractorService>());
    }

    auto loadersRegister = globalIoc()->resolve<IEffectLoadersRegister>(mname);
    if (loadersRegister) {
        loadersRegister->registerLoader(m_effectLoader);
    }

    // auto ir = ioc()->resolve<IInteractiveUriRegister>(mname);
    // if (ir) {
    //     ir->registerQmlUri(Uri("audacity://effects/lv2_viewer"), "Audacity/Lv2/Lv2ViewerDialog.qml");
    // }
}

void Lv2EffectsModule::registerResources()
{
    lv2_init_qrc();
}

void Lv2EffectsModule::registerUiTypes()
{
    REGISTER_AUDACITY_EFFECTS_SINGLETON_TYPE(Lv2ViewModelFactory);
}

void Lv2EffectsModule::onInit(const muse::IApplication::RunMode& mode)
{
    m_metaReader->init();
    m_effectLoader->init();
    m_pluginsScanner->init(mode);
}

void Lv2EffectsModule::onDeinit()
{
    m_effectLoader->deinit();
    m_pluginsScanner->deinit();
    m_metaReader->deinit();
}

muse::modularity::IContextSetup* Lv2EffectsModule::newContext(const muse::modularity::ContextPtr& ctx) const
{
    return new Lv2EffectsContext(ctx);
}

// =====================================================
// Lv2EffectsContext
// =====================================================

void Lv2EffectsContext::registerExports()
{
}

void Lv2EffectsContext::resolveImports()
{
    auto lr = ioc()->resolve<IEffectViewLaunchRegister>(mname);
    if (lr) {
        lr->regLauncher(EffectFamily::LV2, std::make_shared<Lv2ViewLauncher>(iocContext()));
    }
}

void Lv2EffectsContext::onDeinit()
{
}
