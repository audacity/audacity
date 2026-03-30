/*
* Audacity: A Digital Audio Editor
*/
#include "nyquisteffectsmodule.h"

#include "audioplugins/iaudiopluginsscannerregister.h"
#include "audioplugins/iaudiopluginmetareaderregister.h"

#include "effects/effects_base/iparameterextractorregistry.h"
#include "effects/effects_base/ieffectviewlaunchregister.h"
#include "effects/effects_base/ieffectloadersregister.h"

#include "internal/nyquisteffectsrepository.h"
#include "internal/nyquistparameterextractorservice.h"
#include "internal/nyquistviewlauncher.h"
#include "internal/nyquistpluginsscanner.h"
#include "internal/nyquistpluginsmetareader.h"
#include "internal/nyquisteffectsloader.h"

#include "nyquistprompt/nyquistpromptloader.h"
#include "nyquistprompt/nyquistpromptviewmodel.h"

namespace {
const std::string mname("effects_nyquist");
}

static void nyquist_init_qrc()
{
    Q_INIT_RESOURCE(nyquist);
}

au::effects::NyquistEffectsModule::NyquistEffectsModule()
    : m_nyquistEffectsRepository(std::make_unique<NyquistEffectsRepository>(muse::modularity::globalCtx()))
{
}

au::effects::NyquistEffectsModule::~NyquistEffectsModule() = default;

std::string au::effects::NyquistEffectsModule::moduleName() const
{
    return mname;
}

void au::effects::NyquistEffectsModule::registerExports()
{
}

void au::effects::NyquistEffectsModule::registerResources()
{
    nyquist_init_qrc();
}

void au::effects::NyquistEffectsModule::resolveImports()
{
    auto scannerRegister = globalIoc()->resolve<muse::audioplugins::IAudioPluginsScannerRegister>(mname);
    if (scannerRegister) {
        scannerRegister->registerScanner(std::make_shared<NyquistPluginsScanner>());
    }

    m_nyquistMetaReader = std::make_shared<NyquistPluginsMetaReader>();
    auto metaReaderRegister = globalIoc()->resolve<muse::audioplugins::IAudioPluginMetaReaderRegister>(moduleName());
    if (metaReaderRegister) {
        metaReaderRegister->registerReader(m_nyquistMetaReader);
    }

    m_effectLoader = std::make_shared<NyquistEffectsLoader>();
    auto loadersRegister = globalIoc()->resolve<IEffectLoadersRegister>(moduleName());
    if (loadersRegister) {
        loadersRegister->registerLoader(m_effectLoader);
    }
}

void au::effects::NyquistEffectsModule::onPreInit(const muse::IApplication::RunMode& runMode)
{
    if (runMode != muse::IApplication::RunMode::AudioPluginRegistration) {
        m_nyquistPromptLoader = std::make_unique<NyquistPromptLoader>();
        m_nyquistPromptLoader->preInit();
    }
}

void au::effects::NyquistEffectsModule::onInit(const muse::IApplication::RunMode&)
{
    m_nyquistMetaReader->init();
    m_effectLoader->init();
    m_nyquistEffectsRepository->init();
}

void au::effects::NyquistEffectsModule::onAllInited(const muse::IApplication::RunMode&)
{
    // onAllInited, because it depends on Au3Wrap and BuiltinEffects modules
    if (m_nyquistPromptLoader) {
        m_nyquistPromptLoader->init();
    }
}

void au::effects::NyquistEffectsModule::onDeinit()
{
    m_nyquistMetaReader->deinit();
}

muse::modularity::IContextSetup* au::effects::NyquistEffectsModule::newContext(const muse::modularity::ContextPtr& ctx) const
{
    return new NyquistEffectsContext(ctx);
}

au::effects::NyquistEffectsContext::NyquistEffectsContext(const muse::modularity::ContextPtr& ctx)
    : muse::modularity::IContextSetup(ctx)
{
}

void au::effects::NyquistEffectsContext::resolveImports()
{
    auto paramExtractorRegistry = muse::modularity::globalIoc()->resolve<IParameterExtractorRegistry>(mname);
    if (paramExtractorRegistry) {
        paramExtractorRegistry->registerExtractor(std::make_shared<NyquistParameterExtractorService>());
    }

    auto launchRegister = ioc()->resolve<IEffectViewLaunchRegister>(mname);
    if (launchRegister) {
        launchRegister->regLauncher("Nyquist", std::make_shared<NyquistViewLauncher>(iocContext()));
    }
}

void au::effects::NyquistEffectsContext::onDeinit()
{
}
