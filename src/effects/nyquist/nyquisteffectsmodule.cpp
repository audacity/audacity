/*
* Audacity: A Digital Audio Editor
*/
#include "nyquisteffectsmodule.h"

#include "audioplugins/iaudiopluginsscannerregister.h"
#include "audioplugins/iaudiopluginmetareaderregister.h"

#include "effects/effects_base/iparameterextractorregistry.h"
#include "effects/effects_base/ieffectviewlaunchregister.h"

#include "internal/nyquisteffectsrepository.h"
#include "internal/nyquistparameterextractorservice.h"
#include "internal/nyquistviewlauncher.h"
#include "internal/nyquistpluginsscanner.h"
#include "internal/nyquistpluginsmetareader.h"

#include "nyquistprompt/nyquistpromptloader.h"
#include "nyquistprompt/nyquistpromptviewmodel.h"

namespace {
const std::string mname("effects_nyquist");
}

static void nyquist_init_qrc()
{
    Q_INIT_RESOURCE(nyquist);
}

au::effects::NyquistEffectsModule::NyquistEffectsModule() = default;
au::effects::NyquistEffectsModule::~NyquistEffectsModule() = default;

std::string au::effects::NyquistEffectsModule::moduleName() const
{
    return mname;
}

void au::effects::NyquistEffectsModule::registerExports()
{
    m_nyquistMetaReader = std::make_shared<NyquistPluginsMetaReader>();
    m_nyquistEffectsRepository = std::make_shared<NyquistEffectsRepository>(muse::modularity::globalCtx(),
                                                                            std::make_unique<NyquistPluginsScanner>(),
                                                                            m_nyquistMetaReader);

    globalIoc()->registerExport<INyquistEffectsRepository>(mname, m_nyquistEffectsRepository);
}

void au::effects::NyquistEffectsModule::registerResources()
{
    nyquist_init_qrc();
}

void au::effects::NyquistEffectsModule::resolveImports()
{
}

void au::effects::NyquistEffectsModule::onPreInit(const muse::IApplication::RunMode& runMode)
{
    if (runMode != muse::IApplication::RunMode::AudioPluginRegistration) {
        m_nyquistPromptLoader = std::make_unique<NyquistPromptLoader>(muse::modularity::globalCtx());
        m_nyquistPromptLoader->preInit();
    }
}

void au::effects::NyquistEffectsModule::onInit(const muse::IApplication::RunMode& runMode)
{
    m_nyquistMetaReader->init(runMode);
    m_nyquistEffectsRepository->init();
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
