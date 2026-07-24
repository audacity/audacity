/*
 * Audacity: A Digital Audio Editor
 */
#include "audacityplugineffectsmodule.h"

#include "effects/effects_base/ieffectloadersregister.h"
#include "effects/effects_base/ieffectviewlaunchregister.h"
#include "effects/effects_base/iparameterextractorregistry.h"

#include "framework/audioplugins/iaudiopluginmetareaderregister.h"
#include "framework/audioplugins/iaudiopluginsscannerregister.h"
#include "framework/global/modularity/ioc.h"

#include "internal/audacityplugineffectloader.h"
#include "internal/audacitypluginparameterextractorservice.h"
#include "internal/audacitypluginmetareader.h"
#include "internal/audacitypluginscanner.h"
#include "internal/audacitypluginviewlauncher.h"

namespace au::effects {
namespace {
const std::string mname("effects_audacity_plugin");
}

AudacityPluginEffectsModule::AudacityPluginEffectsModule()
    : m_effectLoader(std::make_shared<AudacityPluginEffectLoader>())
{
}

std::string AudacityPluginEffectsModule::moduleName() const
{
    return mname;
}

void AudacityPluginEffectsModule::resolveImports()
{
    if (auto scanners = globalIoc()->resolve<muse::audioplugins::IAudioPluginsScannerRegister>(moduleName())) {
        scanners->registerScanner(std::make_shared<AudacityPluginScanner>());
    }
    if (auto readers = globalIoc()->resolve<muse::audioplugins::IAudioPluginMetaReaderRegister>(moduleName())) {
        readers->registerReader(std::make_shared<AudacityPluginMetaReader>());
    }
    if (auto loaders = globalIoc()->resolve<IEffectLoadersRegister>(moduleName())) {
        loaders->registerLoader(m_effectLoader);
    }
    if (auto extractors = globalIoc()->resolve<IParameterExtractorRegistry>(moduleName())) {
        extractors->registerExtractor(std::make_shared<AudacityPluginParameterExtractorService>());
    }
}

void AudacityPluginEffectsModule::onDeinit()
{
    m_effectLoader->deinit();
}

muse::modularity::IContextSetup* AudacityPluginEffectsModule::newContext(
    const muse::modularity::ContextPtr& context) const
{
    return new AudacityPluginEffectsContext(context);
}

void AudacityPluginEffectsContext::resolveImports()
{
    if (auto launchers = ioc()->resolve<IEffectViewLaunchRegister>(mname)) {
        launchers->regLauncher(EffectFamily::AudacityPlugin,
                               std::make_shared<AudacityPluginViewLauncher>(iocContext()));
    }
}
} // namespace au::effects
