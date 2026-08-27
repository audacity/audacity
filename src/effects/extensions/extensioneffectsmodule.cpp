/*
 * Audacity: A Digital Audio Editor
 */
#include "extensioneffectsmodule.h"

#include <memory>
#include <string>

#include "framework/extensions/internal/extensionsprovider.h"
#include "framework/audioplugins/iaudiopluginmetareaderregister.h"
#include "framework/audioplugins/iaudiopluginsscannerregister.h"
#include "framework/audioplugins/iregisteraudiopluginsscenario.h"
#include "framework/extensions/iextensionsprovider.h"
#include "effects/effects_base/ieffectloadersregister.h"
#include "effects/effects_base/ieffectviewlaunchregister.h"
#include "effects/effects_base/iparameterextractorregistry.h"

#include "effects/effects_base/effectstypes.h"

#include "internal/extensioneffectloader.h"
#include "internal/extensiontypes.h"
#include "internal/extensioneffectsmetareader.h"
#include "internal/extensioneffectsrepository.h"
#include "internal/extensioneffectsscanner.h"
#include "internal/extensionparameterextractor.h"
#include "internal/extensionviewlauncher.h"

namespace au::effects::extensions {
namespace {
const std::string mname = "effects_extensions";
}

ExtensionEffectsModule::ExtensionEffectsModule()
    : m_extensionsProvider(std::make_shared<muse::extensions::ExtensionsProvider>(muse::modularity::globalCtx())),
    m_repository(std::make_shared<ExtensionEffectsRepository>()), m_effectLoader(std::make_shared<ExtensionEffectLoader>(m_repository)),
    m_scanner(std::make_shared<ExtensionEffectsScanner>(m_extensionsProvider, m_repository, m_effectLoader))
{
}

std::string ExtensionEffectsModule::moduleName() const
{
    return mname;
}

void ExtensionEffectsModule::resolveImports()
{
    if (auto registry = muse::modularity::globalIoc()->resolve<muse::audioplugins::IAudioPluginsScannerRegister>(mname)) {
        registry->registerScanner(m_scanner);
    }
    if (auto registry = muse::modularity::globalIoc()->resolve<muse::audioplugins::IAudioPluginMetaReaderRegister>(mname)) {
        registry->registerReader(std::make_shared<ExtensionEffectsMetaReader>(m_repository));
    }
    if (auto registry = muse::modularity::globalIoc()->resolve<IEffectLoadersRegister>(mname)) {
        registry->registerLoader(m_effectLoader);
    }
    if (auto registry = muse::modularity::globalIoc()->resolve<IParameterExtractorRegistry>(mname)) {
        registry->registerExtractor(std::make_shared<ExtensionParameterExtractor>());
    }
}

void ExtensionEffectsModule::onInit(const muse::IApplication::RunMode&)
{
    m_extensionsProvider->reloadExtensions();
    m_repository->initialize(m_extensionsProvider->manifestList(muse::extensions::Filter::Enabled));
}

void ExtensionEffectsModule::onDeinit()
{
    m_effectLoader->deinit();
}

muse::modularity::IContextSetup* ExtensionEffectsModule::newContext(const muse::modularity::ContextPtr& context) const
{
    return new ExtensionEffectsContext(context, m_scanner);
}

void ExtensionEffectsContext::resolveImports()
{
    if (auto registry = ioc()->resolve<IEffectViewLaunchRegister>(mname)) {
        registry->regLauncher(EffectFamily::Extension, std::make_shared<ExtensionViewLauncher>(iocContext()));
    }
}

void ExtensionEffectsContext::onInit(const muse::IApplication::RunMode&)
{
    if (auto provider = ioc()->resolve<muse::extensions::IExtensionsProvider>(mname)) {
        provider->manifestListChanged().onNotify(this, [this] {
            refreshPlugins();
        });
        provider->enabledChanged().onReceive(this, [this](const muse::extensions::ExtensionUri&) {
            refreshPlugins();
        });

        refreshPlugins();
    }
}

void ExtensionEffectsContext::refreshPlugins() const
{
    if (auto scenario = ioc()->resolve<muse::audioplugins::IRegisterAudioPluginsScenario>(mname)) {
        m_scanner->refreshPlugins(*scenario);
    }
}
} // namespace au::effects::extensions
