/*
* Audacity: A Digital Audio Editor
*/
#include "builtineffectsmodule.h"

#include "effects/effects_base/ieffectloadersregister.h"

#include "internal/builtineffectsloader.h"
#include "internal/builtineffectsmetareader.h"
#include "internal/builtineffectsscanner.h"

#include "view/builtineffectsviewregister.h"

#include "framework/audioplugins/iaudiopluginsscannerregister.h"
#include "framework/audioplugins/iaudiopluginmetareaderregister.h"

namespace au::effects {
std::string BuiltinEffectsModule::moduleName() const
{
    return "effects_builtin";
}

BuiltinEffectsModule::BuiltinEffectsModule()
    : m_effectLoader(std::make_shared<BuiltinEffectsLoader>()), m_pluginsScanner(std::make_shared<BuiltinEffectsScanner>()), m_metaReader(
        std::make_shared<BuiltinEffectsMetaReader>())
{
}

void BuiltinEffectsModule::registerExports()
{
    globalIoc()->registerExport<IBuiltinEffectsViewRegister>(moduleName(), new BuiltinEffectsViewRegister());
}

void BuiltinEffectsModule::resolveImports()
{
    const auto scannerRegister = globalIoc()->resolve<muse::audioplugins::IAudioPluginsScannerRegister>(moduleName());
    if (scannerRegister) {
        scannerRegister->registerScanner(m_pluginsScanner);
    }

    const auto metaReaderRegister = globalIoc()->resolve<muse::audioplugins::IAudioPluginMetaReaderRegister>(moduleName());
    if (metaReaderRegister) {
        metaReaderRegister->registerReader(m_metaReader);
    }

    auto loadersRegister = globalIoc()->resolve<IEffectLoadersRegister>(moduleName());
    if (loadersRegister) {
        loadersRegister->registerLoader(m_effectLoader);
    }
}

void BuiltinEffectsModule::onInit(const muse::IApplication::RunMode& mode)
{
    m_effectLoader->init();
    m_pluginsScanner->init(mode);
    m_metaReader->init();
}

void BuiltinEffectsModule::onDelayedInit()
{
}

void BuiltinEffectsModule::onDeinit()
{
    m_effectLoader->deinit();
    m_pluginsScanner->deinit();
    m_metaReader->deinit();
}
}
