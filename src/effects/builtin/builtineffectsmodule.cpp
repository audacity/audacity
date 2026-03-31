/*
* Audacity: A Digital Audio Editor
*/
#include "builtineffectsmodule.h"

#include "effects/effects_base/ieffectloadersregister.h"

#include "internal/builtineffectsloader.h"
#include "internal/builtineffectsrepository.h"
#include "internal/builtineffectsmetareader.h"
#include "internal/builtineffectsscanner.h"

#include "view/builtineffectmodel.h"
#include "view/builtineffectviewloader.h"
#include "view/builtineffectsviewregister.h"

#include "framework/audioplugins/iaudiopluginsscannerregister.h"
#include "framework/audioplugins/iaudiopluginmetareaderregister.h"

static void effects_builtin_init_qrc()
{
    Q_INIT_RESOURCE(effects_builtin);
}

namespace au::effects {
std::string BuiltinEffectsModule::moduleName() const
{
    return "effects_builtin";
}

BuiltinEffectsModule::BuiltinEffectsModule()
    : m_effectLoader(std::make_shared<BuiltinEffectsLoader>()), m_pluginsScanner(std::make_shared<BuiltinEffectsScanner>())
{
}

void BuiltinEffectsModule::registerExports()
{
    globalIoc()->registerExport<IBuiltinEffectsViewRegister>(moduleName(), new BuiltinEffectsViewRegister());
    globalIoc()->registerExport<IBuiltinEffectsRepository>(moduleName(), new BuiltinEffectsRepository());
}

void BuiltinEffectsModule::resolveImports()
{
    const auto scannerRegister = globalIoc()->resolve<muse::audioplugins::IAudioPluginsScannerRegister>(moduleName());
    if (scannerRegister) {
        scannerRegister->registerScanner(m_pluginsScanner);
    }

    const auto metaReaderRegister = globalIoc()->resolve<muse::audioplugins::IAudioPluginMetaReaderRegister>(moduleName());
    if (metaReaderRegister) {
        metaReaderRegister->registerReader(std::make_shared<BuiltinEffectsMetaReader>());
    }

    auto loadersRegister = globalIoc()->resolve<IEffectLoadersRegister>(moduleName());
    if (loadersRegister) {
        loadersRegister->registerLoader(m_effectLoader);
    }
}

void BuiltinEffectsModule::registerResources()
{
    effects_builtin_init_qrc();
}

void BuiltinEffectsModule::registerUiTypes()
{
    qmlRegisterUncreatableType<BuiltinEffectModel>("Audacity.BuiltinEffects", 1, 0, "BuiltinEffectModel", "Not creatable abstract type");
    qmlRegisterType<BuiltinEffectViewLoader>("Audacity.BuiltinEffects", 1, 0, "BuiltinEffectViewLoader");
}

void BuiltinEffectsModule::onInit(const muse::IApplication::RunMode&)
{
    m_effectLoader->init();
    m_pluginsScanner->init();
}

void BuiltinEffectsModule::onDelayedInit()
{
}
}
