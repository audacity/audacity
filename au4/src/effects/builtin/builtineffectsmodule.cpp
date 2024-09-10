/*
* Audacity: A Digital Audio Editor
*/
#include "builtineffectsmodule.h"

#include "internal/builtineffectsrepository.h"

using namespace au::effects;

static void effects_builtin_init_qrc()
{
    Q_INIT_RESOURCE(effects_builtin);
}

std::string BuiltinEffectsModule::moduleName() const
{
    return "effects_builtin";
}

void BuiltinEffectsModule::registerExports()
{
    m_builtinEffectsRepository = std::make_shared<BuiltinEffectsRepository>();

    ioc()->registerExport<IBuiltinEffectsRepository>(moduleName(), m_builtinEffectsRepository);
}

void BuiltinEffectsModule::registerResources()
{
    effects_builtin_init_qrc();
}

void BuiltinEffectsModule::onPreInit(const muse::IApplication::RunMode&)
{
    //! NOTE Should be before PluginManager::Get().Initialize
    //! So it call on pre init
    m_builtinEffectsRepository->init();
}
