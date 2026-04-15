/*
* Audacity: A Digital Audio Editor
*/
#include "builtineffectsmodule.h"

#include "internal/builtineffectsrepository.h"

#include "view/builtineffectmodel.h"
#include "view/builtineffectviewloader.h"
#include "view/builtineffectsviewregister.h"

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
    globalIoc()->registerExport<IBuiltinEffectsViewRegister>(moduleName(), new BuiltinEffectsViewRegister());
    globalIoc()->registerExport<IBuiltinEffectsRepository>(moduleName(), new BuiltinEffectsRepository());
}

void BuiltinEffectsModule::resolveImports()
{
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
}

void BuiltinEffectsModule::onDelayedInit()
{
}
