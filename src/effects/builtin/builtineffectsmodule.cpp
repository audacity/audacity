/*
* Audacity: A Digital Audio Editor
*/
#include "builtineffectsmodule.h"

#include "internal/builtineffectsrepository.h"
#include "internal/builtinviewlauncher.h"

#include "common/abstracteffectmodel.h"
#include "common/effectmanagemenu.h"

#include "effects/effects_base/ieffectviewlaunchregister.h"

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

void BuiltinEffectsModule::resolveImports()
{
    auto lr = ioc()->resolve<IEffectViewLaunchRegister>(moduleName());
    if (lr) {
        lr->regLauncher("Audacity" /*builtin*/, std::make_shared<BuiltinViewLauncher>());
    }
}

void BuiltinEffectsModule::registerResources()
{
    effects_builtin_init_qrc();
}

void BuiltinEffectsModule::registerUiTypes()
{
    qmlRegisterUncreatableType<AbstractEffectModel>("Audacity.Effects", 1, 0, "AbstractEffectModel", "Not creatable abstract type");
    qmlRegisterType<EffectManageMenu>("Audacity.Effects", 1, 0, "EffectManageMenu");
}

void BuiltinEffectsModule::onPreInit(const muse::IApplication::RunMode&)
{
    m_builtinEffectsRepository->preInit();
}

void BuiltinEffectsModule::onInit(const muse::IApplication::RunMode&)
{
    m_builtinEffectsRepository->init();
}
