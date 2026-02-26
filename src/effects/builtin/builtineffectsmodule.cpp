/*
* Audacity: A Digital Audio Editor
*/
#include "builtineffectsmodule.h"

#include "internal/builtineffectsrepository.h"
#include "internal/builtinviewlauncher.h"

#include "common/builtineffectmodel.h"
#include "common/valuewarper/valuewarper.h"

#include "view/builtineffectviewloader.h"
#include "view/effectsviewregister.h"

#include "effects/effects_base/ieffectviewlaunchregister.h"

using namespace au::effects;

static const std::string mname("effects_builtin");

static void effects_builtin_init_qrc()
{
    Q_INIT_RESOURCE(effects_builtin);
}

std::string BuiltinEffectsModule::moduleName() const
{
    return mname;
}

void BuiltinEffectsModule::registerExports()
{
    m_builtinEffectsRepository = std::make_shared<BuiltinEffectsRepository>();

    globalIoc()->registerExport<IEffectsViewRegister>(mname, new EffectsViewRegister());
    globalIoc()->registerExport<IBuiltinEffectsRepository>(mname, m_builtinEffectsRepository);
}

void BuiltinEffectsModule::registerResources()
{
    effects_builtin_init_qrc();
}

void BuiltinEffectsModule::registerUiTypes()
{
    qmlRegisterUncreatableType<BuiltinEffectModel>("Audacity.BuiltinEffects", 1, 0, "BuiltinEffectModel", "Not creatable abstract type");
    qmlRegisterType<BuiltinEffectViewLoader>("Audacity.BuiltinEffects", 1, 0, "BuiltinEffectViewLoader");
    qmlRegisterType<ValueWarper>("Audacity.BuiltinEffects", 1, 0, "ValueWarper");
}

void BuiltinEffectsModule::onPreInit(const muse::IApplication::RunMode&)
{
    //! NOTE preInit() only creates static Registration objects (doesn't use `this`).
    //! Must run at module level before Au3WrapModule::onInit() sets sInitialized = true.
    BuiltinEffectsRepository::preInit();
}

void BuiltinEffectsModule::onInit(const muse::IApplication::RunMode& mode)
{
    m_builtinEffectsRepository->init();
}

muse::modularity::IContextSetup* BuiltinEffectsModule::newContext(const muse::modularity::ContextPtr& ctx) const
{
    return new BuiltinEffectsContext(ctx);
}

// =====================================================
// BuiltinEffectsContext
// =====================================================

void BuiltinEffectsContext::registerExports()
{
}

void BuiltinEffectsContext::resolveImports()
{
    auto lr = ioc()->resolve<IEffectViewLaunchRegister>(mname);
    if (lr) {
        lr->regLauncher("Audacity" /*builtin*/, std::make_shared<BuiltinViewLauncher>(iocContext()));
    }
}

void BuiltinEffectsContext::onInit(const muse::IApplication::RunMode&)
{
}

void BuiltinEffectsContext::onDeinit()
{
}
