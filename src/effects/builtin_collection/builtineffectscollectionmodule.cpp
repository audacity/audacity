/*
* Audacity: A Digital Audio Editor
*/
#include "builtineffectscollectionmodule.h"

#include "internal/builtincollectionloader.h"
#include "internal/builtinviewlauncher.h"

#include "common/valuewarper/valuewarper.h"

#include "effects/effects_base/ieffectviewlaunchregister.h"

using namespace au::effects;

static const std::string mname("builtin_effects_collection");

static void builtin_effects_collection_init_qrc()
{
    Q_INIT_RESOURCE(builtin_effects_collection);
}

BuiltinEffectsCollectionModule::BuiltinEffectsCollectionModule() = default;
BuiltinEffectsCollectionModule::~BuiltinEffectsCollectionModule() = default;

std::string BuiltinEffectsCollectionModule::moduleName() const
{
    return mname;
}

void BuiltinEffectsCollectionModule::registerExports()
{
}

void BuiltinEffectsCollectionModule::registerResources()
{
    builtin_effects_collection_init_qrc();
}

void BuiltinEffectsCollectionModule::registerUiTypes()
{
    qmlRegisterType<ValueWarper>("Audacity.BuiltinEffects", 1, 0, "ValueWarper");
}

void BuiltinEffectsCollectionModule::onPreInit(const muse::IApplication::RunMode&)
{
    //! NOTE preInit() only creates static Registration objects (doesn't use `this`).
    //! Must run at module level before Au3WrapModule::onInit() sets sInitialized = true.
    BuiltinCollectionLoader::preInit();

    m_builtinCollectionLoader = std::make_unique<BuiltinCollectionLoader>(muse::modularity::globalCtx());
}

void BuiltinEffectsCollectionModule::onInit(const muse::IApplication::RunMode&)
{
    m_builtinCollectionLoader->init();
}

muse::modularity::IContextSetup* BuiltinEffectsCollectionModule::newContext(const muse::modularity::ContextPtr& ctx) const
{
    return new BuiltinEffectsCollectionContext(ctx);
}

// =====================================================
// BuiltinEffectsCollectionContext
// =====================================================

BuiltinEffectsCollectionContext::BuiltinEffectsCollectionContext(const muse::modularity::ContextPtr& ctx)
    : muse::modularity::IContextSetup(ctx)
{
}

void BuiltinEffectsCollectionContext::registerExports()
{
}

void BuiltinEffectsCollectionContext::resolveImports()
{
    auto lr = ioc()->resolve<IEffectViewLaunchRegister>(mname);
    if (lr) {
        lr->regLauncher(EffectFamily::Builtin, std::make_shared<BuiltinViewLauncher>(iocContext()));
    }
}

void BuiltinEffectsCollectionContext::onDeinit()
{
}
