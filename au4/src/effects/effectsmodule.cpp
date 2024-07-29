/*
* Audacity: A Digital Audio Editor
*/
#include "effectsmodule.h"

using namespace au::effects;

static void effects_init_qrc()
{
    Q_INIT_RESOURCE(effects);
}

std::string EffectsModule::moduleName() const
{
    return "effects";
}

void EffectsModule::registerExports()
{
}

void EffectsModule::resolveImports()
{
}

void EffectsModule::registerResources()
{
    // effects_init_qrc();
}

void EffectsModule::registerUiTypes()
{
}

void EffectsModule::onInit(const muse::IApplication::RunMode&)
{
}

void EffectsModule::onDeinit()
{
}
