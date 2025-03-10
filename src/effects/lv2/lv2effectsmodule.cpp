/*
* Audacity: A Digital Audio Editor
*/
#include "lv2effectsmodule.h"

using namespace muse;
using namespace au::effects;

static void lv2_init_qrc()
{
    Q_INIT_RESOURCE(lv2);
}

std::string Lv2EffectsModule::moduleName() const
{
    return "effects_lv2";
}

void Lv2EffectsModule::registerExports()
{
}

void Lv2EffectsModule::resolveImports()
{
}

void Lv2EffectsModule::registerResources()
{
    //lv2_init_qrc();
}

void Lv2EffectsModule::registerUiTypes()
{
}

void Lv2EffectsModule::onInit(const muse::IApplication::RunMode&)
{
}

void Lv2EffectsModule::onDeinit()
{
}
