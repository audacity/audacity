/*
 * Audacity: A Digital Audio Editor
 */
#include "vsteffectsstubmodule.h"

static void vst_init_qrc()
{
    Q_INIT_RESOURCE(vst);
}

namespace au::effects {
std::string VstEffectsStubModule::moduleName() const
{
    return "effects_vst_stub";
}

void VstEffectsStubModule::registerResources()
{
    vst_init_qrc();
}
}
