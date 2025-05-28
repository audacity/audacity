/*
 * Audacity: A Digital Audio Editor
 */
#include "vsteffectsmodule.h"

static void vst_init_qrc()
{
    Q_INIT_RESOURCE(vst);
}

namespace au::effects {
std::string VstEffectsModule::moduleName() const
{
    return "effects_vst_stub";
}

void VstEffectsModule::registerResources()
{
    vst_init_qrc();
}
}
