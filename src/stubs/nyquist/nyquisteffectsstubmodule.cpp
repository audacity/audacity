/*
 * Audacity: A Digital Audio Editor
 */
#include "nyquisteffectsstubmodule.h"

static void nyquist_init_qrc()
{
    Q_INIT_RESOURCE(nyquist);
}

namespace au::effects {
std::string NyquistEffectsModule::moduleName() const
{
    return "effects_nyquist_stub";
}

void NyquistEffectsModule::registerResources()
{
    nyquist_init_qrc();
}
}
