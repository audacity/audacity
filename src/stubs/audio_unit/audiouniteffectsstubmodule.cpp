/*
 * Audacity: A Digital Audio Editor
 */
#include "audiouniteffectsstubmodule.h"

static void AudioUnitInitQrc()
{
    Q_INIT_RESOURCE(audiounit);
}

namespace au::effects {
std::string AudioUnitEffectsModule::moduleName() const
{
    return "effects_audio_unit_stub";
}

void AudioUnitEffectsModule::registerResources()
{
    AudioUnitInitQrc();
}
}
