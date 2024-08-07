/*
* Audacity: A Digital Audio Editor
*/
#include "effectsconfiguration.h"

using namespace au::effects;

muse::io::path_t EffectsConfiguration::defaultPath() const
{
    return globalConfiguration()->appDataPath() + "/builtineffects";
}
