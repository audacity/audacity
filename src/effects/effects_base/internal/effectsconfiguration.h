/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../ieffectsconfiguration.h"

#include "modularity/ioc.h"
#include "global/iglobalconfiguration.h"

namespace au::effects {
class EffectsConfiguration : public IEffectsConfiguration
{
    muse::Inject<muse::IGlobalConfiguration> globalConfiguration;

public:
    EffectsConfiguration() = default;

    void init();
};
}
