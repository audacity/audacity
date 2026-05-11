/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/global/modularity/ioc.h"

#include "framework/audioplugins/iknownaudiopluginsmigrationregister.h"

namespace au::effects {
// One-shot setup invoked from EffectsModule::resolveImports(), before the
// audioplugins context loads the cache in onInit.
class KnownAudioPluginsConfigurator
{
    muse::GlobalInject<muse::audioplugins::IKnownAudioPluginsMigrationRegister> m_migrations;
public:
    void init();
};
}
