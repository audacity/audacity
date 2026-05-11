/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/global/modularity/ioc.h"

#include "framework/audioplugins/iknownaudiopluginsmigrationregister.h"

namespace au::effects {
// Configures the framework's shared known-audio-plugins cache for Audacity.
// Not a module — it's one-shot setup invoked from EffectsModule::resolveImports()
// (before the cache is loaded in the audioplugins context onInit).
class KnownAudioPluginsConfigurator
{
    muse::GlobalInject<muse::audioplugins::IKnownAudioPluginsMigrationRegister> m_migrations;
public:
    void init();
};
}
