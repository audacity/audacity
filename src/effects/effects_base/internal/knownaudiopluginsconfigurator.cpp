/*
* Audacity: A Digital Audio Editor
*/
#include "knownaudiopluginsconfigurator.h"

#include "global/serialization/json.h"

using namespace au::effects;

void KnownAudioPluginsConfigurator::init()
{
    if (!m_migrations()) {
        return;
    }

    // v2 -> v3 is MuseScore's hasNativeEditorSupport migration. Audacity
    // participates in the shared version chain but has no work at this
    // step — audacity files have never carried hasNativeEditorSupport
    // since it's a MuseScore native-editor concept that doesn't apply
    // here. Registering a no-op keeps the migration chain complete so
    // load() can reach the framework's target version cleanly.
    m_migrations()->registerMigration(2, [](const muse::JsonArray& plugins) {
        return plugins;
    });
}
