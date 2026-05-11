/*
* Audacity: A Digital Audio Editor
*/
#include "audiopluginsappconfigmodule.h"

#include "modularity/ioc.h"

#include "global/serialization/json.h"

#include "framework/audioplugins/iknownaudiopluginsmigrationregister.h"

using namespace au::app;
using namespace muse::audioplugins;

static const std::string mname("audiopluginsappconfig");

std::string AudioPluginsAppConfigModule::moduleName() const
{
    return mname;
}

void AudioPluginsAppConfigModule::resolveImports()
{
    auto migrations = muse::modularity::globalIoc()->resolve<IKnownAudioPluginsMigrationRegister>(moduleName());
    if (!migrations) {
        return;
    }

    // v2 -> v3 is MuseScore's hasNativeEditorSupport migration. Audacity
    // participates in the shared version chain but has no work at this
    // step — audacity files have never carried hasNativeEditorSupport
    // since it's a MuseScore native-editor concept that doesn't apply
    // here. Registering a no-op keeps the migration chain complete so
    // load() can reach the framework's target version cleanly.
    migrations->registerMigration(2, [](const muse::JsonArray& plugins) {
        return plugins;
    });
}
