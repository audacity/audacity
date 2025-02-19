/*
* Audacity: A Digital Audio Editor
*/
#include "vsteffectsrepository.h"

#include "libraries/lib-components/PluginProvider.h"
#include "libraries/lib-strings/TranslatableString.h"
#include "libraries/lib-module-manager/PluginManager.h"
#include "libraries/lib-module-manager/ModuleManager.h"
#include "libraries/lib-vst3/VST3EffectsModule.h"

#include "au3wrap/internal/wxtypes_convert.h"

#include "log.h"

using namespace au::effects;

EffectMetaList VstEffectsRepository::effectMetaList() const
{
    using namespace muse::audioplugins;
    using namespace muse::audio;

    EffectMetaList effects;

    std::vector<AudioPluginInfo> allEffects = knownPlugins()->pluginInfoList();

    for (const AudioPluginInfo& info : allEffects) {
        if (!(info.type == AudioPluginType::Fx && info.meta.type == AudioResourceType::VstPlugin)) {
            continue;
        }

        bool ok = registerPlugin(info.path);
        if (!ok) {
            continue;
        }

        EffectMeta meta;
        meta.id = muse::String(info.meta.id.c_str());
        meta.title = muse::io::completeBasename(info.path).toString();

        meta.categoryId = VST_CATEGORY_ID;

        effects.push_back(std::move(meta));
    }

    return effects;
}

bool VstEffectsRepository::registerPlugin(const muse::io::path_t& path) const
{
    VST3EffectsModule vst3Module;

    PluginID pluginId;
    TranslatableString errorMessage{};
    int numPlugins = vst3Module.DiscoverPluginsAtPath(
        au3::wxFromString(path.toString()), errorMessage,
        [&](PluginProvider* provider, ComponentInterface* ident) -> const PluginID&
    {
        pluginId = PluginManager::DefaultRegistrationCallback(provider, ident);
        return pluginId;
    });

    if (numPlugins == 0) {
        LOGE() << "not found plugin: " << path;
        return false;
    }

    const auto ptr = PluginManager::Get().GetPlugin(pluginId);
    if (!ptr) {
        LOGE() << "failed register plugin: " << au3::wxToStdSting(pluginId) << ", path: " << path;
        return false;
    }

    return true;
}
