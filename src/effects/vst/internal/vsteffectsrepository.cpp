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

    const std::vector<AudioPluginInfo> allEffects = knownPlugins()->pluginInfoList();

    for (const AudioPluginInfo& info : allEffects) {
        if (!(info.type == AudioPluginType::Fx && info.meta.type == AudioResourceType::VstPlugin)) {
            continue;
        }

        EffectMeta meta;
        meta.id = muse::String(info.meta.id.c_str());
        meta.family = EffectFamily::VST3;
        meta.title = muse::io::completeBasename(info.path).toString();
        meta.isRealtimeCapable = true;
        meta.vendor = muse::String::fromStdString(info.meta.vendor);
        meta.type = EffectType::Processor;
        meta.path = info.path;

        effects.push_back(std::move(meta));
    }

    return effects;
}

bool VstEffectsRepository::ensurePluginIsLoaded(const EffectId& effectId) const
{
    if (PluginManager::Get().IsPluginLoaded(au3::wxFromString(effectId))) {
        return true;
    }

    VST3EffectsModule vst3Module;

    const auto plugins = knownPlugins()->pluginInfoList();
    const auto it = std::find_if(plugins.begin(), plugins.end(), [&](const muse::audioplugins::AudioPluginInfo& info) {
        return info.meta.id == effectId.toStdString();
    });
    if (it == plugins.end()) {
        LOGE() << "plugin not in registry: " << effectId;
        return false;
    }
    if (!it->enabled) {
        LOGW() << "plugin disabled: " << effectId;
        return false;
    }
    if (it->meta.type != muse::audio::AudioResourceType::VstPlugin) {
        LOGE() << "not a VST plugin: " << effectId;
        return false;
    }
    const auto& path = it->path;

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
