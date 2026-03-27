/*
 * Audacity: A Digital Audio Editor
 */

#include "au3pluginregistry.h"

#include "../effectsutils.h"
#include "au3effectsutils.h"
#include "au3wrap/internal/wxtypes_convert.h"

#include "framework/global/log.h"

using namespace muse::audioplugins;

namespace au::effects {
void Au3PluginRegistry::Load(::PluginMap& pluginMap)
{
    const AudioPluginInfoList plugins = knownPlugins()->pluginInfoList();

    for (const AudioPluginInfo& info : plugins) {
        if (!info.enabled) {
            continue;
        }

        const EffectMeta meta = utils::museToAuEffectMeta(info.path, info.meta);

        const wxString id = meta.id.toStdString();

        PluginDescriptor& plug = pluginMap[id];
        plug.SetID(id);
        plug.SetPluginType(PluginTypeEffect);
        plug.SetPath(meta.path.toStdString());
        plug.SetSymbol(ComponentInterfaceSymbol { meta.title.toStdString() });
        plug.SetProviderID(meta.module.toStdString());
        plug.SetVendor(meta.vendor.toStdString());
        plug.SetVersion(meta.version.toStdString());
        plug.SetEffectFamily(utils::effectFamilyToString(meta.family).toStdString());
        plug.SetEffectType(toAu3EffectType(meta.type));
        plug.SetEffectGroup(toAu3EffectGroup(utils::effectCategoryFromString(meta.category)));
        plug.SetRealtimeSupport(
            meta.isRealtimeCapable
            ? EffectDefinitionInterface::RealtimeSince::Always
            : EffectDefinitionInterface::RealtimeSince::Never);
        plug.SetEnabled(meta.isActivated);
        plug.SetValid(info.enabled);

        // Disregarding:
        // - `SetEffectDefault`
        // - `SetEffectLegacy`
        // - `SetEffectAutomatable`
        // - `SetImporterIdentifier`
        // - `SetImporterFilterDescription`
        // - `SetImporterExtensions`
    }

    LOGD() << "Loaded " << plugins.size() << " plugins from known plugins register";
}

void Au3PluginRegistry::Save(const ::PluginMap& pluginMap, bool overwrite)
{
    knownPlugins()->load();
    AudioPluginInfoList existingPlugins = knownPlugins()->pluginInfoList();
    AudioPluginInfoList newPlugins;

    for (const auto& [id, plug] : pluginMap) {
        if (plug.GetPluginType() != PluginTypeEffect) {
            continue;
        }

        const EffectFamily family = utils::effectFamilyFromString(au3::wxToString(plug.GetEffectFamily()));
        const muse::String title = au3::wxToString(plug.GetSymbol().Msgid().MSGID().GET());
        const EffectMeta meta = toEffectMeta(plug, family, title, muse::String {}, false);

        AudioPluginInfo info;
        info.type = AudioPluginType::Fx;
        info.meta = utils::auToMuseEffectMeta(meta);
        info.path = muse::io::path_t(au3::wxToString(plug.GetPath()));
        info.enabled = true;

        const auto it = std::find_if(existingPlugins.begin(), existingPlugins.end(),
                                     [&info](const AudioPluginInfo& existingInfo) {
            return existingInfo.path == info.path;
        });

        if (it == existingPlugins.end()) {
            newPlugins.emplace_back(std::move(info));
        } else if (overwrite) {
            existingPlugins.erase(it);
            newPlugins.emplace_back(std::move(info));
        }
    }

    const auto filePath = audioPluginsConfiguration()->knownAudioPluginsFilePath();
    if (fileSystem()->exists(filePath)) {
        fileSystem()->remove(filePath);
        knownPlugins()->load(); // Clears the in-memory information
    }

    knownPlugins()->registerPlugins(existingPlugins);
    knownPlugins()->registerPlugins(newPlugins);
}
}
