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

void Au3PluginRegistry::Save(const ::PluginMap& pluginMap)
{
    AudioPluginInfoList newPlugins;

    for (const auto& [id, plug] : pluginMap) {
        if (plug.GetPluginType() != PluginTypeEffect) {
            continue;
        }

        const EffectFamily family = utils::effectFamilyFromString(au3::wxToString(plug.GetEffectFamily()));
        const muse::String title = au3::wxToString(plug.GetSymbol().Msgid().MSGID().GET());
        const EffectMeta meta = toEffectMeta(plug, family, title, muse::String {});

        AudioPluginInfo info;
        info.type = AudioPluginType::Fx;
        info.meta = utils::auToMuseEffectMeta(meta);
        info.path = muse::io::path_t(au3::wxToString(plug.GetPath()));
        info.enabled = true;

        newPlugins.push_back(std::move(info));
    }

    const auto filePath = audioPluginsConfiguration()->knownAudioPluginsFilePath();
    if (fileSystem()->exists(filePath)) {
        // Remove the file and reload the register.
        fileSystem()->remove(filePath);
        knownPlugins()->load();
    }

    knownPlugins()->registerPlugins(newPlugins);
}
}
