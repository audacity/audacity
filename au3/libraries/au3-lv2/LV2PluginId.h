/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <lilv/lilv.h>

#include <string>
#include <optional>

/*!
 * @brief To uniquely identify an LV2 plugin's installation on a system,
 * both the installation directory and the plugin URI are needed.
 */
class LV2PluginId
{
    // For internal use only
    friend class LV2EffectsModule;
    friend class LV2EffectBase;

    LV2PluginId(const LilvPlugin*);
    LV2PluginId(std::string pluginUri, std::string bundleUri);

    const std::string pluginUri;
    const std::string bundleUri;
    std::string Serialize() const;
    static std::optional<LV2PluginId> Deserialize(const std::string&);
    bool operator==(const LV2PluginId&) const;
    bool operator!=(const LV2PluginId&) const;
};
