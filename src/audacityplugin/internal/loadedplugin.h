/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <filesystem>
#include <memory>
#include <mutex>
#include <string>
#include <vector>

#include "manifestreader.h"
#include "abi.h"
#include "pluginlibrary.h"

namespace au::audacityplugin {
class EffectInstance;

class LoadedPlugin : public std::enable_shared_from_this<LoadedPlugin>
{
public:
    static std::shared_ptr<LoadedPlugin> load(
        Manifest manifest, const std::filesystem::path& bundlePath, const std::filesystem::path& dataPath, std::string& error);

    ~LoadedPlugin();

    LoadedPlugin(const LoadedPlugin&) = delete;
    LoadedPlugin& operator=(const LoadedPlugin&) = delete;

    const std::string& id() const noexcept { return m_manifest.pluginId; }
    const std::vector<EffectDescriptor>& effects() const noexcept { return m_effectDescriptors; }
    PluginPreferences preferences() const;
    bool repairPreferenceSchema(std::vector<Value>& values) const;
    Status validatePreferences(const std::vector<Value>& values);
    Status applyPreferences(const std::vector<Value>& values);
    CreateInstanceResult createInstance(const std::string& effectId, const EffectCreateInfo& info);

private:
    friend class EffectInstance;

    LoadedPlugin(Manifest manifest, PluginLibrary library);

    void discoverPreferences();
    bool discoverEffects(const std::filesystem::path& bundlePath, std::string& error);
    Status callPreferences(const std::vector<Value>& values, bool apply);
    void destroyEffect(const aup_effect_v0* effect) noexcept;

    Manifest m_manifest;
    PluginLibrary m_library;
    const aup_plugin_v0* m_root = nullptr;
    const aup_preferences_v0* m_preferences = nullptr;
    const aup_effects_v0* m_effects = nullptr;
    std::vector<PluginPreference> m_preferenceItems;
    std::vector<EffectDescriptor> m_effectDescriptors;
    mutable std::mutex m_mutex;
};
} // namespace au::audacityplugin
