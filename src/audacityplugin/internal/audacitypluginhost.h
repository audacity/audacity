/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <filesystem>
#include <map>
#include <memory>
#include <mutex>
#include <string>
#include <vector>

#include "framework/global/iglobalconfiguration.h"
#include "framework/global/modularity/ioc.h"

#include "../iaudacitypluginhost.h"
#include "loadedplugin.h"

namespace au::audacityplugin {
class AudacityPluginHost final : public IAudacityPluginHost
{
    muse::GlobalInject<muse::IGlobalConfiguration> m_globalConfiguration;

public:
    void initialize();
    void shutdown();
    const std::vector<EffectDescriptor>& effects() const override;
    std::vector<PluginPreferences> preferences() const override;
    Status validatePreferences(const std::string& pluginId, const std::vector<Value>& values) override;
    Status applyPreferences(const std::string& pluginId, const std::vector<Value>& values) override;
    CreateInstanceResult createInstance(const std::string& pluginId, const std::string& effectId, const EffectCreateInfo& info) override;

private:
    enum class Scope {
        Application, User
    };

    struct Candidates {
        std::vector<std::shared_ptr<LoadedPlugin> > application;
        std::vector<std::shared_ptr<LoadedPlugin> > user;
    };

    void scanScope(const std::filesystem::path& root, Scope scope, const std::filesystem::path& dataRoot, std::map<std::string,
                                                                                                                   Candidates>& candidates);
    std::shared_ptr<LoadedPlugin> loadBundle(
        const std::filesystem::path& bundle, const std::filesystem::path& dataRoot);
    void restorePreferences(const std::shared_ptr<LoadedPlugin>& plugin);
    bool persistPreferences(const PluginPreferences& preferences, const std::vector<Value>& values) const;
    mutable std::mutex m_mutex;
    std::map<std::string, std::shared_ptr<LoadedPlugin> > m_plugins;
    std::vector<EffectDescriptor> m_effects;
};
} // namespace au::audacityplugin
