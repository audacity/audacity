/**********************************************************************

  Audacity: A Digital Audio Editor

  @file ConfigInterface.cpp

**********************************************************************/
#include "ConfigInterface.h"

#include "PluginManager.h"

namespace PluginSettings {
bool HasConfigGroup(const EffectDefinitionInterface& ident,
                    PluginSettings::ConfigurationType type,
                    const RegistryPath& group)
{
    auto& pluginManager = PluginManager::Get();
    const auto& id = pluginManager.GetID(&ident);
    if (pluginManager.HasConfigGroup(type, id, group)) {
        return true;
    }
    if (auto id2 = pluginManager.OldGetID(&ident); id != id2) {
        return pluginManager.HasConfigGroup(type, id2, group);
    }
    return false;
}

bool GetConfigSubgroups(const EffectDefinitionInterface& ident,
                        PluginSettings::ConfigurationType type,
                        const RegistryPath& group, RegistryPaths& subgroups)
{
    auto& pluginManager = PluginManager::Get();
    const auto& id = pluginManager.GetID(&ident);
    if (pluginManager.GetConfigSubgroups(
            type, id, group, subgroups)) {
        return true;
    }
    if (auto id2 = pluginManager.OldGetID(&ident); id != id2) {
        return pluginManager.GetConfigSubgroups(type, id2, group, subgroups);
    }
    return false;
}

bool HasConfigValue(const EffectDefinitionInterface& ident,
                    PluginSettings::ConfigurationType type,
                    const RegistryPath& group, const RegistryPath& key)
{
    auto& pluginManager = PluginManager::Get();
    const auto& id = pluginManager.GetID(&ident);
    if (pluginManager.HasConfigValue(type, id, group, key)) {
        return true;
    }
    if (auto id2 = pluginManager.OldGetID(&ident); id != id2) {
        return pluginManager.HasConfigValue(type, id2, group, key);
    }
    return false;
}

bool GetConfigValue(const EffectDefinitionInterface& ident,
                    PluginSettings::ConfigurationType type,
                    const RegistryPath& group, const RegistryPath& key,
                    ConfigReference var, ConfigConstReference defval)
{
    auto& pluginManager = PluginManager::Get();
    const auto& id = pluginManager.GetID(&ident);
    if (pluginManager.GetConfigValue(type, id, group, key, var, defval)) {
        return true;
    }
    if (auto id2 = pluginManager.OldGetID(&ident); id != id2) {
        return pluginManager.GetConfigValue(type, id2, group, key, var, defval);
    }
    return false;
}

bool SetConfigValue(const EffectDefinitionInterface& ident,
                    PluginSettings::ConfigurationType type,
                    const RegistryPath& group, const RegistryPath& key,
                    ConfigConstReference value)
{
    auto& pluginManager = PluginManager::Get();
    const auto& id = pluginManager.GetID(&ident);
    return pluginManager.SetConfigValue(type, id, group, key, value);
}

bool RemoveConfigSubgroup(const EffectDefinitionInterface& ident,
                          PluginSettings::ConfigurationType type,
                          const RegistryPath& group)
{
    auto& pluginManager = PluginManager::Get();
    const auto& id = pluginManager.GetID(&ident);
    const auto& id2 = pluginManager.OldGetID(&ident);
    return pluginManager.RemoveConfigSubgroup(type, id, group)
           || (id2 != id && pluginManager.RemoveConfigSubgroup(type, id2, group));
}

bool RemoveConfig(const EffectDefinitionInterface& ident,
                  PluginSettings::ConfigurationType type,
                  const RegistryPath& group, const RegistryPath& key)
{
    auto& pluginManager = PluginManager::Get();
    const auto& id = pluginManager.GetID(&ident);
    const auto& id2 = pluginManager.OldGetID(&ident);
    return pluginManager.RemoveConfig(type, id, group, key)
           || (id2 != id && pluginManager.RemoveConfig(type, id2, group, key));
}
}
