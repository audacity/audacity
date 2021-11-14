/**********************************************************************

  Audacity: A Digital Audio Editor

  @file ConfigInterface.cpp

**********************************************************************/
#include "ConfigInterface.h"

#include "PluginManager.h"

namespace PluginSettings {

bool HasConfigGroup( EffectDefinitionInterface &ident,
   PluginSettings::ConfigurationType type,
   const RegistryPath & group)
{
   auto &pluginManager = PluginManager::Get();
   const auto &id = pluginManager.GetID(&ident);
   return pluginManager.HasConfigGroup(type, id, group);
}

bool GetConfigSubgroups( EffectDefinitionInterface &ident,
   PluginSettings::ConfigurationType type,
   const RegistryPath & group, RegistryPaths &subgroups)
{
   auto &pluginManager = PluginManager::Get();
   const auto &id = pluginManager.GetID(&ident);
   return pluginManager.GetConfigSubgroups(
      type, id, group, subgroups);
}

bool GetConfigValue( EffectDefinitionInterface &ident,
   PluginSettings::ConfigurationType type,
   const RegistryPath & group, const RegistryPath & key,
   ConfigReference var, ConfigConstReference defval)
{
   auto &pluginManager = PluginManager::Get();
   const auto &id = pluginManager.GetID(&ident);
   return pluginManager.GetConfigValue(type, id, group, key, var, defval);
}

bool SetConfigValue( EffectDefinitionInterface &ident,
   PluginSettings::ConfigurationType type,
   const RegistryPath & group, const RegistryPath & key,
   ConfigConstReference value)
{
   auto &pluginManager = PluginManager::Get();
   const auto &id = pluginManager.GetID(&ident);
   return pluginManager.SetConfigValue(type, id, group, key, value);
}

bool RemoveConfigSubgroup( EffectDefinitionInterface &ident,
      PluginSettings::ConfigurationType type,
   const RegistryPath & group)
{
   auto &pluginManager = PluginManager::Get();
   const auto &id = pluginManager.GetID(&ident);
   return pluginManager.RemoveConfigSubgroup(type, id, group);
}

bool RemoveConfig( EffectDefinitionInterface &ident,
   PluginSettings::ConfigurationType type,
   const RegistryPath & group, const RegistryPath & key)
{
   auto &pluginManager = PluginManager::Get();
   const auto &id = pluginManager.GetID(&ident);
   return pluginManager.RemoveConfig(type, id, group, key);
}

}
