/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "framework/global/modularity/imoduleinterface.h"

#include "audacityplugintypes.h"

namespace au::audacityplugin {
class IAudacityPluginHost : MODULE_GLOBAL_INTERFACE
{
    INTERFACE_ID(IAudacityPluginHost)

public:
    virtual ~IAudacityPluginHost() = default;

    virtual const std::vector<EffectDescriptor>& effects() const = 0;
    virtual std::vector<PluginPreferences> preferences() const = 0;
    virtual Status validatePreferences(const std::string& pluginId, const std::vector<Value>& values) = 0;
    virtual Status applyPreferences(const std::string& pluginId, const std::vector<Value>& values) = 0;
    virtual CreateInstanceResult createInstance(const std::string& pluginId, const std::string& effectId, const EffectCreateInfo& info) = 0;
};
} // namespace au::audacityplugin
