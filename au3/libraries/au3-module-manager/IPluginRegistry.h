/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "au3-module-manager/PluginDescriptor.h"

#include <map>

typedef std::map<PluginID, PluginDescriptor> PluginMap;

class IPluginRegistry
{
public:
    virtual ~IPluginRegistry() = default;
    virtual void Load(PluginMap&) = 0;
    virtual void Save(const PluginMap&, bool overwrite) = 0;
};
