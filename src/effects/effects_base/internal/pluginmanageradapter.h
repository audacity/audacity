/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <memory>

#include "effects/effects_base/ipluginmanager.h"

class PluginManager;

namespace au::effects {
class PluginManagerAdapter : public IPluginManager
{
public:
    PluginManagerAdapter();
    ~PluginManagerAdapter() override;

    void initialize() override;
    void terminate() override;

private:
    std::unique_ptr<PluginManager> m_pluginManager;
};
}
