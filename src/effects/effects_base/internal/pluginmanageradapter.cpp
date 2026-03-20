/*
* Audacity: A Digital Audio Editor
*/
#include "pluginmanageradapter.h"

#include "au3-module-manager/PluginManager.h"
#include "effects/effects_base/internal/effectconfigsettings.h"

using namespace au::effects;

PluginManagerAdapter::PluginManagerAdapter()
{
    m_pluginManager = std::make_unique<PluginManager>();
    PluginManager::SetInstance(m_pluginManager.get());
}

PluginManagerAdapter::~PluginManagerAdapter()
{
    terminate();
}

void PluginManagerAdapter::initialize()
{
    m_pluginManager->Initialize([](const FilePath& localFileName) {
        return std::make_unique<au::au3::EffectConfigSettings>(localFileName.ToStdString());
    });
}

void PluginManagerAdapter::terminate()
{
    if (!m_pluginManager) {
        return;
    }

    m_pluginManager->Terminate();
    PluginManager::SetInstance(nullptr);
    m_pluginManager.reset();
}
