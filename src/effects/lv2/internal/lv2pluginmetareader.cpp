/*
 * Audacity: A Digital Audio Editor
 */
#include "lv2pluginmetareader.h"
#include "libraries/lib-module-manager/PluginManager.h"
#include "libraries/lib-module-manager/PluginManager.h"
#include "libraries/lib-strings/TranslatableString.h"

namespace au::effects {
Lv2PluginMetaReader::Lv2PluginMetaReader()
    : AbstractAudioPluginMetaReader{m_module} {}

muse::audio::AudioResourceType Lv2PluginMetaReader::metaType() const
{
    return muse::audio::AudioResourceType::Lv2Plugin;
}

void Lv2PluginMetaReader::doInit(const muse::IApplication::RunMode& mode)
{
    if (mode == muse::IApplication::RunMode::AudioPluginRegistration) {
        m_module.InitializePluginRegistration();
    } else {
        m_module.Initialize();
    }
}

bool Lv2PluginMetaReader::canReadMeta(const muse::io::path_t& path) const
{
    const wxString wxPluginPath{ path.toStdString() };
    const std::vector<wxString> paths = m_module.FindModulePaths(PluginManager::Get());
    return std::any_of(paths.begin(), paths.end(), [&](const wxString& modulePath) {
        return wxPluginPath == modulePath;
    });
}
}
