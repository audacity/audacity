/*
 * Audacity: A Digital Audio Editor
 */
#include "lv2pluginmetareader.h"
#include "au3-module-manager/PluginManager.h"
#include "au3-module-manager/PluginManager.h"
#include "au3-strings/TranslatableString.h"

namespace au::effects {
Lv2PluginMetaReader::Lv2PluginMetaReader()
    : Au3AudioPluginMetaReader{m_module} {}

muse::audio::AudioResourceType Lv2PluginMetaReader::metaType() const
{
    return muse::audio::AudioResourceType::Lv2Plugin;
}

bool Lv2PluginMetaReader::canReadMeta(const muse::io::path_t& path) const
{
    const wxString wxPath{ path.c_str() };
    m_module.LoadBundle(wxPath);
    return m_module.CheckPluginExist(wxPath);
}
}
