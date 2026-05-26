/*
* Audacity: A Digital Audio Editor
*/

#include "audiounitpluginsmetareader.h"

#include <regex>

#include "au3-audio-unit/AudioUnitEffectsModule.h"

#include "audiounittypes.h"

au::effects::AudioUnitPluginsMetaReader::AudioUnitPluginsMetaReader()
    : Au3AudioPluginMetaReader(m_module)
{
}

muse::audioplugins::PluginType au::effects::AudioUnitPluginsMetaReader::metaType() const
{
    return std::string(au::effects::audio_unit::AUDIO_RESOURCE_TYPE_NAME);
}

bool au::effects::AudioUnitPluginsMetaReader::canReadMeta(const muse::io::path_t& path) const
{
    // AudioUnit path: "type/subtype/manufacturer/name"
    // each field is FourCharCode
    // https://developer.apple.com/documentation/audiotoolbox/audiocomponentdescription
    std::regex pattern("[^/]{4}/[^/]{4}/[^/]{4}/.+$");
    return std::regex_match(path.toStdString(), pattern);
}
