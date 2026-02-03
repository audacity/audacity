/*
* Audacity: A Digital Audio Editor
*/

#include "audiounitpluginsmetareader.h"

#include <regex>

#include "au3-audio-unit/AudioUnitEffectsModule.h"

using namespace muse::audio;

au::effects::AudioUnitPluginsMetaReader::AudioUnitPluginsMetaReader()
    : AbstractAudioPluginMetaReader(m_module)
{
}

AudioResourceType au::effects::AudioUnitPluginsMetaReader::metaType() const
{
    return AudioResourceType::AudioUnit;
}

bool au::effects::AudioUnitPluginsMetaReader::canReadMeta(const muse::io::path_t& path) const
{
    // AudioUnit path: "type/subtype/manufacturer/name"
    // each field is FourCharCode
    // https://developer.apple.com/documentation/audiotoolbox/audiocomponentdescription
    std::regex pattern("[^/]{4}/[^/]{4}/[^/]{4}/.+$");
    return std::regex_match(path.toStdString(), pattern);
}
