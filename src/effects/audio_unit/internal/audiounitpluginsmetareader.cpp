/*
* Audacity: A Digital Audio Editor
*/

#include "audiounitpluginsmetareader.h"

#include <regex>

#include "libraries/lib-audio-unit/AudioUnitEffectsModule.h"

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
    std::regex pattern("[A-Za-z0-9]{4}/[A-Za-z0-9]{4}/[A-Za-z0-9]{4}.*$");
    return std::regex_match(path.toStdString(), pattern);
}
