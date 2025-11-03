/*
* Audacity: A Digital Audio Editor
*/
#include "audiomodule.h"

#include "internal/audiothreadsecurer.h"

namespace au::audio {
AudioModule::AudioModule()
    : m_audioThreadSecurer{std::make_shared<AudioThreadSecurer>()}
{
}

std::string AudioModule::moduleName() const
{
    return "audio";
}

void AudioModule::registerExports()
{
    // for muse
    ioc()->registerExport<muse::audio::IAudioThreadSecurer>(moduleName(), m_audioThreadSecurer);
}

void AudioModule::onInit(const muse::IApplication::RunMode&)
{
    m_audioThreadSecurer->setupMainThread();

    //! NOTE At the moment this is needed for VST plugins
    //! and everything is done in the main thread.
    m_audioThreadSecurer->setupAudioEngineThread();
}
}
