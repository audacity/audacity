/*
* Audacity: A Digital Audio Editor
*/
#include "auaudiomodule.h"

#include "internal/audiothreadsecurer.h"

namespace au::auaudio {
AuAudioModule::AuAudioModule()
    : m_audioThreadSecurer{std::make_shared<AudioThreadSecurer>()}
{
}

std::string AuAudioModule::moduleName() const
{
    return "auaudio";
}

void AuAudioModule::registerExports()
{
    // for muse
    ioc()->registerExport<muse::audio::IAudioThreadSecurer>(moduleName(), m_audioThreadSecurer);
}

void AuAudioModule::onInit(const muse::IApplication::RunMode&)
{
    m_audioThreadSecurer->setupMainThread();

    //! NOTE At the moment this is needed for VST plugins
    //! and everything is done in the main thread.
    m_audioThreadSecurer->setupAudioEngineThread();
}
}
