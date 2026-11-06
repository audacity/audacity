/*
* Audacity: A Digital Audio Editor
*/
#include "audiomodule.h"

#include "internal/audioengineswitcher.h"
#include "internal/audiothreadsecurer.h"

namespace au::audio {
AudioModule::AudioModule()
    : m_audioThreadSecurer{std::make_shared<AudioThreadSecurer>()}, m_audioEngineSwitcher{std::make_shared<AudioEngineSwitcher>()}
{
}

std::string AudioModule::moduleName() const
{
    return "audio";
}

void AudioModule::registerExports()
{
    globalIoc()->registerExport<IAudioEngine>(moduleName(), m_audioEngineSwitcher);
    globalIoc()->registerExport<IAudioDriverController>(moduleName(), m_audioEngineSwitcher);
    globalIoc()->registerExport<IAudioBackendRegister>(moduleName(), m_audioEngineSwitcher);
    globalIoc()->registerExport<IAudioBackendSelector>(moduleName(), m_audioEngineSwitcher);

    // for muse
    globalIoc()->registerExport<muse::audio::IAudioThreadSecurer>(moduleName(), m_audioThreadSecurer);
}

void AudioModule::onInit(const muse::IApplication::RunMode&)
{
    m_audioThreadSecurer->setupMainThread();

    //! NOTE At the moment this is needed for VST plugins
    //! and everything is done in the main thread.
    m_audioThreadSecurer->setupAudioEngineThread();

    // The backends registered when the modules resolved their imports
    m_audioEngineSwitcher->init();
}
}
