/*
* Audacity: A Digital Audio Editor
*/
#include "audiomodule.h"

#include "modularity/ioc.h"

#include "internal/audioengine.h"
#include "internal/audiothreadsecurer.h"

using namespace au::audio;
using namespace muse::modularity;

std::string AudioModule::moduleName() const
{
    return "audio";
}

void AudioModule::registerExports()
{
    m_audioEngine = std::make_shared<AudioEngine>();
    m_audioThreadSecurer = std::make_shared<AudioThreadSecurer>();

    ioc()->registerExport<IAudioEngine>(moduleName(), m_audioEngine);

    // for muse
    ioc()->registerExport<muse::audio::IAudioThreadSecurer>(moduleName(), m_audioThreadSecurer);
}

void AudioModule::resolveImports()
{
}

void AudioModule::registerResources()
{
}

void AudioModule::registerUiTypes()
{
}

void AudioModule::onInit(const muse::IApplication::RunMode&)
{
    m_audioEngine->init();
    m_audioThreadSecurer->setupMainThread();

    //! NOTE At the moment this is needed for VST plugins
    //! and everything is done in the main thread.
    m_audioThreadSecurer->setupWorkerThread();
}

void AudioModule::onDeinit()
{
}
