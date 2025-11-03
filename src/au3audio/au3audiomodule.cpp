/*
* Audacity: A Digital Audio Editor
*/
#include "au3audiomodule.h"

#include "modularity/ioc.h"

#include "internal/au3audioengine.h"
#include "internal/audiothreadsecurer.h"

using namespace au::au3audio;
using namespace muse::modularity;

std::string Au3AudioModule::moduleName() const
{
    return "au3audio";
}

void Au3AudioModule::registerExports()
{
    m_audioEngine = std::make_shared<Au3AudioEngine>();
    m_audioThreadSecurer = std::make_shared<audio::AudioThreadSecurer>();

    ioc()->registerExport<audio::IAudioEngine>(moduleName(), m_audioEngine);

    // for muse
    ioc()->registerExport<muse::audio::IAudioThreadSecurer>(moduleName(), m_audioThreadSecurer);
}

void Au3AudioModule::resolveImports()
{
}

void Au3AudioModule::registerResources()
{
}

void Au3AudioModule::registerUiTypes()
{
}

void Au3AudioModule::onInit(const muse::IApplication::RunMode&)
{
    m_audioEngine->init();
    m_audioThreadSecurer->setupMainThread();

    //! NOTE At the moment this is needed for VST plugins
    //! and everything is done in the main thread.
    m_audioThreadSecurer->setupAudioEngineThread();
}

void Au3AudioModule::onDeinit()
{
}
