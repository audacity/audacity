/*
* Audacity: A Digital Audio Editor
*/
#include "audiomodule.h"

#include "modularity/ioc.h"

#include "internal/audioengine.h"

using namespace au::audio;
using namespace muse::modularity;

std::string AudioModule::moduleName() const
{
    return "audio";
}

void AudioModule::registerExports()
{
    m_audioEngine = std::make_shared<AudioEngine>();

    ioc()->registerExport(moduleName(), m_audioEngine);
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
}

void AudioModule::onDeinit()
{
}
