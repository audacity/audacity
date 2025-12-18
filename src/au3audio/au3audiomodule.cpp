/*
* Audacity: A Digital Audio Editor
*/
#include "au3audiomodule.h"

#include "framework/global/modularity/ioc.h"

#include "internal/au3audioengine.h"
#include "internal/au3audiodevicesprovider.h"

using namespace au::au3audio;
using namespace muse::modularity;

std::string Au3AudioModule::moduleName() const
{
    return "au3audio";
}

void Au3AudioModule::registerExports()
{
    m_audioEngine = std::make_shared<Au3AudioEngine>();
    m_audioDevicesProvider = std::make_shared<Au3AudioDevicesProvider>();

    ioc()->registerExport<audio::IAudioEngine>(moduleName(), m_audioEngine);
    ioc()->registerExport<audio::IAudioDevicesProvider>(moduleName(), m_audioDevicesProvider);
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
    m_audioDevicesProvider->init();
}

void Au3AudioModule::onDeinit()
{
    m_audioEngine->deinit();
}
