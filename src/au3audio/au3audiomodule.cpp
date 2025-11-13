/*
* Audacity: A Digital Audio Editor
*/
#include "au3audiomodule.h"

#include "framework/global/modularity/ioc.h"

#include "internal/au3audioengine.h"

using namespace au::au3audio;
using namespace muse::modularity;

std::string Au3AudioModule::moduleName() const
{
    return "au3audio";
}

void Au3AudioModule::registerExports()
{
    m_audioEngine = std::make_shared<Au3AudioEngine>();

    ioc()->registerExport<audio::IAudioEngine>(moduleName(), m_audioEngine);
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
}

void Au3AudioModule::onDeinit()
{
    m_audioEngine->deinit();
}
