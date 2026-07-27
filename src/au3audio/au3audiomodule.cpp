/*
* Audacity: A Digital Audio Editor
*/
#include "au3audiomodule.h"

#include "framework/global/modularity/ioc.h"

#include "internal/au3audioengine.h"
#include "internal/au3audiodrivercontroller.h"
#include "internal/au3audiodevicesprovider.h"

using namespace au::au3audio;
using namespace muse::modularity;

static const std::string mname("au3audio");

std::string Au3AudioModule::moduleName() const
{
    return mname;
}

void Au3AudioModule::registerExports()
{
    m_audioEngine = std::make_shared<Au3AudioEngine>();
    m_audioDriverController = std::make_shared<Au3AudioDriverController>();

    globalIoc()->registerExport<audio::IAudioEngine>(mname, m_audioEngine);
    globalIoc()->registerExport<audio::IAudioDriverController>(mname, m_audioDriverController);
}

void Au3AudioModule::onInit(const muse::IApplication::RunMode&)
{
    m_audioEngine->init();
    m_audioDriverController->init();
}

void Au3AudioModule::onDeinit()
{
    m_audioEngine->deinit();
}

IContextSetup* Au3AudioModule::newContext(const muse::modularity::ContextPtr& ctx) const
{
    return new Au3AudioContext(ctx);
}

void Au3AudioContext::registerExports()
{
    m_audioDevicesProvider = std::make_shared<Au3AudioDevicesProvider>(iocContext());
    ioc()->registerExport<audio::IAudioDevicesProvider>(mname, m_audioDevicesProvider);
}

void Au3AudioContext::onInit(const muse::IApplication::RunMode&)
{
    m_audioDevicesProvider->init();
}

void Au3AudioContext::onDeinit()
{
}
