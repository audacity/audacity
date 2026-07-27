/*
* Audacity: A Digital Audio Editor
*/
#include "au3audiomodule.h"

#include "framework/global/modularity/ioc.h"

#include "internal/au3audioengine.h"
#include "internal/au3audiodrivercontroller.h"

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
