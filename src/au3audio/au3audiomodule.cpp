/*
* Audacity: A Digital Audio Editor
*/
#include "au3audiomodule.h"

#include "framework/global/modularity/ioc.h"

#include "internal/au3audioengine.h"
#include "internal/au3audiodevicesprovider.h"
#include "internal/au3devicemanager.h"

#ifdef Q_OS_MAC
#include "internal/platform/macos/macossystemaudiodeviceslistener.h"
#else
#include "internal/platform/stub/stubsystemaudiodeviceslistener.h"
#endif

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
    m_deviceManager = std::make_shared<Au3DeviceManager>();

#ifdef Q_OS_MAC
    m_systemAudioDevicesListener = std::make_shared<MacosSystemAudioDevicesListener>();
#else
    m_systemAudioDevicesListener = std::make_shared<StubSystemAudioDevicesListener>();
#endif

    globalIoc()->registerExport<audio::IAudioEngine>(mname, m_audioEngine);
    globalIoc()->registerExport<IAu3DeviceManager>(mname, m_deviceManager);
    globalIoc()->registerExport<ISystemAudioDevicesListener>(mname, m_systemAudioDevicesListener);
}

void Au3AudioModule::onInit(const muse::IApplication::RunMode&)
{
    m_audioEngine->init();
    m_systemAudioDevicesListener->startListening();
}

void Au3AudioModule::onDeinit()
{
    m_systemAudioDevicesListener->stopListening();
    m_audioEngine->deinit();
}

IContextSetup* Au3AudioModule::newContext(const muse::modularity::ContextPtr& ctx) const
{
    return new Au3AudioContext(ctx);
}

// =====================================================
// Au3AudioContext
// =====================================================

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
