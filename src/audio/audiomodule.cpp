/*
* Audacity: A Digital Audio Editor
*/
#include "audiomodule.h"

#include "internal/audioengineswitcher.h"
#include "internal/audiothreadsecurer.h"

#if defined(Q_OS_MAC)
#include "internal/platform/macos/macossystemaudiodeviceslistener.h"
#elif defined(Q_OS_WIN)
#include "internal/platform/windows/windowssystemaudiodeviceslistener.h"
#elif defined(AU_AUDIO_HAS_UDEV)
#include "internal/platform/linux/linuxsystemaudiodeviceslistener.h"
#else
#include "internal/platform/stub/stubsystemaudiodeviceslistener.h"
#endif

namespace au::audio {
namespace {
std::shared_ptr<ISystemAudioDevicesListener> makeSystemAudioDevicesListener()
{
#if defined(Q_OS_MAC)
    return std::make_shared<MacosSystemAudioDevicesListener>();
#elif defined(Q_OS_WIN)
    return std::make_shared<WindowsSystemAudioDevicesListener>();
#elif defined(AU_AUDIO_HAS_UDEV)
    return std::make_shared<LinuxSystemAudioDevicesListener>();
#else
    return std::make_shared<StubSystemAudioDevicesListener>();
#endif
}
}

AudioModule::AudioModule()
    : m_audioThreadSecurer{std::make_shared<AudioThreadSecurer>()}, m_audioEngineSwitcher{std::make_shared<AudioEngineSwitcher>()},
    m_systemAudioDevicesListener{makeSystemAudioDevicesListener()}
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
    globalIoc()->registerExport<ISystemAudioDevicesListener>(moduleName(), m_systemAudioDevicesListener);

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

    m_systemAudioDevicesListener->startListening();
}

void AudioModule::onDeinit()
{
    m_systemAudioDevicesListener->stopListening();
}
}
