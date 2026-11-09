/*
* Audacity: A Digital Audio Editor
*/
#include "au3audiomodule.h"

#include "framework/global/log.h"
#include "framework/global/modularity/ioc.h"

#include "audio/driver/audiodrivercontroller.h"
#include "audio/iaudiobackendregister.h"

#include "portaudiodevices/iportaudiodeviceaccess.h"

#include "internal/au3audioengine.h"

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
}

void Au3AudioModule::resolveImports()
{
    const auto backendRegister = globalIoc()->resolve<audio::IAudioBackendRegister>(mname);
    const auto portAudio = globalIoc()->resolve<portaudiodevices::IPortAudioDeviceAccess>(mname);
    IF_ASSERT_FAILED(backendRegister && portAudio) {
        return;
    }
    const auto driverController = portAudio->driverController();
    driverController->setAudioEngine(m_audioEngine);
    // AudioIO shares the PortAudio devices with other engines: the driver controller follows the active one
    backendRegister->registerBackend({ "portaudio", m_audioEngine, driverController,
                                       [driverController] { return driverController->suspendStream(); }, "PortAudio (AudioIO)",
                                       [this, driverController] { driverController->setAudioEngine(m_audioEngine); } });
}

void Au3AudioModule::onInit(const muse::IApplication::RunMode&)
{
    m_audioEngine->init();
}

void Au3AudioModule::onDeinit()
{
    m_audioEngine->deinit();
}
