/*
 * Audacity: A Digital Audio Editor
 */
#include "au4audiomodule.h"

#include "framework/global/log.h"
#include "framework/global/modularity/ioc.h"

#include "audio/driver/audiodrivercontroller.h"
#include "audio/iaudiobackendregister.h"
#include "portaudiodevices/iportaudiodeviceaccess.h"

#include "internal/au4audioengine.h"

using namespace au::au4audio;
using namespace muse::modularity;

static const std::string mname("au4audio");

std::string Au4AudioModule::moduleName() const
{
    return mname;
}

void Au4AudioModule::resolveImports()
{
    const auto backendRegister = globalIoc()->resolve<audio::IAudioBackendRegister>(mname);
    const auto portAudio = globalIoc()->resolve<portaudiodevices::IPortAudioDeviceAccess>(mname);
    IF_ASSERT_FAILED(backendRegister && portAudio) {
        return;
    }
    // On AudioIO's devices and settings, sharing their driver controller with AudioIO: it follows the active engine
    const auto driverController = portAudio->driverController();
    m_portAudioEngine = std::make_shared<Au4AudioEngine>(portAudio->deviceOpener());
    m_portAudioEngine->setDriverController(driverController.get());
    const auto suspendStream = [driverController] { return driverController->suspendStream(); };
    backendRegister->registerBackend({ "new-portaudio", m_portAudioEngine, driverController, suspendStream,
                                       "New audio engine (PortAudio)", [this, driverController] {
            driverController->setAudioEngine(m_portAudioEngine);
        } });
}
