/*
 * Audacity: A Digital Audio Editor
 */
#include "portaudiodevicesmodule.h"

#include "framework/global/modularity/ioc.h"

#include "audio/driver/audiodrivercontroller.h"

#include "au3-audio-devices/DeviceManager.h"

#include "internal/portaudiodevice.h"
#include "internal/portaudiodevices.h"

using namespace au::portaudiodevices;

namespace {
const std::string mname("portaudiodevices");

class PortAudioDriver final : public IPortAudioDeviceAccess
{
public:
    explicit PortAudioDriver(std::shared_ptr<au::audio::AudioDriverController> driverController)
        : m_driverController{std::move(driverController)} {}

    std::shared_ptr<au::audio::AudioDriverController> driverController() const override { return m_driverController; }
    std::shared_ptr<au::audio::IDeviceOpener> deviceOpener() const override { return m_deviceOpener; }

private:
    const std::shared_ptr<au::audio::AudioDriverController> m_driverController;
    const std::shared_ptr<au::audio::IDeviceOpener> m_deviceOpener = std::make_shared<PortAudioDeviceOpener>();
};
}

std::string PortAudioDevicesModule::moduleName() const
{
    return mname;
}

void PortAudioDevicesModule::registerExports()
{
    m_driverController = std::make_shared<audio::AudioDriverController>(std::make_unique<PortAudioDevices>(), "AudioIO");
    m_driver = std::make_shared<PortAudioDriver>(m_driverController);
    globalIoc()->registerExport<IPortAudioDeviceAccess>(mname, m_driver);
}

void PortAudioDevicesModule::onInit(const muse::IApplication::RunMode&)
{
    audio::AudioDriverController::initSharedDefaults(DeviceManager::GetOptimalSupportedSampleRate());
    m_driverController->init();
}
