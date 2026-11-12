/*
 * Audacity: A Digital Audio Editor
 */
#include "jucedevicesmodule.h"

#include "framework/global/modularity/ioc.h"

#include "audio/driver/audiodrivercontroller.h"

#include "internal/juceaudiodevices.h"
#include "internal/jucedevice.h"

using namespace au::jucedevices;

namespace {
const std::string mname("jucedevices");

class JuceDriver final : public IJuceDeviceAccess
{
public:
    JuceDriver(std::shared_ptr<au::audio::AudioDriverController> driverController, std::shared_ptr<au::audio::IDeviceOpener> deviceOpener)
        : m_driverController{std::move(driverController)}, m_deviceOpener{std::move(deviceOpener)} {}

    std::shared_ptr<au::audio::AudioDriverController> driverController() const override { return m_driverController; }
    std::shared_ptr<au::audio::IDeviceOpener> deviceOpener() const override { return m_deviceOpener; }

private:
    const std::shared_ptr<au::audio::AudioDriverController> m_driverController;
    const std::shared_ptr<au::audio::IDeviceOpener> m_deviceOpener;
};
}

std::string JuceDevicesModule::moduleName() const
{
    return mname;
}

void JuceDevicesModule::registerExports()
{
    const auto devices = std::make_shared<JuceAudioDevices>();
    m_driverController = std::make_shared<audio::AudioDriverController>(devices, "JuceAudio");
    m_driver = std::make_shared<JuceDriver>(m_driverController, std::make_shared<JuceDeviceOpener>(devices));
    globalIoc()->registerExport<IJuceDeviceAccess>(mname, m_driver);
}

void JuceDevicesModule::onInit(const muse::IApplication::RunMode&)
{
    m_driverController->init();
}
