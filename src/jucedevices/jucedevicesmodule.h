/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <memory>

#include "framework/global/modularity/imodulesetup.h"

#include "ijucedeviceaccess.h"

namespace au::jucedevices {
class JuceDevicesModule : public muse::modularity::IModuleSetup
{
public:
    std::string moduleName() const override;
    void registerExports() override;
    void onInit(const muse::IApplication::RunMode& mode) override;

private:
    std::shared_ptr<audio::AudioDriverController> m_driverController;
    std::shared_ptr<IJuceDeviceAccess> m_driver;
};
}
