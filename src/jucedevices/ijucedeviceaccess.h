/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <memory>

#include "framework/global/modularity/imoduleinterface.h"

namespace au::audio {
class AudioDriverController;
class IDeviceOpener;
}

namespace au::jucedevices {
//! The JUCE devices, with settings of their own
class IJuceDeviceAccess : MODULE_GLOBAL_INTERFACE
{
    INTERFACE_ID(IJuceDeviceAccess)

public:
    virtual ~IJuceDeviceAccess() = default;

    //! Of the JUCE devices, with the "JuceAudio" settings
    virtual std::shared_ptr<audio::AudioDriverController> driverController() const = 0;
    //! Opens JUCE devices as the configuration says
    virtual std::shared_ptr<audio::IDeviceOpener> deviceOpener() const = 0;
};
}
