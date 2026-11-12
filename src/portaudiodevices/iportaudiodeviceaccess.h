/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <memory>

#include "framework/global/modularity/imoduleinterface.h"

namespace au::audio {
class AudioDriverController;
}

namespace au::portaudiodevices {
//! The PortAudio devices, which the audio engines opening them share
class IPortAudioDeviceAccess : MODULE_GLOBAL_INTERFACE
{
    INTERFACE_ID(IPortAudioDeviceAccess)

public:
    virtual ~IPortAudioDeviceAccess() = default;

    //! Of the PortAudio devices, with the settings of AudioIO
    virtual std::shared_ptr<audio::AudioDriverController> driverController() const = 0;
};
}
