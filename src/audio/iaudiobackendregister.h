/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <functional>
#include <memory>
#include <string>

#include "framework/global/modularity/imoduleinterface.h"

#include "driver/iaudiodrivercontroller.h"
#include "iaudioengine.h"

namespace au::audio {
//! An IAudioEngine implementation and the IAudioDriverController managing its devices.
//! Only one backend owns the audio devices at a time.
struct AudioBackend {
    std::string id;
    std::shared_ptr<IAudioEngine> engine;
    std::shared_ptr<IAudioDriverController> driverController;
    //! Called when it becomes the active backend; may be empty. Lets backends share devices, or an engine.
    std::function<void()> activated;
};

//! Where backend modules declare their audio backend.
//! The IAudioEngine and IAudioDriverController exports forward to the active backend.
class IAudioBackendRegister : MODULE_GLOBAL_INTERFACE
{
    INTERFACE_ID(IAudioBackendRegister)

public:
    virtual ~IAudioBackendRegister() = default;

    //! The first registered backend becomes the active one, unless the AU_AUDIO_BACKEND
    //! environment variable names another.
    virtual void registerBackend(const AudioBackend& backend) = 0;
};
}
