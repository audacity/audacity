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
#include "iaudiostreamsuspender.h"

namespace au::audio {
//! An IAudioEngine implementation and the IAudioDriverController managing its devices.
//! Only one backend owns the audio devices at a time.
struct AudioBackend {
    std::string id;
    std::shared_ptr<IAudioEngine> engine;
    std::shared_ptr<IAudioDriverController> driverController;
    //! Stops the backend's stream, if any, so that another backend can be activated; empty if it can't
    std::function<AudioStreamRestorer()> suspendStream;
    //! What the user sees it as; the id if empty
    std::string title;
};

//! Where backend modules declare their audio backend.
//! The IAudioEngine and IAudioDriverController exports forward to the active backend.
class IAudioBackendRegister : MODULE_GLOBAL_INTERFACE
{
    INTERFACE_ID(IAudioBackendRegister)

public:
    virtual ~IAudioBackendRegister() = default;

    //! The first registered backend is active, until the module's initialization activates the one chosen last, or the
    //! one the AU_AUDIO_BACKEND environment variable names, which overrides the choice for this run.
    virtual void registerBackend(const AudioBackend& backend) = 0;
};
}
