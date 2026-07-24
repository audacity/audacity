/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/global/async/notification.h"
#include "framework/global/modularity/imoduleinterface.h"

namespace au::au3audio {
//! NOTE Reports audio device changes on the OS side: a device was
//! plugged/unplugged or the system default device changed
class ISystemAudioDevicesListener : MODULE_GLOBAL_INTERFACE
{
    INTERFACE_ID(ISystemAudioDevicesListener)
public:
    virtual ~ISystemAudioDevicesListener() = default;

    virtual void startListening() = 0;
    virtual void stopListening() = 0;

    virtual muse::async::Notification systemDevicesChanged() const = 0;
};
}
