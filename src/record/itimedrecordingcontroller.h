/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <chrono>

#include "framework/global/async/notification.h"
#include "framework/global/modularity/imoduleinterface.h"

namespace au::record {
class ITimedRecordingController : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(ITimedRecordingController)

public:
    enum class State {
        Idle,
        Waiting,
        Recording,
        Quitting
    };

    virtual ~ITimedRecordingController() = default;

    virtual State state() const = 0;
    virtual muse::async::Notification stateChanged() const = 0;

    virtual std::chrono::system_clock::time_point startTime() const = 0;
    virtual std::chrono::system_clock::time_point endTime() const = 0;
    virtual std::chrono::system_clock::time_point quitTime() const = 0;

    virtual void requestStop() = 0;
};
}
