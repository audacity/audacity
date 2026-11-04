/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <chrono>

#include "framework/global/async/notification.h"
#include "framework/global/modularity/imoduleinterface.h"
#include "framework/global/types/ret.h"

namespace au::record {
class ITimedRecordingScenario : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(ITimedRecordingScenario)

public:
    enum class State {
        Idle,
        Waiting,
        Recording,
        Quitting
    };

    virtual ~ITimedRecordingScenario() = default;

    virtual muse::Ret openSetup() = 0;

    virtual State state() const = 0;
    virtual muse::async::Notification stateChanged() const = 0;

    virtual std::chrono::system_clock::time_point startTime() const = 0;
    virtual std::chrono::system_clock::time_point endTime() const = 0;
    virtual std::chrono::system_clock::time_point quitTime() const = 0;

    virtual void requestStop() = 0;
};
}
