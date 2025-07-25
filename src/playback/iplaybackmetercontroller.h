/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "async/notification.h"
#include "modularity/ioc.h"
#include "modularity/imoduleinterface.h"

#include <string>
#include <vector>

namespace au::playback {
class IPlaybackMeterController : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IPlaybackMeterController)

public:
    virtual ~IPlaybackMeterController() = default;

    virtual double stepToPosition(double sample) const = 0;
    virtual double sampleToPosition(double sample) const = 0;
    virtual double positionToSample(double position) const = 0;
    virtual std::string sampleToText(double sample) const = 0;

    virtual std::vector<double> fullSteps() const = 0;
    virtual std::vector<double> smallSteps() const = 0;

    virtual muse::async::Notification playbackMeterChanged() const = 0;
};
}
