/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "ipeakfinder.h"

#include "framework/global/modularity/imoduleinterface.h"

namespace au::spectrogram {
class IPeakFinderFactory : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IPeakFinderFactory)
public:
    virtual ~IPeakFinderFactory() = default;

    virtual std::unique_ptr<IPeakFinder> newInstance(int trackId, int channel, double startTime, double endTime) const = 0;
};
}
