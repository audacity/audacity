/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "internal/ipeakfinderfactory.h"
#include "internal/ipeakfinder.h"

#include "context/iglobalcontext.h"

#include "framework/global/modularity/ioc.h"

namespace au::spectrogram {
class Au3PeakFinderFactory : public IPeakFinderFactory, public muse::Contextable
{
    muse::ContextInject<context::IGlobalContext> globalContext { this };

public:
    Au3PeakFinderFactory(const muse::modularity::ContextPtr& ctx);
    ~Au3PeakFinderFactory() override = default;
    std::unique_ptr<IPeakFinder> newInstance(int trackId, int channel, double startTime, double endTime) const override;
};
}
