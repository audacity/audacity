#include "au3peakfinderfactory.h"
#include "au3peakfinder.h"

#include "au3wrap/internal/domaccessor.h"
#include "framework/global/log.h"

namespace au::spectrogram {
Au3PeakFinderFactory::Au3PeakFinderFactory(const muse::modularity::ContextPtr& ctx)
    : muse::Contextable(ctx)
{
}

std::unique_ptr<au::spectrogram::IPeakFinder> Au3PeakFinderFactory::newInstance(int trackId, int channel, double startTime,
                                                                                double endTime) const
{
    const auto prj = globalContext()->currentProject();
    // If config is not null, then so should prj.
    IF_ASSERT_FAILED(prj) {
        return nullptr;
    }

    ::WaveTrack* const waveTrack = au3::DomAccessor::findWaveTrack(*reinterpret_cast<::AudacityProject*>(prj->au3ProjectPtr()),
                                                                   ::TrackId { trackId });

    IF_ASSERT_FAILED(waveTrack) {
        return nullptr;
    }

    return std::make_unique<Au3PeakFinder>(*waveTrack, channel, startTime, endTime);
}
}
