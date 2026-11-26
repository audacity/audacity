#include "wavepainterutils.h"

#include "global/realfn.h"

#include "au3wrap/internal/domaccessor.h"

static constexpr auto PIXELS_PER_SAMPLE_WHEN_CONNECTING_POINTS = 0.5;
static constexpr auto PIXELS_PER_SAMPLE_WHEN_INDIVIDUAL_POINTS = 4;

namespace au::projectscene::wavepainterutils {
IWavePainter::PlotType getPlotType(std::shared_ptr<au::project::IAudacityProject> project, const trackedit::ClipKey& clipKey, double zoom)
{
    au::au3::Au3Project* au3Project = reinterpret_cast<au::au3::Au3Project*>(project->au3ProjectPtr());
    WaveTrack* track = au::au3::DomAccessor::findWaveTrack(*au3Project, TrackId(clipKey.trackId));
    if (!track) {
        return IWavePainter::PlotType::MinMaxRMS;
    }

    std::shared_ptr<WaveClip> waveClip = au::au3::DomAccessor::findWaveClip(track, clipKey.itemId);
    if (!waveClip) {
        return IWavePainter::PlotType::MinMaxRMS;
    }

    return getPlotType(waveClip->GetRate() / waveClip->GetStretchRatio(), zoom);
}

IWavePainter::PlotType getPlotType(double samplesPerSecond, double zoom)
{
    const double threshold1 = PIXELS_PER_SAMPLE_WHEN_CONNECTING_POINTS * samplesPerSecond;
    if (zoom < threshold1) {
        return IWavePainter::PlotType::MinMaxRMS;
    }

    const double threshold2 = PIXELS_PER_SAMPLE_WHEN_INDIVIDUAL_POINTS * samplesPerSecond;
    if (zoom < threshold2) {
        return IWavePainter::PlotType::ConnectingDots;
    }

    return IWavePainter::PlotType::Stem;
}

WaveMetrics getWaveMetrics(std::shared_ptr<au::project::IAudacityProject> project, const trackedit::ClipKey& clipKey,
                           const IWavePainter::Params& params, bool snapToSamples)
{
    au::au3::Au3Project* au3Project = reinterpret_cast<au::au3::Au3Project*>(project->au3ProjectPtr());
    WaveTrack* track = au::au3::DomAccessor::findWaveTrack(*au3Project, TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(track) {
        return {};
    }

    std::shared_ptr<WaveClip> waveClip = au::au3::DomAccessor::findWaveClip(track, clipKey.itemId);
    IF_ASSERT_FAILED(waveClip) {
        return {};
    }

    return getWaveMetrics(waveClip->Start(), waveClip->GetTrimLeft(), waveClip->GetRate() / waveClip->GetStretchRatio(), params,
                          snapToSamples);
}

WaveMetrics getWaveMetrics(double playStartTime, double trimLeft, double samplesPerSecond, const IWavePainter::Params& params,
                           bool snapToSamples)
{
    WaveMetrics wm;

    wm.zoom = params.zoom;
    wm.fromTime = params.fromTime;
    wm.toTime = params.toTime;
    wm.selectionStartTime = 0;
    wm.selectionEndTime = 0;
    wm.width = params.geometry.width;
    wm.left = params.geometry.left;

    // calculate selection area relative to the clip itself
    if (!muse::RealIsEqual(params.selectionStartTime, params.selectionEndTime)) {
        const double relativeStartTime = params.selectionStartTime - playStartTime;
        const double relativeEndTime = params.selectionEndTime - playStartTime;

        const auto snapToSampleTimeConv = [&](double time) {
            return std::round(time * samplesPerSecond) / samplesPerSecond;
        };

        wm.selectionStartTime = (snapToSamples ? snapToSampleTimeConv(relativeStartTime) : relativeStartTime) + trimLeft;
        wm.selectionEndTime = (snapToSamples ? snapToSampleTimeConv(relativeEndTime) : relativeEndTime) + trimLeft;
    }

    return wm;
}
}
