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
    IF_ASSERT_FAILED(track) {
        return IWavePainter::PlotType::MinMaxRMS;
    }

    std::shared_ptr<WaveClip> waveClip = au::au3::DomAccessor::findWaveClip(track, clipKey.clipId);
    if (!waveClip) {
        return IWavePainter::PlotType::MinMaxRMS;
    }

    const double sampleRate = waveClip->GetRate();
    const double stretchRatio = waveClip->GetStretchRatio();

    const double rate = sampleRate / stretchRatio;

    const double threshold1 = PIXELS_PER_SAMPLE_WHEN_CONNECTING_POINTS * rate;
    if (zoom < threshold1) {
        return IWavePainter::PlotType::MinMaxRMS;
    }

    const double threshold2 = PIXELS_PER_SAMPLE_WHEN_INDIVIDUAL_POINTS * rate;
    if (zoom < threshold2) {
        return IWavePainter::PlotType::ConnectingDots;
    }

    return IWavePainter::PlotType::Stem;
}

WaveMetrics getWaveMetrics(std::shared_ptr<au::project::IAudacityProject> project, const trackedit::ClipKey& clipKey,
                           const IWavePainter::Params& params)
{
    WaveMetrics wm;

    au::au3::Au3Project* au3Project = reinterpret_cast<au::au3::Au3Project*>(project->au3ProjectPtr());
    WaveTrack* track = au::au3::DomAccessor::findWaveTrack(*au3Project, TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(track) {
        return wm;
    }

    std::shared_ptr<WaveClip> waveClip = au::au3::DomAccessor::findWaveClip(track, clipKey.clipId);
    IF_ASSERT_FAILED(waveClip) {
        return wm;
    }

    wm.zoom = params.zoom;
    wm.fromTime = params.fromTime;
    wm.toTime = params.toTime;
    wm.selectionStartTime = 0;
    wm.selectionEndTime = 0;
    wm.width = params.geometry.width;
    wm.left = params.geometry.left;

    // calculate selection area relative to the clip itself
    if (!muse::RealIsEqual(params.selectionStartTime, params.selectionEndTime)) {
        wm.selectionStartTime = params.selectionStartTime - waveClip->Start() + waveClip->GetTrimLeft();
        wm.selectionEndTime = params.selectionEndTime - waveClip->Start() + waveClip->GetTrimLeft();
    }

    return wm;
}
}
