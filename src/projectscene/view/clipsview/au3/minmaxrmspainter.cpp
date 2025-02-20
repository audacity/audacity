#include "minmaxrmspainter.h"

#include "au3wrap/internal/domaccessor.h"
#include "wavepainterutils.h"
#include "WaveformScale.h"
#include "WaveformSettings.h"
#include "WaveformPainter.h"

namespace {
graphics::Color ColorFromQColor(const QColor& color)
{
    return graphics::Color(color.red(), color.green(), color.blue(), color.alpha());
}
}

namespace au::projectscene {
void MinMaxRMSPainter::paint(QPainter& painter, const trackedit::ClipKey& clipKey, const IWavePainter::Params& params)
{
    au::au3::Au3Project* au3Project = reinterpret_cast<au::au3::Au3Project*>(globalContext()->currentProject()->au3ProjectPtr());
    WaveTrack* track = au::au3::DomAccessor::findWaveTrack(*au3Project, TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(track) {
        return;
    }

    std::shared_ptr<WaveClip> waveClip = au::au3::DomAccessor::findWaveClip(track, clipKey.clipId);
    if (!waveClip) {
        return;
    }

    float zoomMin, zoomMax;
    auto& cache = WaveformScale::Get(*track);
    cache.GetDisplayBounds(zoomMin, zoomMax);

    auto& settings = WaveformSettings::Get(*track);
    const float dbRange = settings.dBRange;
    const bool dB = !settings.isLinear();

    const std::vector<double> channelHeight {
        params.geometry.height * params.channelHeightRatio,
        params.geometry.height * (1 - params.channelHeightRatio),
    };

    auto& waveformPainter = WaveformPainter::Get(*waveClip);
    WavePaintParameters paintParameters;

    auto metrics = wavepainterutils::getWaveMetrics(globalContext()->currentProject(), clipKey, params);

    for (size_t index = 0; index < waveClip->NChannels(); index++) {
        metrics.height = channelHeight[index];
        paintParameters
        .SetDisplayParameters(
            //TODO: uncomment and fix
            metrics.height, zoomMin, zoomMax, false /*artist->mShowClipping*/)
        .SetDBParameters(dbRange, dB)
        .SetBlankColor(ColorFromQColor(params.style.blankBrush))
        .SetSampleColors(
            ColorFromQColor(params.style.samplePen),
            ColorFromQColor(params.style.selectedSamplePen))
        .SetShowRMS(false)
        .SetRMSColors(
            ColorFromQColor(params.style.rmsPen),
            ColorFromQColor(params.style.rmsPen))
        .SetBackgroundColors(
            ColorFromQColor(params.style.normalBackground),
            ColorFromQColor(params.style.selectedBackground))
        .SetClippingColors(
            ColorFromQColor(params.style.clippedPen),
            ColorFromQColor(params.style.clippedPen))
        .SetEnvelope(waveClip->GetEnvelope());

        au::projectscene::WaveMetrics _metrics = metrics;
        _metrics.fromTime += waveClip->GetTrimLeft();
        _metrics.toTime += waveClip->GetTrimLeft();

        metrics.top += metrics.height;

        waveformPainter.Draw(index, painter, paintParameters, _metrics);
    }
}
}
