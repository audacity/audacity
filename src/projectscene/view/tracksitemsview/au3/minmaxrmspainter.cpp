#include "minmaxrmspainter.h"

#include "au3wrap/internal/domaccessor.h"
#include "wavepainterutils.h"
#include "WaveformPainter.h"

using namespace au::au3;

namespace {
graphics::Color ColorFromQColor(const QColor& color)
{
    return graphics::Color(color.red(), color.green(), color.blue(), color.alpha());
}

float getDBValue(float value, float dbRange)
{
    float sign = (value >= 0 ? 1 : -1);

    if (value != 0.) {
        float db = LINEAR_TO_DB(fabs(value));
        value = (db + dbRange) / dbRange;

        if (value < 0.0) {
            value = 0.0;
        }

        value *= sign;
    }

    return value;
}
}

namespace au::projectscene {
void MinMaxRMSPainter::paint(QPainter& painter, const trackedit::ClipKey& clipKey, const IWavePainter::Params& params)
{
    au::au3::Au3Project* au3Project = reinterpret_cast<au::au3::Au3Project*>(globalContext()->currentProject()->au3ProjectPtr());

    WaveTrack* track = au::au3::DomAccessor::findWaveTrack(*au3Project, TrackId(clipKey.trackId));
    if (!track) {
        return;
    }

    std::shared_ptr<WaveClip> waveClip = au::au3::DomAccessor::findWaveClip(track, clipKey.itemId);
    if (!waveClip) {
        return;
    }

    const float dbRange = std::abs(params.dbRange);
    const bool dB = !params.isLinear;

    const std::vector<double> channelHeight {
        params.geometry.height * params.channelHeightRatio,
        params.geometry.height * (1 - params.channelHeightRatio),
    };

    const float zoomMin = dB ? getDBValue(params.displayBounds.first, dbRange) : params.displayBounds.first;
    const float zoomMax = dB ? getDBValue(params.displayBounds.second, dbRange) : params.displayBounds.second;

    auto& waveformPainter = WaveformPainter::Get(*waveClip);
    WavePaintParameters paintParameters;

    auto metrics = wavepainterutils::getWaveMetrics(globalContext()->currentProject(), clipKey, params);

    for (size_t index = 0; index < waveClip->NChannels(); index++) {
        metrics.height = channelHeight[index];
        paintParameters
        .SetDisplayParameters(
            metrics.height, zoomMin, zoomMax, params.showClipping)
        .SetDBParameters(dbRange, dB)
        .SetBlankColor(ColorFromQColor(params.style.blankBrush))
        .SetSampleColors(
            ColorFromQColor(params.style.samplePen),
            ColorFromQColor(params.style.selectedSamplePen))
        .SetShowRMS(params.showRMS)
        .SetRMSColors(
            ColorFromQColor(params.style.rmsPen),
            ColorFromQColor(params.style.rmsSelectedPen))
        .SetBackgroundColors(
            ColorFromQColor(params.style.normalBackground),
            ColorFromQColor(params.style.selectedBackground))
        .SetClippingColors(
            ColorFromQColor(params.style.clippedPen),
            ColorFromQColor(params.style.clippedPen))
        .SetEnvelopeColors(
            ColorFromQColor(params.style.envelopeBackground),
            ColorFromQColor(params.style.selectedEnvelopeBackground))
        .SetEnvelope(waveClip->GetEnvelope());

        au::projectscene::WaveMetrics _metrics = metrics;
        _metrics.fromTime += waveClip->GetTrimLeft();
        _metrics.toTime += waveClip->GetTrimLeft();

        metrics.top += static_cast<int>(metrics.height);

        waveformPainter.Draw(index, painter, paintParameters, _metrics);
    }
}
}
