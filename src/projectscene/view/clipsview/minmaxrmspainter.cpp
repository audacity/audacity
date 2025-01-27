#include "minmaxrmspainter.h"

#include "au3/WaveformScale.h"
#include "au3/WaveformSettings.h"
#include "au3/WaveformPainter.h"

namespace {
graphics::Color ColorFromQColor(const QColor& color)
{
    return graphics::Color(color.red(), color.green(), color.blue(), color.alpha());
}
}

namespace au::projectscene {
void MinMaxRMSPainter::paint(int channelIndex, QPainter& painter, const WaveMetrics& metrics, const Style& style, const au::au3::Au3WaveTrack& track, const au::au3::Au3WaveClip& clip)
{
    auto& waveformPainter = WaveformPainter::Get(clip);
    WavePaintParameters paintParameters;

    float zoomMin, zoomMax;
    auto& cache = WaveformScale::Get(track);
    cache.GetDisplayBounds(zoomMin, zoomMax);

    auto& settings = WaveformSettings::Get(track);
    const float dbRange = settings.dBRange;
    const bool dB = !settings.isLinear();

    paintParameters
    .SetDisplayParameters(
        //TODO: uncomment and fix
        metrics.height, zoomMin, zoomMax, false /*artist->mShowClipping*/)
    .SetDBParameters(dbRange, dB)
    .SetBlankColor(ColorFromQColor(style.blankBrush))
    .SetSampleColors(
        ColorFromQColor(style.samplePen),
        ColorFromQColor(style.selectedSamplePen))
    .SetShowRMS(false)
    .SetRMSColors(
        ColorFromQColor(style.rmsPen),
        ColorFromQColor(style.rmsPen))
    .SetBackgroundColors(
        ColorFromQColor(style.normalBackground),
        ColorFromQColor(style.selectedBackground))
    .SetClippingColors(
        ColorFromQColor(style.clippedPen),
        ColorFromQColor(style.clippedPen))
    .SetEnvelope(clip.GetEnvelope());

    au::projectscene::WaveMetrics _metrics = metrics;
    _metrics.fromTime += clip.GetTrimLeft();
    _metrics.toTime += clip.GetTrimLeft();

    waveformPainter.Draw(channelIndex, painter, paintParameters, _metrics);
}
}