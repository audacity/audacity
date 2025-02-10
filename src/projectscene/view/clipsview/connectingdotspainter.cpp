#include "connectingdotspainter.h"

#include "au3/WaveformSettings.h"
#include "au3/WaveformScale.h"
#include "samplespainterutils.h"
#include "WaveClip.h"
#include "WaveClipUtilities.h"
#include "ZoomInfo.h"

namespace {
void drawConnectingPoints(const au::projectscene::SampleData& samples, const au::projectscene::WaveMetrics& metrics, QPainter& painter,
                          const au::projectscene::IWavePainter::Style& style)
{
    painter.setPen(style.samplePen);

    const size_t slen = samples.size();
    for (size_t s = 0; s < slen - 1; s++) {
        painter.drawLine(
            metrics.left + samples.x[s], metrics.top + samples.y[s],
            metrics.left + samples.x[s + 1], metrics.top + samples.y[s + 1]);
    }
}
}

namespace au::projectscene {
void ConnectingDotsPainter::paint(int channelIndex, QPainter& painter, const WaveMetrics& metrics, const Style& style,
                                  const au::au3::Au3WaveTrack& track, const au::au3::Au3WaveClip& clip)
{
    float zoomMin, zoomMax;
    const auto& cache = WaveformScale::Get(track);
    cache.GetDisplayBounds(zoomMin, zoomMax);

    const auto& settings = WaveformSettings::Get(track);
    const float dBRange = settings.dBRange;
    const bool dB = !settings.isLinear();
    const double trimLeft = clip.GetTrimLeft();

    samplespainterutils::drawBackground(painter, metrics, style, trimLeft);
    samplespainterutils::drawBaseLine(painter, metrics, style);

    const auto samples = samplespainterutils::getSampleData(clip, channelIndex, metrics, dB, dBRange, zoomMax, zoomMin);
    if (samples.size() == 0) {
        return;
    }

    drawConnectingPoints(samples, metrics, painter, style);
}
}
