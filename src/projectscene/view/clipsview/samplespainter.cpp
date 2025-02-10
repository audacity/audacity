#include "samplespainter.h"

#include "au3/WaveformScale.h"
#include "au3/WaveformSettings.h"
#include "Envelope.h"
#include "sampledata.h"
#include "samplespainterutils.h"
#include "WaveClip.h"
#include "ZoomInfo.h"

constexpr auto SAMPLE_TICK_SIZE = 4;

namespace {
void drawSampleHead(const au::projectscene::SampleData& samples, const au::projectscene::WaveMetrics& metrics, QPainter& painter,
                    const au::projectscene::IWavePainter::Style& style)
{
    size_t slen = samples.size();
    const ZoomInfo zoomInfo{ metrics.fromTime, metrics.zoom };

    const auto selectedStartPosition
        = std::max(-10000, std::min(10000, static_cast<int>(zoomInfo.TimeToPosition(metrics.selectionStartTime))));
    const auto selectedEndPosition = std::max(-10000, std::min(10000, static_cast<int>(zoomInfo.TimeToPosition(metrics.selectionEndTime))));

    painter.setBrush(style.sampleBrush);

    auto pr = QRect(0, 0, SAMPLE_TICK_SIZE, SAMPLE_TICK_SIZE);
    for (size_t s = 0; s < slen; s++) {
        if (samples.y[s] >= 0 && samples.y[s] < metrics.height) {
            if (selectedStartPosition <= samples.x[s] && samples.x[s] <= selectedEndPosition) {
                painter.setPen(style.sampleHeadSelection);
            } else {
                painter.setPen(style.sampleHead);
            }

            pr.moveLeft(metrics.left + samples.x[s] - SAMPLE_TICK_SIZE / 2);
            pr.moveTop(metrics.top + samples.y[s] - SAMPLE_TICK_SIZE / 2);

            painter.drawEllipse(pr);
        }
    }
}

void drawSampleStalk(const au::projectscene::SampleData& samples, int yZero, const au::projectscene::WaveMetrics& metrics,
                     QPainter& painter, const au::projectscene::IWavePainter::Style& style)
{
    painter.setPen(style.sampleStalk);

    const size_t slen = samples.size();
    for (size_t s = 0; s < slen; s++) {
        painter.drawLine(
            metrics.left + samples.x[s], metrics.top + samples.y[s],
            metrics.left + samples.x[s], yZero);
    }
}
}

namespace au::projectscene {
void SamplesPainter::paint(int channelIndex, QPainter& painter, const WaveMetrics& metrics, const Style& style,
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

    int yZero = samplespainterutils::getWaveYPos(0.0, zoomMin, zoomMax, metrics.height, dB, true, dBRange, false);
    yZero = metrics.top + std::max(-1, std::min(static_cast<int>(metrics.height + metrics.top), yZero));

    drawSampleHead(samples, metrics, painter, style);
    drawSampleStalk(samples, yZero, metrics, painter, style);
}
}
