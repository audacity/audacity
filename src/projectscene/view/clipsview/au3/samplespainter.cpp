#include "samplespainter.h"

#include "au3wrap/internal/domaccessor.h"
#include "Envelope.h"
#include "sampledata.h"
#include "samplespainterutils.h"
#include "wavepainterutils.h"
#include "WaveClip.h"
#include "WaveformScale.h"
#include "WaveformSettings.h"
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
void SamplesPainter::paint(QPainter& painter, const trackedit::ClipKey& clipKey, const IWavePainter::Params& params)
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

    const std::vector<double> channelHeight {
        params.geometry.height * params.channelHeightRatio,
        params.geometry.height * (1 - params.channelHeightRatio),
    };

    float zoomMin, zoomMax;
    const auto& cache = WaveformScale::Get(*track);
    cache.GetDisplayBounds(zoomMin, zoomMax);

    const auto& settings = WaveformSettings::Get(*track);
    const float dBRange = settings.dBRange;
    const bool dB = !settings.isLinear();
    const double trimLeft = waveClip->GetTrimLeft();

    auto waveMetrics = wavepainterutils::getWaveMetrics(globalContext()->currentProject(), clipKey, params);

    for (size_t index = 0; index < waveClip->NChannels(); index++) {
        waveMetrics.height = channelHeight[index];
        samplespainterutils::drawBackground(painter, waveMetrics, params.style, trimLeft);
        samplespainterutils::drawBaseLine(painter, waveMetrics, params.style);
        const auto samples = samplespainterutils::getSampleData(*waveClip, index, waveMetrics, dB, dBRange, zoomMax, zoomMin);
        if (samples.size() == 0) {
            continue;
        }

        int yZero = samplespainterutils::getWaveYPos(0.0, zoomMin, zoomMax, waveMetrics.height, dB, true, dBRange, false);
        yZero = waveMetrics.top + std::max(-1, std::min(static_cast<int>(waveMetrics.height + waveMetrics.top), yZero));

        drawSampleHead(samples, waveMetrics, painter, params.style);
        drawSampleStalk(samples, yZero, waveMetrics, painter, params.style);
        waveMetrics.top += waveMetrics.height;
    }
}
}
