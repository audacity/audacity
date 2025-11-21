#include "samplespainter.h"

#include <set>

#include "au3wrap/internal/domaccessor.h"
#include "Envelope.h"
#include "sampledata.h"
#include "samplespainterutils.h"
#include "wavepainterutils.h"
#include "WaveClip.h"
#include "PendingTracks.h"
#include "ZoomInfo.h"

constexpr auto SAMPLE_TICK_SIZE = 4;
constexpr auto SAMPLE_HEAD_PADDING = (SAMPLE_TICK_SIZE / 2) + 1;

using namespace au::au3;

namespace {
void drawSampleHead(const au::projectscene::SampleData& samples, const au::projectscene::WaveMetrics& metrics, QPainter& painter,
                    const au::projectscene::IWavePainter::Style& style, const bool showClipping)
{
    size_t slen = samples.size();
    const ZoomInfo zoomInfo{ metrics.fromTime, metrics.zoom };

    const auto selectedStartPosition
        = std::max(-10000, std::min(10000, static_cast<int>(zoomInfo.TimeToPosition(metrics.selectionStartTime))));
    const auto selectedEndPosition = std::max(-10000, std::min(10000, static_cast<int>(zoomInfo.TimeToPosition(metrics.selectionEndTime))));

    std::set<double> clippedXSet;
    if (showClipping && !samples.clippedX.empty()) {
        clippedXSet = std::set<double>(samples.clippedX.begin(), samples.clippedX.end());
    }

    QPen pen;
    QBrush brush;
    for (size_t s = 0; s < slen; s++) {
        if (samples.y[s] >= 0 && samples.y[s] < metrics.height) {
            if (showClipping && clippedXSet.count(samples.x[s]) > 0) {
                pen = style.clippedPen;
                brush = QBrush(style.clippedPen);
            } else {
                pen = style.sampleHead;
                brush = style.sampleBrush;
            }
            // selection with change the color of the pen (the outside of the head)
            if (selectedStartPosition <= samples.x[s] && samples.x[s] <= selectedEndPosition) {
                pen = style.sampleHeadSelection;
            }
            painter.setPen(pen);
            painter.setBrush(brush);

            const int centerX = static_cast<int>(std::round(metrics.left + samples.x[s]));
            const int centerY = static_cast<int>(std::round(metrics.top + samples.y[s]));

            const int left = centerX - SAMPLE_TICK_SIZE / 2;
            const int top = centerY - SAMPLE_TICK_SIZE / 2;

            painter.drawEllipse(left, top, SAMPLE_TICK_SIZE, SAMPLE_TICK_SIZE);
        }
    }
}

void drawSampleStalk(const au::projectscene::SampleData& samples, int yZero, const au::projectscene::WaveMetrics& metrics,
                     QPainter& painter, const au::projectscene::IWavePainter::Style& style, const bool showClipping)
{
    const size_t slen = samples.size();

    std::set<double> clippedXSet;
    if (showClipping && !samples.clippedX.empty()) {
        clippedXSet = std::set<double>(samples.clippedX.begin(), samples.clippedX.end());
    }

    for (size_t s = 0; s < slen; s++) {
        const int x = static_cast<int>(std::round(metrics.left + samples.x[s]));
        const int y1 = static_cast<int>(std::round(metrics.top + samples.y[s]));

        // Use clipped pen color for clipped samples, normal stalk color for others
        if (showClipping && clippedXSet.count(samples.x[s]) > 0) {
            painter.setPen(style.clippedPen);
        } else {
            painter.setPen(style.sampleStalk);
        }

        QPoint p1(x, y1);
        QPoint p2(x, yZero);
        painter.drawLine(p1, p2);
    }
}
}

namespace au::projectscene {
void SamplesPainter::paint(QPainter& painter, const trackedit::ClipKey& clipKey, const IWavePainter::Params& params)
{
    painter.save();
    painter.setRenderHint(QPainter::Antialiasing, false);

    au::au3::Au3Project* au3Project = reinterpret_cast<au::au3::Au3Project*>(globalContext()->currentProject()->au3ProjectPtr());

    WaveTrack* track = au::au3::DomAccessor::findWaveTrack(*au3Project, TrackId(clipKey.trackId));
    if (!track) {
        return;
    }

    std::shared_ptr<WaveClip> waveClip = au::au3::DomAccessor::findWaveClip(track, clipKey.itemId);
    if (!waveClip) {
        return;
    }

    const std::vector<double> channelHeight {
        params.geometry.height * params.channelHeightRatio,
        params.geometry.height * (1 - params.channelHeightRatio),
    };

    const float dBRange = std::abs(params.dbRange);
    const bool dB = !params.isLinear;
    const double trimLeft = waveClip->GetTrimLeft();

    auto waveMetrics = wavepainterutils::getWaveMetrics(globalContext()->currentProject(), clipKey, params, true);

    for (size_t index = 0; index < waveClip->NChannels(); index++) {
        waveMetrics.height = channelHeight[index];

        // Draw background with the full channel area
        samplespainterutils::drawBackground(painter, waveMetrics, params.style, trimLeft);

        // Create padded metrics for sample rendering with symmetric padding around separator
        auto paddedMetrics = waveMetrics;
        paddedMetrics.top += SAMPLE_HEAD_PADDING;
        paddedMetrics.height -= 2 * SAMPLE_HEAD_PADDING;
        int yZero = samplespainterutils::getWaveYPos(0.0, -params.verticalZoom, params.verticalZoom, paddedMetrics.height, dB,
                                                     true, dBRange, false);
        yZero = paddedMetrics.top + std::max(-1, std::min(static_cast<int>(paddedMetrics.height + paddedMetrics.top), yZero));
        const auto samples = samplespainterutils::getSampleData(*waveClip, index, paddedMetrics, dB, dBRange, params.verticalZoom,
                                                                -params.verticalZoom);
        if (samples.size() == 0) {
            samplespainterutils::drawCenterLine(painter, waveMetrics, params.style, yZero);
            continue;
        }
        drawSampleStalk(samples, yZero, paddedMetrics, painter, params.style, params.showClipping);

        // draw baseline after the sample stalk to ensure it's on top of it
        samplespainterutils::drawCenterLine(painter, waveMetrics, params.style, yZero);

        // drawSampleHead must be called after drawSampleStalk for the selection effect to work correctly
        drawSampleHead(samples, paddedMetrics, painter, params.style, params.showClipping);
        waveMetrics.top += waveMetrics.height;
    }
    painter.restore();
}
}
