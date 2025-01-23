#include "au3samplespainter.h"
#include "Envelope.h"
#include "WaveClip.h"
#include "ZoomInfo.h"

constexpr auto SAMPLE_TICK_SIZE = 4;

namespace {

struct SampleData {
    QVector<int> y {};
    QVector<int> x {};

    SampleData() = default;

    SampleData(QVector<int> pY, QVector<int> pX)
    {
        IF_ASSERT_FAILED(pY.size() == pX.size()) {
            return;
        }

        y = std::move(pY);
        x = std::move(pX);
    }

    size_t size() const { return x.size(); }
};

/// Takes a value between min and max and returns a value between
/// height and 0
int GetWaveYPos(float value, float min, float max,
                int height, bool dB, bool outer,
                float dBr, bool clip)
{
    if (dB) {
        if (height == 0) {
            return 0;
        }

        float sign = (value >= 0 ? 1 : -1);

        if (value != 0.) {
            float db = LINEAR_TO_DB(fabs(value));
            value = (db + dBr) / dBr;
            if (!outer) {
                value -= 0.5;
            }
            if (value < 0.0) {
                value = 0.0;
            }
            value *= sign;
        }
    } else {
        if (!outer) {
            if (value >= 0.0) {
                value -= 0.5;
            } else {
                value += 0.5;
            }
        }
    }

    if (clip) {
        if (value < min) {
            value = min;
        }
        if (value > max) {
            value = max;
        }
    }

    value = (max - value) / (max - min);
    return (int)(value * (height - 1) + 0.5);
}

void DrawBaseLine(QPainter& painter, const au::projectscene::WaveMetrics& metrics, const Style& style)
{
    painter.setPen(style.centerLine);
    painter.drawLine(metrics.left, metrics.top + metrics.height / 2,
                     metrics.left + metrics.width, metrics.top + metrics.height / 2);
}

void DrawBackground(QPainter& painter, const au::projectscene::WaveMetrics& metrics, const Style& style, const double trimLeft)
{
    const ZoomInfo zoomInfo{metrics.fromTime, metrics.zoom};

    if (metrics.selectionStartTime == metrics.selectionEndTime) {
        painter.fillRect(metrics.left, metrics.top, metrics.width, metrics.height, style.normalBackground);
        return;
    }

    const auto relativeSelectionStartTime = metrics.selectionStartTime - trimLeft;
    const auto relativeSelectionEndTime = metrics.selectionEndTime - trimLeft;

    if (relativeSelectionEndTime < metrics.fromTime || relativeSelectionStartTime > metrics.toTime) {
        painter.fillRect(metrics.left, metrics.top, metrics.width, metrics.height, style.normalBackground);
    } else {
        const auto selectedStartPosition = std::max(zoomInfo.TimeToPosition(relativeSelectionStartTime), zoomInfo.TimeToPosition(metrics.fromTime));
        const auto selectedEndPosition = std::min(zoomInfo.TimeToPosition(relativeSelectionEndTime), zoomInfo.TimeToPosition(metrics.toTime));
 
        painter.fillRect(metrics.left, metrics.top, selectedStartPosition - metrics.left, metrics.height, style.normalBackground);
        painter.fillRect(selectedStartPosition, metrics.top, selectedEndPosition - selectedStartPosition, metrics.height, style.selectedBackground);
        painter.fillRect(selectedEndPosition, metrics.top, metrics.top + metrics.width, metrics.height, style.normalBackground);
    }
}

void DrawSampleHead(const SampleData& samples, const au::projectscene::WaveMetrics& metrics, QPainter& painter, const Style& style)
{
    const ZoomInfo zoomInfo{metrics.fromTime, metrics.zoom};
    size_t slen = samples.size();

    const auto selectedStartPosition = std::max(-10000, std::min(10000, (int)(zoomInfo.TimeToPosition(metrics.selectionStartTime))));
    const auto selectedEndPosition = std::max(-10000, std::min(10000, (int)(zoomInfo.TimeToPosition(metrics.selectionEndTime))));
    
    auto pr = QRect(0, 0, SAMPLE_TICK_SIZE, SAMPLE_TICK_SIZE);
    painter.setBrush(style.sampleBrush);

    for (decltype(slen) s = 0; s < slen; s++) {
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

void DrawSampleStalk(const SampleData& samples, bool dB, float dBRange, float zoomMax, float zoomMin, const au::projectscene::WaveMetrics& metrics, QPainter& painter, const Style& style)
{
    size_t slen = samples.size();
    int yZero = GetWaveYPos(0.0, zoomMin, zoomMax, metrics.height, dB, true, dBRange, false);
    
    painter.setPen(style.sampleStalk);
    yZero = metrics.left + std::max(-1, std::min(static_cast<int>(metrics.height), yZero));
    for (size_t s = 0; s < slen; s++) {
        painter.drawLine(
            metrics.left + samples.x[s], metrics.top + samples.y[s],
            metrics.left + samples.x[s], yZero);
    }
}

void DrawConnectingPoints(const SampleData& samples, const au::projectscene::WaveMetrics& metrics, QPainter& painter)
{
    size_t slen = samples.size();
    for (size_t s = 0; s < slen - 1; s++) {
        painter.drawLine(
            metrics.left + samples.x[s], metrics.top + samples.y[s],
            metrics.left + samples.x[s + 1], metrics.top + samples.y[s + 1]);
    }
}

SampleData GetSampleData(const Au3WaveClip& clip, int channelIndex, const au::projectscene::WaveMetrics& metrics, bool dB, float dBRange, float zoomMax, float zoomMin)
{
    const ZoomInfo zoomInfo{metrics.fromTime, metrics.zoom};
    double rate = clip.GetRate();
    const double t0 = metrics.fromTime;
    const auto s0 = sampleCount(floor(t0 * rate));
    const auto snSamples = clip.GetVisibleSampleCount();
    if (s0 > snSamples) {
        return SampleData();
    }

    const double t1 = metrics.toTime;
    const auto s1 = sampleCount(ceil(t1 * rate));

    // Assume size_t will not overflow, else we wouldn't be here drawing the
    // few individual samples
    auto slen = std::min(snSamples - s0, s1 - s0 + 1).as_size_t();

    if (slen <= 0) {
        SampleData();
    }

    Floats buffer{ size_t(slen) };
    clip.GetSamples(channelIndex, (samplePtr)buffer.get(), floatSample, s0, slen, false);

    auto xpos = QVector<int>(static_cast<int>(slen));
    auto ypos = QVector<int>(static_cast<int>(slen));

    for (decltype(slen) s = 0; s < slen; s++) {
        const double time = (s + s0).as_double() / rate;
        const int xx
            =std::max(-10000, std::min(10000,
                                       (int)(zoomInfo.TimeToPosition(time))));
        xpos[s] = xx;

        // Calculate sample as it would be rendered, so quantize time
        double value
            =clip.GetEnvelope().GetValue(time, 1.0 / clip.GetRate());
        const double tt = buffer[s] * value;

        ypos[s]
            =std::max(-1,
                      std::min(static_cast<int>(metrics.height),
                               GetWaveYPos(tt, zoomMin, zoomMax,
                                           metrics.height, dB, true, dBRange, false)));
    }

    return SampleData(ypos, xpos);
}
}

namespace au::projectscene {
void DrawIndividualSamples(int channelIndex, QPainter& painter,
                           const Style& style,
                           const WaveMetrics& metrics,
                           const Au3WaveClip& clip,
                           float zoomMin, float zoomMax,
                           bool dB, float dBRange,
                           bool showPoints)
{
    const double trimLeft = clip.GetTrimLeft();

    DrawBackground(painter, metrics, style, trimLeft);
    DrawBaseLine(painter, metrics, style);

    const auto samples = GetSampleData(clip, channelIndex, metrics, dB, dBRange, zoomMax, zoomMin);
    if (samples.size() == 0) {
        return;
    }

    if (showPoints) {
        DrawSampleHead(samples, metrics, painter, style);
        DrawSampleStalk(samples, dB, dBRange, zoomMax, zoomMin, metrics, painter, style);
    } else {
        DrawConnectingPoints(samples, metrics, painter);
    }
}
}
