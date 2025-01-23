#include "au3samplespainter.h"
#include "Envelope.h"
#include "WaveClip.h"
#include "ZoomInfo.h"

constexpr auto SAMPLE_TICK_SIZE = 4;

namespace {
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

void DrawBaseLine(QPainter& painter, const QRect& rect, const Style& style)
{
    painter.setPen(style.centerLine);
    painter.drawLine(rect.x(), rect.y() + rect.height() / 2,
                     rect.x() + rect.width(), rect.y() + rect.height() / 2);
}

void DrawBackground(QPainter& painter, const QRect& rect, const au::projectscene::WaveMetrics& metrics, const Style& style, const double trimLeft)
{
    const ZoomInfo zoomInfo{metrics.fromTime, metrics.zoom};

    if (metrics.selectionStartTime == metrics.selectionEndTime) {
        painter.fillRect(rect.x(), rect.y(), rect.width(), rect.height(), style.normalBackground);
        return;
    }

    const auto relativeSelectionStartTime = metrics.selectionStartTime - trimLeft;
    const auto relativeSelectionEndTime = metrics.selectionEndTime - trimLeft;

    if (relativeSelectionEndTime < metrics.fromTime || relativeSelectionStartTime > metrics.toTime) {
        painter.fillRect(rect.x(), rect.y(), rect.width(), rect.height(), style.normalBackground);
    } else {
        const auto selectedStartPosition = std::max(zoomInfo.TimeToPosition(relativeSelectionStartTime), zoomInfo.TimeToPosition(metrics.fromTime));
        const auto selectedEndPosition = std::min(zoomInfo.TimeToPosition(relativeSelectionEndTime), zoomInfo.TimeToPosition(metrics.toTime));
 
        painter.fillRect(rect.x(), rect.y(), selectedStartPosition - rect.x(), rect.height(), style.normalBackground);
        painter.fillRect(selectedStartPosition, rect.y(), selectedEndPosition - selectedStartPosition, rect.height(), style.selectedBackground);
        painter.fillRect(selectedEndPosition, rect.y(), rect.x() + rect.width(), rect.height(), style.normalBackground);
    }
}

void DrawSampleHead(size_t slen, const ArrayOf<int>& sampleY, const ArrayOf<int>& sampleX, const au::projectscene::WaveMetrics& metrics, QPainter& painter, const Style& style) {
    const ZoomInfo zoomInfo{metrics.fromTime, metrics.zoom};
    
    const auto selectedStartPosition = std::max(-10000, std::min(10000, (int)(zoomInfo.TimeToPosition(metrics.selectionStartTime))));
    const auto selectedEndPosition = std::max(-10000, std::min(10000, (int)(zoomInfo.TimeToPosition(metrics.selectionEndTime))));
    
    auto pr = QRect(0, 0, SAMPLE_TICK_SIZE, SAMPLE_TICK_SIZE);
    painter.setBrush(style.sampleBrush);

    for (decltype(slen) s = 0; s < slen; s++) {
        if (sampleY[s] >= 0 && sampleY[s] < metrics.height) {
            if (selectedStartPosition <= sampleX[s] && sampleX[s] <= selectedEndPosition) {
                painter.setPen(style.sampleHeadSelection);
            } else {
                painter.setPen(style.sampleHead);
            }

            pr.moveLeft(metrics.left + sampleX[s] - SAMPLE_TICK_SIZE / 2);
            pr.moveTop(metrics.top + sampleY[s] - SAMPLE_TICK_SIZE / 2);

            painter.drawEllipse(pr);
        }
    }
}

void DrawSampleStalk(size_t slen, bool dB, float dBRange, float zoomMax, float zoomMin, const ArrayOf<int>& sampleY, const ArrayOf<int>& sampleX, const au::projectscene::WaveMetrics& metrics, QPainter& painter, const Style& style) {
    int yZero = GetWaveYPos(0.0, zoomMin, zoomMax, metrics.height, dB, true, dBRange, false);
    
    painter.setPen(style.sampleStalk);
    yZero = metrics.left + std::max(-1, std::min(static_cast<int>(metrics.height), yZero));
    for (decltype(slen) s = 0; s < slen; s++) {
        painter.drawLine(
            metrics.left + sampleX[s], metrics.top + sampleY[s],
            metrics.left + sampleX[s], yZero);
    }
}

void DrawConnectingPoints(size_t slen, const ArrayOf<int>& sampleY, const ArrayOf<int>& sampleX, const au::projectscene::WaveMetrics& metrics, QPainter& painter) {
    for (decltype(slen) s = 0; s < slen - 1; s++) {
        painter.drawLine(
            metrics.left + sampleX[s], metrics.top + sampleY[s],
            metrics.left + sampleX[s + 1], metrics.top + sampleY[s + 1]);
    }
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
    const QRect rect{static_cast<int>(metrics.left), static_cast<int>(metrics.top), static_cast<int>(metrics.width), static_cast<int>(metrics.height)};
    const ZoomInfo zoomInfo{metrics.fromTime, metrics.zoom};
    const double trimLeft = clip.GetTrimLeft();

    DrawBackground(painter, rect, metrics, style, trimLeft);
    DrawBaseLine(painter, rect, style);

    double rate = clip.GetRate();
    const double t0 = metrics.fromTime;
    const auto s0 = sampleCount(floor(t0 * rate));
    const auto snSamples = clip.GetVisibleSampleCount();
    if (s0 > snSamples) {
        return;
    }

    const double t1 = metrics.toTime;
    const auto s1 = sampleCount(ceil(t1 * rate));

    // Assume size_t will not overflow, else we wouldn't be here drawing the
    // few individual samples
    auto slen = std::min(snSamples - s0, s1 - s0 + 1).as_size_t();

    if (slen <= 0) {
        return;
    }

    Floats buffer{ size_t(slen) };
    clip.GetSamples(channelIndex, (samplePtr)buffer.get(), floatSample, s0, slen,
                    // Suppress exceptions in this drawing operation:
                    false);

    ArrayOf<int> xpos{ size_t(slen) };
    ArrayOf<int> ypos{ size_t(slen) };

    for (decltype(slen) s = 0; s < slen; s++) {
        const double time = (s + s0).as_double() / rate;
        const int xx   // An offset into the rectangle rect
            =std::max(-10000, std::min(10000,
                                       (int)(zoomInfo.TimeToPosition(time))));
        xpos[s] = xx;

        // Calculate sample as it would be rendered, so quantize time
        double value
            =clip.GetEnvelope().GetValue(time, 1.0 / clip.GetRate());
        const double tt = buffer[s] * value;

        ypos[s]
            =std::max(-1,
                      std::min(rect.height(),
                               GetWaveYPos(tt, zoomMin, zoomMax,
                                           rect.height(), dB, true, dBRange, false)));
    }

    if (showPoints) {
        DrawSampleHead(slen, ypos, xpos, metrics, painter, style);
        DrawSampleStalk(slen, dB, dBRange, zoomMax, zoomMin, ypos, xpos, metrics, painter, style);
    } else {
        DrawConnectingPoints(slen, ypos, xpos, metrics, painter);
    }
}
}
