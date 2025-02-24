#include "samplespainterutils.h"

#include "au3wrap/internal/domaccessor.h"
#include "wavepainterutils.h"
#include "Envelope.h"
#include "WaveClip.h"
#include "WaveClipUtilities.h"
#include "WaveChannelUtilities.h"
#include "WaveTrack.h"
#include "WaveformScale.h"
#include "WaveformSettings.h"
#include "ZoomInfo.h"

static constexpr auto X_MIN_DISTANCE = 5;
static constexpr auto Y_MIN_DISTANCE = 5;
static constexpr auto BASELINE_HIT_AREA_SIZE = 20;

namespace au::projectscene::samplespainterutils {
float FromDB(float value, double dBRange)
{
    if (value == 0) {
        return 0;
    }

    double sign = (value >= 0 ? 1 : -1);
    return DB_TO_LINEAR((fabs(value) * dBRange) - dBRange) * sign;
}

float ValueOfPixel(int yy, int height, bool offset,
                   bool dB, double dBRange, float zoomMin, float zoomMax)
{
    float v
        =height == 1 ? (zoomMin + zoomMax) / 2
          : zoomMax - (yy / (float)(height - 1)) * (zoomMax - zoomMin);
    if (offset) {
        if (v > 0.0) {
            v += .5;
        } else {
            v -= .5;
        }
    }

    if (dB) {
        v = FromDB(v, dBRange);
    }

    return v;
}

int getWaveYPos(float value, float min, float max,
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

void drawBackground(QPainter& painter, const au::projectscene::WaveMetrics& metrics, const IWavePainter::Style& style,
                    const double trimLeft)
{
    const ZoomInfo zoomInfo{ metrics.fromTime, metrics.zoom };

    // If there is no selection, just draw the normal background
    if (metrics.selectionStartTime == metrics.selectionEndTime) {
        painter.fillRect(metrics.left, metrics.top, metrics.width, metrics.height, style.normalBackground);
        return;
    }

    const auto relativeSelectionStartTime = metrics.selectionStartTime - trimLeft;
    const auto relativeSelectionEndTime = metrics.selectionEndTime - trimLeft;

    if (relativeSelectionEndTime < metrics.fromTime || relativeSelectionStartTime > metrics.toTime) {
        painter.fillRect(metrics.left, metrics.top, metrics.width, metrics.height, style.normalBackground);
    } else {
        const auto selectedStartPosition
            = std::max(zoomInfo.TimeToPosition(relativeSelectionStartTime), zoomInfo.TimeToPosition(metrics.fromTime));
        const auto selectedEndPosition
            = std::min(zoomInfo.TimeToPosition(relativeSelectionEndTime), zoomInfo.TimeToPosition(metrics.toTime));

        painter.fillRect(metrics.left, metrics.top, selectedStartPosition - metrics.left, metrics.height, style.normalBackground);
        painter.fillRect(selectedStartPosition, metrics.top, selectedEndPosition - selectedStartPosition, metrics.height,
                         style.selectedBackground);
        painter.fillRect(selectedEndPosition, metrics.top, metrics.top + metrics.width, metrics.height, style.normalBackground);
    }
}

void drawBaseLine(QPainter& painter, const au::projectscene::WaveMetrics& metrics, const IWavePainter::Style& style)
{
    painter.setPen(style.centerLine);
    painter.drawLine(metrics.left, metrics.top + metrics.height / 2,
                     metrics.left + metrics.width, metrics.top + metrics.height / 2);
}

SampleData getSampleData(const au::au3::Au3WaveClip& clip, int channelIndex, const au::projectscene::WaveMetrics& metrics,
                         bool dB, float dBRange, float zoomMax, float zoomMin)
{
    const ZoomInfo zoomInfo{ metrics.fromTime, metrics.zoom };
    double rate = clip.GetRate() / clip.GetStretchRatio();
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
    const auto slen = std::min(snSamples - s0, s1 - s0 + 1).as_size_t();
    if (slen <= 0) {
        SampleData();
    }

    Floats buffer{ slen };
    clip.GetSamples(channelIndex, (samplePtr)buffer.get(), floatSample, s0, slen, false);

    auto xpos = std::vector<int>(slen);
    auto ypos = std::vector<int>(slen);
    const auto invRate = 1.0 / rate;

    for (size_t s = 0; s < slen; s++) {
        const double time = (s + s0).as_double() / rate;
        const int xx = std::max(-10000, std::min(10000, static_cast<int>(zoomInfo.TimeToPosition(time))));
        xpos[s] = xx;

        const double value = clip.GetEnvelope().GetValue(time, invRate);
        const double tt = buffer[s] * value;

        ypos[s] = std::max(-1, std::min(static_cast<int>(metrics.height),
                                        samplespainterutils::getWaveYPos(tt, zoomMin, zoomMax, metrics.height, dB, true, dBRange, false)));
    }

    return SampleData(ypos, xpos);
}

std::optional<int> isNearSample(std::shared_ptr<au::project::IAudacityProject> project, const trackedit::ClipKey& clipKey,
                                const QPoint& position, const IWavePainter::Params& params)
{
    au::au3::Au3Project* au3Project = reinterpret_cast<au::au3::Au3Project*>(project->au3ProjectPtr());
    WaveTrack* track = au::au3::DomAccessor::findWaveTrack(*au3Project, TrackId(clipKey.trackId));
    if (!track) {
        return std::nullopt;
    }

    std::shared_ptr<WaveClip> waveClip = au::au3::DomAccessor::findWaveClip(track, clipKey.clipId);
    if (!waveClip) {
        return std::nullopt;
    }

    auto waveMetrics = wavepainterutils::getWaveMetrics(project, clipKey, params);

    const ZoomInfo zoomInfo { waveMetrics.fromTime, waveMetrics.zoom };

    const auto time = zoomInfo.PositionToTime(position.x());
    const auto sampleOffset = waveClip->TimeToSamples(time);
    const auto adjustedTime = waveClip->SamplesToTime(sampleOffset);
    const auto adjustedPosition = zoomInfo.TimeToPosition(adjustedTime);

    auto& settings = WaveformSettings::Get(*track);
    const float dBRange = settings.dBRange;
    const bool dB = !settings.isLinear();

    float zoomMin, zoomMax;
    auto& cache = WaveformScale::Get(*track);
    cache.GetDisplayBounds(zoomMin, zoomMax);

    const std::vector<double> channelHeight {
        params.geometry.height * params.channelHeightRatio,
        params.geometry.height * (1 - params.channelHeightRatio),
    };

    std::map<int, int> channelSampleDistance;
    int top = 0;
    for (size_t i = 0; i < waveClip->NChannels(); i++) {
        waveMetrics.height = channelHeight[i];

        float oneSample;
        if (!WaveClipUtilities::GetFloatAtTime(*waveClip, adjustedTime, i, oneSample, false)) {
            continue;
        }

        const double value = waveClip->GetEnvelope().GetValue(adjustedTime, (1 / waveClip->GetRate()));
        const auto tt = oneSample * value;

        const auto adjustedYPos = position.y() - top;

        const auto baselineYPos = getWaveYPos(0.0, zoomMin, zoomMax, waveMetrics.height, dB, true, dBRange, false);
        if (std::abs(adjustedYPos - baselineYPos) <= BASELINE_HIT_AREA_SIZE) {
            return i;
        }

        // It not on the baseline hit area check if it is closer enough to x position
        if (std::abs(adjustedPosition - position.x()) > X_MIN_DISTANCE) {
            continue;
        }

        const auto ypos = getWaveYPos(tt, zoomMin, zoomMax, waveMetrics.height, dB, true, dBRange, false);

        if (position.y() < top || position.y() > top + waveMetrics.height) {
            // Check bounderies
            top += static_cast<int>(waveMetrics.height);
            continue;
        }

        // The first and second condition are for the case when the mouse is between the baseline and the sample
        // The third condition is for the case where the mouse is really close to the sample
        if ((ypos > baselineYPos) && (adjustedYPos > baselineYPos) && (adjustedYPos < ypos)) {
            channelSampleDistance[i] = std::abs(baselineYPos - ypos);
        } else if ((ypos < baselineYPos) && (adjustedYPos < baselineYPos) && (adjustedYPos > ypos)) {
            channelSampleDistance[i] = std::abs(baselineYPos - ypos);
        } else if (std::abs(adjustedYPos - ypos) <= Y_MIN_DISTANCE) {
            channelSampleDistance[i] = std::abs(baselineYPos - ypos);
        }
        top += static_cast<int>(waveMetrics.height);
        waveMetrics.top += waveMetrics.height;
    }

    if (channelSampleDistance.size() == 0) {
        return std::nullopt;
    }

    // Find the closest channel to the clicked position
    auto it = std::min_element(channelSampleDistance.begin(), channelSampleDistance.end(), [](const auto& a, const auto& b) {
        return a.second < b.second;
    });

    return (*it).first;
}

void interpolatePoints(std::vector<QPoint>& container, const QPoint& previousPosition, const QPoint& finalPosition)
{
    // We do a simple linear interpolation if the move more than 1 pixel to avoid missing point due mouse fast movement
    if (std::abs(previousPosition.x() - finalPosition.x()) > 1) {
        const auto xdiff = std::abs(finalPosition.x() - previousPosition.x());
        const auto ydiff = finalPosition.y() - previousPosition.y();
        const auto rate = static_cast<double>(ydiff) / xdiff;

        auto cnt = 0;
        if (previousPosition.x() < finalPosition.x()) {
            for (auto i = previousPosition.x(); i <= finalPosition.x(); i++) {
                container.push_back(QPoint(i, previousPosition.y() + (rate * cnt)));
                cnt++;
            }
        } else {
            for (auto i = finalPosition.x(); i <= previousPosition.x(); i++) {
                container.push_back(QPoint(i, finalPosition.y() + (-rate * cnt)));
                cnt++;
            }
        }
    }
}

void setLastClickPos(const unsigned int currentChannel, std::shared_ptr<au::project::IAudacityProject> project,
                     const trackedit::ClipKey& clipKey, const QPoint& lastPosition, const QPoint& currentPosition,
                     const IWavePainter::Params& params)
{
    au::au3::Au3Project* au3Project = reinterpret_cast<au::au3::Au3Project*>(project->au3ProjectPtr());
    WaveTrack* track = au::au3::DomAccessor::findWaveTrack(*au3Project, TrackId(clipKey.trackId));
    if (!track) {
        return;
    }

    std::shared_ptr<WaveClip> waveClip = au::au3::DomAccessor::findWaveClip(track, clipKey.clipId);
    if (!waveClip) {
        return;
    }

    const auto waveChannels = track->Channels();
    if (currentChannel >= waveChannels.size()) {
        return;
    }

    auto it = waveChannels.begin();
    std::advance(it, currentChannel);
    const auto waveChannel = *it;

    std::vector<QPoint> points;
    if (std::abs(lastPosition.x() - currentPosition.x()) > 1) {
        points.reserve(std::abs(lastPosition.x() - currentPosition.x()));
        interpolatePoints(points, lastPosition, currentPosition);
    } else {
        points.push_back(currentPosition);
    }

    const std::vector<double> channelHeight {
        params.geometry.height * params.channelHeightRatio,
        params.geometry.height * (1 - params.channelHeightRatio),
    };

    auto& settings = WaveformSettings::Get(*track);
    const float dBRange = settings.dBRange;
    const bool dB = !settings.isLinear();

    float zoomMin, zoomMax;
    auto& cache = WaveformScale::Get(*track);
    cache.GetDisplayBounds(zoomMin, zoomMax);

    auto waveMetrics = wavepainterutils::getWaveMetrics(project, clipKey, params);
    waveMetrics.height = channelHeight[currentChannel];

    const ZoomInfo zoomInfo { waveMetrics.fromTime, waveMetrics.zoom };

    const auto startTime = zoomInfo.PositionToTime(points[0].x());

    double lastAdjustedTime = 0;
    std::vector<float> samples;
    samples.reserve(points.size());
    for (const auto& point : points) {
        const auto time = zoomInfo.PositionToTime(point.x());
        const auto clip = WaveChannelUtilities::GetClipAtTime(*waveChannel, time + waveClip->GetPlayStartTime());
        if (!clip) {
            continue;
        }

        const auto sampleOffset = clip->TimeToSamples(time);
        const auto adjustedTime = clip->SamplesToTime(sampleOffset);

        if (adjustedTime == lastAdjustedTime) {
            continue;
        }
        lastAdjustedTime = adjustedTime;

        float oneSample;
        if (!WaveClipUtilities::GetFloatAtTime(clip->GetClip(), adjustedTime, currentChannel, oneSample, false)) {
            continue;
        }

        const auto y = std::min(
            static_cast<int>(point.y() - (currentChannel * waveMetrics.height)), static_cast<int>(waveMetrics.height - 2));
        const auto yy = std::max(y, 2);

        float newValue = samplespainterutils::ValueOfPixel(yy, waveMetrics.height, false, dB, dBRange, zoomMin, zoomMax);
        samples.push_back(newValue);
    }
    WaveChannelUtilities::SetFloatsFromTime(*waveChannel, startTime + waveClip->GetPlayStartTime(), samples.data(),
                                            samples.size(), narrowestSampleFormat,
                                            PlaybackDirection::forward);
}
}
