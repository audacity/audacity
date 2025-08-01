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

namespace {
static constexpr int SMOOTHING_KERNEL_RADIUS = 3;
static constexpr int SMOOTHING_BRUSH_RADIUS = 5;
static constexpr double SMOOTHING_PROPORTION_MAX = 0.7;
static constexpr double SMOOTHING_PROPORTION_MIN = 0.0;
void applySmoothingKernel(const Floats& sampleRegion, Floats& newSampleRegion, const std::pair<int, int> sampleRegionRange)
{
    for (auto jj = -SMOOTHING_BRUSH_RADIUS; jj <= SMOOTHING_BRUSH_RADIUS; ++jj) {
        float sumOfSamples = 0;
        for (auto ii = -SMOOTHING_KERNEL_RADIUS; ii <= SMOOTHING_KERNEL_RADIUS; ++ii) {
            const auto sampleRegionIndex
                =ii + jj + SMOOTHING_KERNEL_RADIUS + SMOOTHING_BRUSH_RADIUS;
            const auto inRange = sampleRegionRange.first <= sampleRegionIndex
                                 && sampleRegionIndex < sampleRegionRange.second;
            if (!inRange) {
                continue;
            }
            //The average is a weighted average, scaled by a weighting kernel that is simply triangular
            // A triangular kernel across N items, with a radius of R ( 2 R + 1 points), if the farthest:
            // points have a probability of a, the entire triangle has total probability of (R + 1)^2.
            // For sample number ii and middle brush sample M,  (R + 1 - abs(M-ii))/ ((R+1)^2) gives a
            // legal distribution whose total probability is 1.
            sumOfSamples += (SMOOTHING_KERNEL_RADIUS + 1 - abs(ii))
                            * sampleRegion[sampleRegionIndex];
        }
        newSampleRegion[jj + SMOOTHING_BRUSH_RADIUS]
            =sumOfSamples
              / ((SMOOTHING_KERNEL_RADIUS + 1) * (SMOOTHING_KERNEL_RADIUS + 1));
    }
}

void mixSmoothingSamples(const Floats& sampleRegion, Floats& newSampleRegion)
{
    // Now that the NEW sample levels are determined, go through each and mix it appropriately
    // with the original point, according to a 2-part linear function whose center has probability
    // SMOOTHING_PROPORTION_MAX and extends out SMOOTHING_BRUSH_RADIUS, at which the probability is
    // SMOOTHING_PROPORTION_MIN.  _MIN and _MAX specify how much of the smoothed curve make it through.
    float prob;
    for (auto jj = -SMOOTHING_BRUSH_RADIUS; jj <= SMOOTHING_BRUSH_RADIUS; ++jj) {
        prob
            =SMOOTHING_PROPORTION_MAX
              - static_cast<float>(abs(jj)) / SMOOTHING_BRUSH_RADIUS
              * (SMOOTHING_PROPORTION_MAX - SMOOTHING_PROPORTION_MIN);

        newSampleRegion[jj + SMOOTHING_BRUSH_RADIUS]
            =newSampleRegion[jj + SMOOTHING_BRUSH_RADIUS] * prob
              + sampleRegion[SMOOTHING_BRUSH_RADIUS + SMOOTHING_KERNEL_RADIUS + jj]
              * (1 - prob);
    }
}

std::vector<QPoint> interpolatePoints(const QPoint& previousPosition, const QPoint& finalPosition)
{
    std::vector<QPoint> container;
    if (previousPosition.x() == finalPosition.x()) {
        container.push_back(finalPosition);
        return container;
    }

    container.reserve(std::abs(previousPosition.x() - finalPosition.x()));

    // We do a simple linear interpolation to handle fast mouse movements
    if (previousPosition.x() != finalPosition.x()) {
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

    return container;
}
}

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

    auto xpos = std::vector<double>(slen);
    auto ypos = std::vector<double>(slen);
    const auto invRate = 1.0 / rate;

    for (size_t s = 0; s < slen; s++) {
        const double time = (s + s0).as_double() / rate;
        const double xx = zoomInfo.TimeToPositionF(time);
        xpos[s] = xx;

        const double value = clip.GetEnvelope().GetValue(time, invRate);
        const double tt = buffer[s] * value;

        ypos[s] = std::max(-1, std::min(static_cast<int>(metrics.height),
                                        samplespainterutils::getWaveYPos(tt, zoomMin, zoomMax, metrics.height, dB, true, dBRange, false)));
    }

    return SampleData(ypos, xpos);
}

std::optional<int> hitChannelIndex(std::shared_ptr<project::IAudacityProject> project, const trackedit::ClipKey& clipKey,
                                   const QPoint& position, const IWavePainter::Params& params)
{
    au::au3::Au3Project* au3Project = reinterpret_cast<au::au3::Au3Project*>(project->au3ProjectPtr());
    WaveTrack* track = au::au3::DomAccessor::findWaveTrack(*au3Project, TrackId(clipKey.trackId));
    if (!track) {
        return std::nullopt;
    }

    const std::vector<double> channelHeights {
        params.geometry.height * params.channelHeightRatio,
        params.geometry.height * (1 - params.channelHeightRatio),
    };

    int top = 0;
    for (size_t i = 0; i < track->NChannels(); ++i) {
        const int bottom = top + static_cast<int>(channelHeights[i]);
        const int y = position.y();

        if (y >= top && y <= bottom) {
            return i;
        }

        top = bottom;
    }

    return std::nullopt;
}

std::optional<int> hitNearestSampleChannelIndex(std::shared_ptr<au::project::IAudacityProject> project, const trackedit::ClipKey& clipKey,
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
            top += static_cast<int>(waveMetrics.height);
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

void setIsolatedPoint(const unsigned int currentChannel, const trackedit::ClipKey& clipKey,
                      std::shared_ptr<au::project::IAudacityProject> project, const QPoint& isolatedPoint,
                      const QPoint& currentPosition, const IWavePainter::Params& params)
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

    const auto isolatedPointTime = zoomInfo.PositionToTime(isolatedPoint.x());

    const auto clip = WaveChannelUtilities::GetClipAtTime(*waveChannel, isolatedPointTime + waveClip->GetPlayStartTime());
    if (!clip) {
        return;
    }

    const auto y = std::min(
        static_cast<int>(currentPosition.y() - (currentChannel * waveMetrics.height)), static_cast<int>(waveMetrics.height - 2));
    const auto yy = std::max(y, 2);

    float newValue = samplespainterutils::ValueOfPixel(yy, waveMetrics.height, false, dB, dBRange, zoomMin, zoomMax);

    WaveChannelUtilities::SetFloatAtTime(*waveChannel, isolatedPointTime + clip->GetPlayStartTime(), newValue, narrowestSampleFormat);
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

    const std::vector<QPoint> points = interpolatePoints(lastPosition, currentPosition);

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

    double startTime = -1.0;
    double clip_duration = waveClip->GetPlayDuration();
    double lastAdjustedTime = -1.0;

    std::vector<float> samples;
    samples.reserve(points.size());
    for (const auto& point : points) {
        const auto time = zoomInfo.PositionToTime(point.x());

        if (time < 0 || time > clip_duration) {
            //We do not process points from other clips
            continue;
        }

        if (startTime < 0 || startTime > clip_duration) {
            //Adjust clip duration to the first valid point
            startTime = time;
        }

        const auto sampleOffset = waveClip->TimeToSamples(time);
        const auto adjustedTime = waveClip->SamplesToTime(sampleOffset);

        if (adjustedTime == lastAdjustedTime) {
            //Remove points on the same sample
            continue;
        }
        lastAdjustedTime = adjustedTime;

        float oneSample;
        if (!WaveClipUtilities::GetFloatAtTime(*waveClip, adjustedTime, currentChannel, oneSample, false)) {
            continue;
        }

        const auto y = std::min(
            static_cast<int>(point.y() - (currentChannel * waveMetrics.height)), static_cast<int>(waveMetrics.height - 2));
        const auto yy = std::max(y, 2);

        float newValue = samplespainterutils::ValueOfPixel(yy, waveMetrics.height, false, dB, dBRange, zoomMin, zoomMax);
        samples.push_back(newValue);
    }

    if (startTime < 0) {
        return;
    }

    WaveChannelUtilities::SetFloatsFromTime(*waveChannel, startTime + waveClip->GetPlayStartTime(), samples.data(),
                                            samples.size(), narrowestSampleFormat,
                                            PlaybackDirection::forward);
}

void smoothLastClickPos(const unsigned int currentChannel, std::shared_ptr<au::project::IAudacityProject> project,
                        const trackedit::ClipKey& clipKey, const QPoint& currentPosition, const IWavePainter::Params& params)
{
    // Get the region size around the selected point (one central sample plus some kernel radius on both sides)
    static constexpr size_t SAMPLE_REGION_SIZE = 1 + 2 * (SMOOTHING_KERNEL_RADIUS + SMOOTHING_BRUSH_RADIUS);
    static constexpr size_t NEW_SAMPLE_REGION_SIZE = 1 + 2 * (SMOOTHING_BRUSH_RADIUS);

    //  Smoothing works like this:  There is a smoothing kernel radius constant that
    //  determines how wide the averaging window is.  Plus, there is a smoothing brush radius,
    //  which determines how many pixels wide around the selected pixel this smoothing is applied.
    //
    //  Samples will be replaced by a mixture of the original points and the smoothed points,
    //  with a triangular mixing probability whose value at the center point is
    //  SMOOTHING_PROPORTION_MAX and at the far bounds is SMOOTHING_PROPORTION_MIN

    au::au3::Au3Project* au3Project = reinterpret_cast<au::au3::Au3Project*>(project->au3ProjectPtr());
    WaveTrack* track = au::au3::DomAccessor::findWaveTrack(*au3Project, TrackId(clipKey.trackId));
    if (!track) {
        return;
    }

    std::shared_ptr<WaveClip> waveClip = au::au3::DomAccessor::findWaveClip(track, clipKey.clipId);
    if (!waveClip) {
        return;
    }

    const auto channels = track->Channels();
    if (currentChannel >= channels.size()) {
        return;
    }

    auto it = channels.begin();
    std::advance(it, currentChannel);

    auto channel = *it;

    auto waveMetrics = wavepainterutils::getWaveMetrics(project, clipKey, params);
    const ZoomInfo zoomInfo { waveMetrics.fromTime, waveMetrics.zoom };

    const auto time = zoomInfo.PositionToTime(currentPosition.x());
    const auto sampleOffset = waveClip->TimeToSamples(time);
    const auto adjustedTime = waveClip->SamplesToTime(sampleOffset);

    Floats sampleRegion{ SAMPLE_REGION_SIZE };
    Floats newSampleRegion{ NEW_SAMPLE_REGION_SIZE };

    //Get a sample from the clip to do some tricks on.
    constexpr auto mayThrow = false;
    const auto sampleRegionRange = WaveChannelUtilities::GetFloatsCenteredAroundTime(*channel,
                                                                                     adjustedTime + waveClip->GetPlayStartTime(),
                                                                                     sampleRegion.get(),
                                                                                     SMOOTHING_KERNEL_RADIUS + SMOOTHING_BRUSH_RADIUS,
                                                                                     mayThrow);

    applySmoothingKernel(sampleRegion, newSampleRegion, sampleRegionRange);
    mixSmoothingSamples(sampleRegion, newSampleRegion);

    // Set a range of samples around the mouse event
    // Don't require dithering later
    WaveChannelUtilities::SetFloatsCenteredAroundTime(*channel,
                                                      adjustedTime + waveClip->GetPlayStartTime(),
                                                      newSampleRegion.get(), SMOOTHING_BRUSH_RADIUS,
                                                      narrowestSampleFormat);
}
}
