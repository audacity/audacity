/*
 * Audacity: A Digital Audio Editor
 */

/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

Audacity: A Digital Audio Editor

ClipParameters.cpp

Matthieu Hodgkinson split from class WaveChannelView.cpp

**********************************************************************/
#include "ClipParameters.h"
#include "ClipInterface.h"

namespace au::spectrogram {
namespace {
// Returns an offset in seconds to be applied to the right clip
// boundary so that it does not overlap the last sample
double CalculateAdjustmentForZoomLevel(double avgPixPerSecond, bool showSamples)
{
    constexpr double pixelsOffset { 2 }; // The desired offset in pixels
    if (showSamples) {
        // adjustment so that the last circular point doesn't appear
        // to be hanging off the end
        return pixelsOffset
               / avgPixPerSecond; // pixels / ( pixels / second ) = seconds
    }
    return .0;
}

double GetPixelsPerSecond(const QRect& viewRect, const ZoomInfo& zoomInfo)
{
    const auto h = zoomInfo.PositionToTime(0);
    const auto trackRectT1 = zoomInfo.PositionToTime(viewRect.width());
    return viewRect.width() / (trackRectT1 - h);
}

bool ShowIndividualSamples(
    int sampleRate, double stretchRatio, double pixelsPerSecond)
{
    const auto secondsPerSample = stretchRatio / sampleRate;
    const auto pixelsPerSample = pixelsPerSecond * secondsPerSample;
    return pixelsPerSample > 0.5;
}

double GetBlankSpaceBeforePlayEndTime(const ClipTimes& clip)
{
    return 0.99 * clip.GetStretchRatio() / clip.GetRate();
}
} // namespace

ClipParameters::ClipParameters(const ClipTimes& clip, const QRect& trackPaintableSubrect, const ZoomInfo& zoomInfo)
    : trackRectT0{zoomInfo.viewportT0}, averagePixelsPerSecond{GetPixelsPerSecond(trackPaintableSubrect, zoomInfo)},
    showIndividualSamples{ShowIndividualSamples(
                              clip.GetRate(),
                              clip.GetStretchRatio(), averagePixelsPerSecond)}
{
    const auto trackRectT1 = zoomInfo.viewportT1;
    const auto playStartTime = clip.GetPlayStartTime();

    const double clipLength = clip.GetPlayEndTime() - clip.GetPlayStartTime();

    // Hidden duration because too far left.
    const auto hiddenDurationLeft = trackRectT0 - playStartTime;
    const auto tpost = trackRectT1 - playStartTime;

    const auto blank = GetBlankSpaceBeforePlayEndTime(clip);

    // Calculate actual selection bounds so that visibleT0 > 0 and visibleT1 < the
    // end of the track
    visibleT0 = std::max(hiddenDurationLeft, .0);
    visibleT1 = std::min(tpost, clipLength - blank)
                + CalculateAdjustmentForZoomLevel(
        averagePixelsPerSecond, showIndividualSamples);

    // Make sure visibleT1 (the right bound) is greater than 0
    if (visibleT1 < 0.0) {
        visibleT1 = 0.0;
    }

    // Make sure visibleT1 is greater than visibleT0
    if (visibleT0 > visibleT1) {
        visibleT0 = visibleT1;
    }

    // The variable "mid" will be the rectangle containing the
    // actual waveform, as opposed to any blank area before
    // or after the track.
    paintableClipRect = trackPaintableSubrect;

    // If the left edge of the track is to the right of the left
    // edge of the display, then there's some unused area to the
    // left of the track.  Reduce the "paintableClipRect"
    leftOffset = 0;
    if (hiddenDurationLeft < 0) {
        // Fix Bug #1296 caused by premature conversion to (int).
        int64_t time64 = zoomInfo.TimeToPosition(playStartTime);
        if (time64 < 0) {
            time64 = 0;
        }
        leftOffset = (time64 < trackPaintableSubrect.width()) ? (int)time64 : trackPaintableSubrect.width();

        paintableClipRect.setX(paintableClipRect.x() + leftOffset);
    }

    // If the right edge of the track is to the left of the right
    // edge of the display, then there's some unused area to the right
    // of the track.  Reduce the "paintableClipRect" rect by the
    // size of the blank area.
    if (tpost > visibleT1) {
        int64_t time64 = zoomInfo.TimeToPosition(playStartTime + visibleT1);
        if (time64 < 0) {
            time64 = 0;
        }
        const int hiddenRightOffset = (time64 < trackPaintableSubrect.width()) ? (int)time64 : trackPaintableSubrect.width();

        paintableClipRect.setWidth(std::max(0, hiddenRightOffset - leftOffset));
    }
}

QRect ClipParameters::GetClipRect(const ClipTimes& clip, const ZoomInfo& zoomInfo, const QRect& viewRect, bool* outShowSamples)
{
    const auto pixelsPerSecond = GetPixelsPerSecond(viewRect, zoomInfo);
    const auto showIndividualSamples = ShowIndividualSamples(
        clip.GetRate(), clip.GetStretchRatio(), pixelsPerSecond);
    const auto clipEndingAdjustment
        =CalculateAdjustmentForZoomLevel(pixelsPerSecond, showIndividualSamples);
    if (outShowSamples != nullptr) {
        *outShowSamples = showIndividualSamples;
    }
    constexpr auto edgeLeft = static_cast<int64_t>(std::numeric_limits<int>::min());
    constexpr auto edgeRight = static_cast<int64_t>(std::numeric_limits<int>::max());
    const auto left = std::clamp<int64_t>(zoomInfo.TimeToPosition(clip.GetPlayStartTime(), viewRect.x()), edgeLeft, edgeRight);
    const auto blank = GetBlankSpaceBeforePlayEndTime(clip);
    const auto right = std::clamp<int64_t>(zoomInfo.TimeToPosition(clip.GetPlayEndTime() - blank + clipEndingAdjustment,
                                                                   viewRect.x()), edgeLeft, edgeRight);
    if (right >= left) {
        // after clamping we can expect that left and right
        // are small enough to be put into int
        return QRect(
            static_cast<int>(left), viewRect.y(),
            std::max(1, static_cast<int>(right - left)), viewRect.height());
    }
    return QRect();
}
}
