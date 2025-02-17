/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

Audacity: A Digital Audio Editor

ClipParameters.cpp

Matthieu Hodgkinson split from class WaveChannelView.cpp

**********************************************************************/
#include "ClipParameters.h"
#include "ClipInterface.h"
#include "ZoomInfo.h"

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

double GetPixelsPerSecond(const wxRect& viewRect, const ZoomInfo& zoomInfo)
{
    const auto h = zoomInfo.PositionToTime(0, 0, true);
    const auto trackRectT1 = zoomInfo.PositionToTime(viewRect.width, 0, true);
    return viewRect.width / (trackRectT1 - h);
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

ClipParameters::ClipParameters(
    const ClipTimes& clip, const wxRect& rect, const ZoomInfo& zoomInfo)
    : trackRectT0{zoomInfo.PositionToTime(0, 0, true)}
    , averagePixelsPerSecond{GetPixelsPerSecond(rect, zoomInfo)}
    , showIndividualSamples{ShowIndividualSamples(
                                clip.GetRate(), clip.GetStretchRatio(), averagePixelsPerSecond)}
{
    const auto trackRectT1 = zoomInfo.PositionToTime(rect.width, 0, true);
    const auto stretchRatio = clip.GetStretchRatio();
    const auto playStartTime = clip.GetPlayStartTime();

    const double clipLength = clip.GetPlayEndTime() - clip.GetPlayStartTime();

    // Hidden duration because too far left.
    const auto tpre = trackRectT0 - playStartTime;
    const auto tpost = trackRectT1 - playStartTime;

    const auto blank = GetBlankSpaceBeforePlayEndTime(clip);

    // Calculate actual selection bounds so that t0 > 0 and t1 < the
    // end of the track
    t0 = std::max(tpre, .0);
    t1 = std::min(tpost, clipLength - blank)
         + CalculateAdjustmentForZoomLevel(
        averagePixelsPerSecond, showIndividualSamples);

    // Make sure t1 (the right bound) is greater than 0
    if (t1 < 0.0) {
        t1 = 0.0;
    }

    // Make sure t1 is greater than t0
    if (t0 > t1) {
        t0 = t1;
    }

    // The variable "hiddenMid" will be the rectangle containing the
    // actual waveform, as opposed to any blank area before
    // or after the track, as it would appear without the fisheye.
    hiddenMid = rect;

    // If the left edge of the track is to the right of the left
    // edge of the display, then there's some unused area to the
    // left of the track.  Reduce the "hiddenMid"
    hiddenLeftOffset = 0;
    if (tpre < 0) {
        // Fix Bug #1296 caused by premature conversion to (int).
        wxInt64 time64 = zoomInfo.TimeToPosition(playStartTime, 0, true);
        if (time64 < 0) {
            time64 = 0;
        }
        hiddenLeftOffset = (time64 < rect.width) ? (int)time64 : rect.width;

        hiddenMid.x += hiddenLeftOffset;
        hiddenMid.width -= hiddenLeftOffset;
    }

    // If the right edge of the track is to the left of the right
    // edge of the display, then there's some unused area to the right
    // of the track.  Reduce the "hiddenMid" rect by the
    // size of the blank area.
    if (tpost > t1) {
        wxInt64 time64 = zoomInfo.TimeToPosition(playStartTime + t1, 0, true);
        if (time64 < 0) {
            time64 = 0;
        }
        const int hiddenRightOffset
            =(time64 < rect.width) ? (int)time64 : rect.width;

        hiddenMid.width = std::max(0, hiddenRightOffset - hiddenLeftOffset);
    }
    // The variable "mid" will be the rectangle containing the
    // actual waveform, as distorted by the fisheye,
    // as opposed to any blank area before or after the track.
    mid = rect;

    // If the left edge of the track is to the right of the left
    // edge of the display, then there's some unused area to the
    // left of the track.  Reduce the "mid"
    leftOffset = 0;
    if (tpre < 0) {
        wxInt64 time64 = zoomInfo.TimeToPosition(playStartTime, 0, false);
        if (time64 < 0) {
            time64 = 0;
        }
        leftOffset = (time64 < rect.width) ? (int)time64 : rect.width;

        mid.x += leftOffset;
        mid.width -= leftOffset;
    }

    // If the right edge of the track is to the left of the right
    // edge of the display, then there's some unused area to the right
    // of the track.  Reduce the "mid" rect by the
    // size of the blank area.
    if (tpost > t1) {
        wxInt64 time64 = zoomInfo.TimeToPosition(playStartTime + t1, 0, false);
        if (time64 < 0) {
            time64 = 0;
        }
        const int distortedRightOffset
            =(time64 < rect.width) ? (int)time64 : rect.width;

        mid.width = std::max(0, distortedRightOffset - leftOffset);
    }
}

wxRect ClipParameters::GetClipRect(
    const ClipTimes& clip, const ZoomInfo& zoomInfo, const wxRect& viewRect,
    bool* outShowSamples)
{
    const auto pixelsPerSecond = GetPixelsPerSecond(viewRect, zoomInfo);
    const auto showIndividualSamples = ShowIndividualSamples(
        clip.GetRate(), clip.GetStretchRatio(), pixelsPerSecond);
    const auto clipEndingAdjustment
        =CalculateAdjustmentForZoomLevel(pixelsPerSecond, showIndividualSamples);
    if (outShowSamples != nullptr) {
        *outShowSamples = showIndividualSamples;
    }
    constexpr auto edgeLeft
        =static_cast<ZoomInfo::int64>(std::numeric_limits<int>::min());
    constexpr auto edgeRight
        =static_cast<ZoomInfo::int64>(std::numeric_limits<int>::max());
    const auto left = std::clamp(
        zoomInfo.TimeToPosition(clip.GetPlayStartTime(), viewRect.x, true),
        edgeLeft, edgeRight);
    const auto right = std::clamp(
        zoomInfo.TimeToPosition(
            clip.GetPlayEndTime() - GetBlankSpaceBeforePlayEndTime(clip)
            + clipEndingAdjustment,
            viewRect.x, true),
        edgeLeft, edgeRight);
    if (right >= left) {
        // after clamping we can expect that left and right
        // are small enough to be put into int
        return wxRect(
            static_cast<int>(left), viewRect.y,
            std::max(1, static_cast<int>(right - left)), viewRect.height);
    }
    return wxRect();
}
