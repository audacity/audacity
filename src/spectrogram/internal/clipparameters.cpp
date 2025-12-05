/*
 * Audacity: A Digital Audio Editor
 */
#include "clipparameters.h"
#include "spectrogramutils.h"

namespace au::spectrogram {
namespace {
double GetBlankSpaceBeforePlayEndTime(const ClipTimes& clip)
{
    return 0.99 * clip.stretchRatio / clip.playEndTime;
}
} // namespace

ClipParameters::ClipParameters(const ClipTimes& clip, const QRect& trackPaintableSubrect, const ViewInfo& viewInfo)
{
    const auto trackRectT1 = viewInfo.viewportT1;
    const auto playStartTime = clip.playStartTime;

    const double clipLength = clip.playEndTime - clip.playStartTime;

    // Hidden duration because too far left.
    const auto hiddenDurationLeft = viewInfo.viewportT0 - playStartTime;
    const auto tpost = trackRectT1 - playStartTime;

    const auto blank = GetBlankSpaceBeforePlayEndTime(clip);

    // Calculate actual selection bounds so that visibleT0 > 0 and visibleT1 < the
    // end of the track
    visibleT0 = std::max(hiddenDurationLeft, .0);
    const auto visibleT1 = std::max(std::min(tpost, clipLength - blank), 0.0);

    // Make sure visibleT1 is greater than visibleT0
    if (visibleT0 > visibleT1) {
        visibleT0 = visibleT1;
    }

    // If the left edge of the track is to the right of the left
    // edge of the display, then there's some unused area to the
    // left of the track.  Reduce the "paintableClipRect"
    leftOffset = 0;
    if (hiddenDurationLeft < 0) {
        // Fix Bug #1296 caused by premature conversion to (int).
        int64_t time64 = timeToPosition(viewInfo, playStartTime);
        if (time64 < 0) {
            time64 = 0;
        }
        leftOffset = (time64 < trackPaintableSubrect.width()) ? (int)time64 : trackPaintableSubrect.width();
    }
}
}
