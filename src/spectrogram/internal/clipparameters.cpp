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
    const auto trackRectStartTime = viewInfo.viewportEndTime;
    const auto playStartTime = clip.playStartTime;

    const double clipLength = clip.playEndTime - clip.playStartTime;

    // Hidden duration because too far left.
    const auto hiddenDurationLeft = viewInfo.viewportStartTime - playStartTime;
    const auto tpost = trackRectStartTime - playStartTime;

    const auto blank = GetBlankSpaceBeforePlayEndTime(clip);

    // Calculate actual selection bounds so that m_visibleStartTime > 0 and visibleEndTime < the
    // end of the track
    m_visibleStartTime = std::max(hiddenDurationLeft, .0);
    const auto visibleEndTime = std::max(std::min(tpost, clipLength - blank), 0.0);

    if (m_visibleStartTime > visibleEndTime) {
        m_visibleStartTime = visibleEndTime;
    }

    // If the left edge of the track is to the right of the left
    // edge of the display, then there's some unused area to the
    // left of the track.  Reduce the "paintableClipRect"
    m_leftOffset = 0;
    if (hiddenDurationLeft < 0) {
        // Fix Bug #1296 caused by premature conversion to (int).
        int64_t time64 = timeToPosition(viewInfo, playStartTime);
        if (time64 < 0) {
            time64 = 0;
        }
        m_leftOffset = (time64 < trackPaintableSubrect.width()) ? (int)time64 : trackPaintableSubrect.width();
    }
}
}
