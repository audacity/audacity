/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "./spectrogramutils.h"

#include <QRect>

namespace au::spectrogram {
struct ClipTimes {
    const double playStartTime;
    const double playEndTime;
    const double stretchRatio;
};

struct ClipParameters
{
    // Do a bunch of calculations common to waveform and spectrum drawing.
    /**
     * @param trackPaintableSubrect Let `rect` be the rectangle occupied by a track in the UI, including track header and the upper rectangle reserved for clip titles.
     * `trackPaintableSubrect` is a subrectangle of `rect` where x is the width of the track header and y the height of the clip headers,
     * and beside that filling the remaining rectangle area. In other words, the rectangle where spectrogram values on the track may be painted (or not if
     * there is no clip there).
     */
    ClipParameters(const ClipTimes& clip, const QRect& trackPaintableSubrect, const ZoomInfo& zoomInfo);

    // Lower and upper visible time boundaries (relative to clip). If completely
    // off-screen, `t0 == t1`.
    double visibleT0;

    int leftOffset;
};
}
