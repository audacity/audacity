/*
 * Audacity: A Digital Audio Editor
 */

/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

Audacity: A Digital Audio Editor

ClipParameters.h

Matthieu Hodgkinson split from class WaveChannelView.h

**********************************************************************/
#pragma once

#include "../spectrogramutils.h"

#include <QRect>

class ClipTimes;

namespace au::spectrogram {
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

    const double trackRectT0; // absolute time of left edge of track

    // Lower and upper visible time boundaries (relative to clip). If completely
    // off-screen, `t0 == t1`.
    double visibleT0;
    double visibleT1;

    const double averagePixelsPerSecond;
    const bool showIndividualSamples;

    QRect paintableClipRect;
    int leftOffset;
};
}
