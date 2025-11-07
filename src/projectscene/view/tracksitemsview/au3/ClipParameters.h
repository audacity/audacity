/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

Audacity: A Digital Audio Editor

ClipParameters.h

Matthieu Hodgkinson split from class WaveChannelView.h

**********************************************************************/
#pragma once

#include <wx/gdicmn.h>

class ZoomInfo;
class ClipTimes;

struct AUDACITY_DLL_API ClipParameters
{
    // Do a bunch of calculations common to waveform and spectrum drawing.
    ClipParameters(
        const ClipTimes& clip, const wxRect& rect, const ZoomInfo& zoomInfo);

    const double trackRectT0; // absolute time of left edge of track

    // Lower and upper visible time boundaries (relative to clip). If completely
    // off-screen, `t0 == t1`.
    double t0;
    double t1;

    const double averagePixelsPerSecond;
    const bool showIndividualSamples;

    wxRect hiddenMid;
    int hiddenLeftOffset;

    wxRect mid;
    int leftOffset;

    // returns a clip rectangle restricted by viewRect,
    // and with clipOffsetX - clip horizontal origin offset within view rect
    static wxRect GetClipRect(
        const ClipTimes& clip, const ZoomInfo& zoomInfo, const wxRect& viewRect, bool* outShowSamples = nullptr);
};
