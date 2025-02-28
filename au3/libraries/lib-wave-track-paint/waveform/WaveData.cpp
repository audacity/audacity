/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  WaveData.h

  Dmitry Vedenko

**********************************************************************/
#include "WaveData.h"

WaveformDisplay::WaveformDisplay(int w)
    : width(w)
{
}

void WaveformDisplay::AppendColumns(
    const WaveDisplayColumn* begin, const WaveDisplayColumn* end)
{
    ownColums.insert(ownColums.end(), begin, end);
    columns = ownColums.data();
}

void WaveformDisplay::Allocate()
{
    ownColums.reserve(width);

    if (width > 0) {
        columns = ownColums.data();
    }
}

WaveformDisplay::~WaveformDisplay() = default;
