/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  WaveData.h

  Dmitry Vedenko

**********************************************************************/
#include "WaveData.h"

WaveDisplay::WaveDisplay(int w)
    : width(w)
{
}

void WaveDisplay::AppendColumns(
   const WaveDisplayColumn* begin, const WaveDisplayColumn* end)
{
   ownColums.insert(ownColums.end(), begin, end);
   columns = ownColums.data();
}

void WaveDisplay::Allocate()
{
   ownColums.reserve(width);

   if (width > 0)
      columns = ownColums.data();
}

WaveDisplay::~WaveDisplay()
{
}
