/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  WaveData.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <vector>

#include "PixelSampleMapper.h"

// Data for sample blocks related to the column
struct GRAPHICS_API WaveDisplayColumn final
{
   float min;
   float max;
   float rms;
};
// A bundle of arrays needed for drawing waveforms.  The object may or may not
// own the storage for those arrays.  If it does, it destroys them.
class GRAPHICS_API WaveDisplay final
{
public:
   int width { 0 };

   WaveDisplayColumn* columns { nullptr };
   PixelSampleMapper mapper;

   void
   AppendColumns(const WaveDisplayColumn* begin, const WaveDisplayColumn* end);

private:
   std::vector<WaveDisplayColumn> ownColums;

public:
   WaveDisplay(int w);

   // Create "own" arrays.
   void Allocate();

   ~WaveDisplay();
};
