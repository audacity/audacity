/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  WaveClipPainter.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <memory>

#include "graphics/Rect.h"

class WavePaintParameters;
class WaveDataCache;
class ZoomInfo;

namespace graphics
{
class Painter;
class RendererID;
}


//! Base class for the WaveClipPainters
class WAVE_TRACK_PAINT_API WaveClipPainter /* not final */
{
public:
   virtual ~WaveClipPainter() = default;
   
   virtual void SetSelection(const ZoomInfo& zoom, float t0, float t1) = 0;
   virtual void Draw(
      graphics::Painter& painter, const WavePaintParameters& params,
      const ZoomInfo& zoomInfo, const graphics::Rect& targetRect,
      float leftOffset, float from, float to) = 0;
}; // class WaveClipPainter

WAVE_TRACK_PAINT_API std::shared_ptr<WaveClipPainter> CreateWaveClipPainter(
   const graphics::RendererID& rendererID,
   std::shared_ptr<WaveDataCache> dataCache);
