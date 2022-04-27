/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  D2DRenderer.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <memory>

#include "graphics/Painter.h"
#include "graphics/RendererID.h"

struct ID2D1Factory;
class ID2D1StrokeStyle;

class FontInfo;
class D2DFontCollection;

class GRAPHICS_D2D_API D2DRenderer final
{
public:
   D2DRenderer();
   ~D2DRenderer();

   bool IsAvailable() const noexcept;
   void Shutdown();

   std::unique_ptr<Painter> CreateHWNDPainter(void* window, const FontInfo& defaultFont);
   std::unique_ptr<Painter> CreateHDCPainter(void*  dc, const FontInfo& defaultFont);
   std::unique_ptr<Painter> CreateMeasuringPainter(const FontInfo& defaultFont);

   std::shared_ptr<PainterImage> CreateImage(
      PainterImageFormat format, uint32_t width, uint32_t height,
      const void* data, const void* alphaData);

   RendererID GetRendererID() const noexcept;

   D2DFontCollection* GetFontCollection();

   ID2D1Factory* GetD2DFactory() const;

   ID2D1StrokeStyle* GetStrokeStyle(const Pen& pen);

private:
   class D2DRendererImpl;
   std::unique_ptr<D2DRendererImpl> mImpl;
};

GRAPHICS_D2D_API D2DRenderer& SharedD2DRenderer();
