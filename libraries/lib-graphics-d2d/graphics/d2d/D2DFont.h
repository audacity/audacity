/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  D2DFont.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <dwrite.h>
#include <wrl.h>

#include "graphics/Painter.h"
#include "graphics/FontInfo.h"

#include "GraphicsObjectCache.h"

namespace graphics::d2d
{

class D2DRenderTarget;

class D2DFont final : public PainterFont
{
public:
   D2DFont(
      const RendererID& rendererId, IDWriteFactory* factory, uint32_t dpi,
      const FontInfo& fontInfo);

   D2DFont(const D2DFont&) = delete;
   D2DFont& operator=(const D2DFont&) = delete;

   std::string_view GetFace() const override;

   float GetFontSize() const override;

   Metrics GetFontMetrics() const override;

   Size GetTextSize(const std::string_view& text) const override;

   bool IsValid() const;

   void Draw(
      D2DRenderTarget& renderTarget, Point origin, Brush backgroundBrush,
      const std::string_view& text) const;

   void ClenupDWriteResources();

private:
   void UpdateFontMetrics();

   Microsoft::WRL::ComPtr<IDWriteTextLayout>
   CreateLayout(const std::string& text) const;

   FontInfo mFontInfo;
   Metrics mMetrics;

   IDWriteFactory* mDWriteFactory;

   Microsoft::WRL::ComPtr<IDWriteTextFormat> mTextFormat;

   using LayoutCache = GraphicsObjectCache<
      std::string, Microsoft::WRL::ComPtr<IDWriteTextLayout>>;
   mutable LayoutCache mLayoutCache;
};

} // namespace graphics::d2d
