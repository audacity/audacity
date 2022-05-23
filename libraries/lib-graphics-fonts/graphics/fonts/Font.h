/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  Font.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <cstdint>
#include <numeric>
#include <memory>
#include <string_view>
#include <vector>

#include "graphics/Painter.h"
#include "graphics/FontInfo.h"
#include "GraphicsObjectCache.h"

namespace graphics::fonts
{
class TextLayout;

class FontFace;

class FontRenderer;
struct RenderingParameters;
   
class GRAPHICS_FONTS_API Font final : public PainterFont
{
private:
   Font(const FontInfo& info, std::shared_ptr<FontFace> face, uint32_t dpi);
   
public:
   ~Font();
   
   std::string_view GetFace() const override;

   float GetFontSize() const override;
   uint32_t GetPixelSize() const noexcept;
   uint32_t GetDPI() const noexcept;

   Metrics GetFontMetrics() const override;

   Size GetTextSize(const std::string_view& text) const override;
   void DrawText(FontRenderer& renderer, std::string_view text, Color color) const;

   FontFace& GetFontFace() noexcept;
   const FontFace& GetFontFace() const noexcept;

private:
   
   std::shared_ptr<TextLayout> CreateTextLayout(std::string text) const;
   
   FontInfo mFontInfo;
   std::shared_ptr<FontFace> mFontFace;

   uint32_t mDPI;
   
   using LayoutCache = GraphicsObjectCache<std::string, std::shared_ptr<TextLayout>>;
   mutable LayoutCache mLayoutCache;

   std::shared_ptr<TextLayout> mEmptyLayout;

   friend class FontLibrary;
}; // class Font

} // namespace graphics::fonts
