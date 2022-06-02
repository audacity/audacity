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
   Font(const FontInfo& info, std::shared_ptr<FontFace> face);
   
public:
   ~Font();
   
   std::string_view GetFace() const override;

   float GetFontSize() const override;

   Metrics GetFontMetrics() const override;

   Size GetTextSize(const std::string_view& text, bool gridFitted = true) const override;
   void DrawText(FontRenderer& renderer, std::string_view text, Color color) const;

   FontFace& GetFontFace() noexcept;
   const FontFace& GetFontFace() const noexcept;

private:
   using LayoutCache =
      GraphicsObjectCache<std::string, std::shared_ptr<TextLayout>>;
   using DPIAwareLayoutCache = std::map<uint32_t, LayoutCache>;
   
   LayoutCache& GetLayoutCacheForDPI(uint32_t dpi, bool hinted) const;
   std::shared_ptr<TextLayout> CreateTextLayout(uint32_t dpi, std::string text, bool hinted) const;
   
   FontInfo mFontInfo;
   std::shared_ptr<FontFace> mFontFace;
   
   mutable DPIAwareLayoutCache mLayoutCaches[2];

   std::shared_ptr<TextLayout> mEmptyLayout;

   friend class FontLibrary;
}; // class Font

} // namespace graphics::fonts
