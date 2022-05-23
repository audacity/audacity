/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  FontFace.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <cstdint>
#include <string_view>
#include <memory>
#include <vector>

#include "graphics/Painter.h"
#include "graphics/FontInfo.h"

namespace graphics::fonts
{
class TextLayout;
class FontProvider;

struct FontSymbol final
{
   std::vector<Color> bitmap;
   
   uint32_t width;
   uint32_t height;

   int32_t left;
   int32_t top;
};

struct FontSize final
{
   float pointSize;
   uint32_t dpi;
};

class GRAPHICS_FONTS_API FontFace final
{
   FontFace(FontProvider& provider, const FontInfo& info, size_t libraryIndex);
public:
   ~FontFace();

   std::shared_ptr<TextLayout> CreateTextLayout(FontSize fontSize, std::string_view text) const;
   FontSymbol GetFontSymbol(uint32_t pixelSize, uint32_t codepoint, bool hinted) const;

   float GetPointSize() const noexcept;

   const PainterFont::Metrics& GetMetrics(FontSize fontSize) const noexcept;

   const FontInfo& GetFontInfo() const noexcept;

   bool IsOk() const noexcept;

   size_t GetLibraryIndex() const noexcept;

   static uint32_t GetPixelSize(FontSize size) noexcept;

private:
   
   FontInfo mFontInfo;

   size_t mLibraryIndex { std::numeric_limits<size_t>::max() };

   class FreeTypeFace;
   std::unique_ptr<FreeTypeFace> mFreetypeFace;   
   
   friend class FontLibrary;
};
} // namespace graphics::fonts
