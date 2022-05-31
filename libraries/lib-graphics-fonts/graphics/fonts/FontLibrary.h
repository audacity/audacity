/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  FontLibrary.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <cstdint>
#include <map>
#include <memory>

#include "graphics/FontInfo.h"

namespace graphics::fonts
{
class Font;
class FontFace;
class FontProvider;

class GRAPHICS_FONTS_API FontLibrary final
{
public:
   FontLibrary();
   ~FontLibrary();

   std::shared_ptr<Font> GetFont(const FontInfo& fontInfo);

   static void SetFontProvider(std::unique_ptr<FontProvider> fontProvider);

private:
   std::shared_ptr<FontFace> GetFontFace(FontInfo fontInfo);
   
   using FontFacesMap = std::map<FontInfo, std::shared_ptr<FontFace>>;
   using FontsMap = std::map<FontInfo, std::shared_ptr<Font>>;

   static std::unique_ptr<FontProvider> mProvider;

   FontFacesMap mFontFaces;
   FontsMap mFonts;
}; // class FontLibrary

GRAPHICS_FONTS_API FontLibrary& GetFontLibrary();
} // namespace graphics::fonts
