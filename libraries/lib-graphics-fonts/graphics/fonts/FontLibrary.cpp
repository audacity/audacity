/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  FontLibrary.cpp

  Dmitry Vedenko

**********************************************************************/
#include "FontLibrary.h"

#include <cassert>

#include "Font.h"
#include "FontFace.h"
#include "FontProvider.h"

namespace graphics::fonts
{

std::unique_ptr<FontProvider> FontLibrary::mProvider;
   
FontLibrary& GetFontLibrary()
{
   static FontLibrary library;
   return library;
}

FontLibrary::FontLibrary()
{
}

FontLibrary::~FontLibrary()
{
}

std::shared_ptr<Font>
FontLibrary::GetFont(const FontInfo& fontInfo, uint32_t dpi)
{
   if (mProvider == nullptr)
   {
      assert(mProvider != nullptr);
      return {};
   }
   
   const auto fontKey = std::make_pair(dpi, fontInfo);

   auto it = mFonts.find(fontKey);

   if (it != mFonts.end())
      return it->second;

   auto fontFace = GetFontFace(fontInfo, dpi);

   if (fontFace == nullptr)
      return {};

   auto font = std::shared_ptr<Font>(new Font(fontInfo, std::move(fontFace), dpi));
   
   mFonts.emplace(fontKey, font);

   return font;
}

void FontLibrary::SetFontProvider(std::unique_ptr<FontProvider> fontProvider)
{
   mProvider = std::move(fontProvider);
}

std::shared_ptr<FontFace>
FontLibrary::GetFontFace(FontInfo fontInfo, uint32_t dpi)
{
   if (mProvider == nullptr)
   {
      assert(mProvider != nullptr);
      return {};
   }

   fontInfo = fontInfo
      .SetStrikethrough(false)
      .SetUnderlined(false)
      .SetPointSize(0.0f);

   auto it = mFontFaces.find(fontInfo);

   if (it != mFontFaces.end())
      return it->second;

   auto fontFace = std::shared_ptr<FontFace>(
      new FontFace(*mProvider, fontInfo, mFontFaces.size()));

   if (!fontFace->IsOk())
      return {};

   mFontFaces.emplace(fontInfo, fontFace);

   return fontFace;
}

} // namespace graphics::fonts
