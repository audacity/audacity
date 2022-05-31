/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  Font.cpp

  Dmitry Vedenko

**********************************************************************/
#include "Font.h"

#include "TextLayout.h"
#include "FontRenderer.h"
#include "FontFace.h"

namespace graphics::fonts
{
Font::Font(
   const FontInfo& info, std::shared_ptr<FontFace> face)
    : PainterFont(GetRendererIndependentID())
    , mFontInfo(info)
    , mFontFace(std::move(face))
{
   mEmptyLayout =
      mFontFace->CreateTextLayout(FontSize { info.GetPointSize(), FontFace::BaseDPI() }, std::string_view());
}

Font::~Font()
{
}

std::string_view Font::GetFace() const
{
   return mFontInfo.GetFaceName();
}

float Font::GetFontSize() const
{
   return mFontInfo.GetPointSize();
}

PainterFont::Metrics Font::GetFontMetrics() const
{
   return mFontFace->GetMetrics(FontSize { mFontInfo.GetPointSize(), FontFace::BaseDPI() });
}

Size Font::GetTextSize(const std::string_view& text) const
{
   auto layout = GetLayoutCacheForDPI(FontFace::BaseDPI()).Get(text);
   
   return { static_cast<float>(layout->GetWidth()),
            static_cast<float>(layout->GetHeight()) };
}

void Font::DrawText(
   FontRenderer& renderer, std::string_view text, Color color) const
{
   renderer.Draw(
      *this, *GetLayoutCacheForDPI(renderer.GetDPI()).Get(text), color);
}

FontFace& Font::GetFontFace() noexcept
{
   return *mFontFace;
}

const FontFace& Font::GetFontFace() const noexcept
{
   return *mFontFace;
}

Font::LayoutCache& Font::GetLayoutCacheForDPI(uint32_t dpi) const
{
   auto it = mLayoutCache.find(dpi);

   if (it != mLayoutCache.end())
      return it->second;

   auto result = mLayoutCache.emplace(
      dpi, LayoutCache([dpi, this](const std::string& key)
                       { return CreateTextLayout(dpi, key); }));

   return result.first->second;
}

std::shared_ptr<TextLayout>
Font::CreateTextLayout(uint32_t dpi, std::string text) const
{
   if (text.empty())
      return mEmptyLayout;
   
   return mFontFace->CreateTextLayout(
      FontSize { mFontInfo.GetPointSize(), dpi }, text);
}

} // namespace graphics::fonts
