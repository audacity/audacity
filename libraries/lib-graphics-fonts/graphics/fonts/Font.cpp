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
      mFontFace->CreateTextLayout(FontSize { info.GetPointSize(), FontFace::BaseDPI() }, std::string_view(), true);
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

Size Font::GetTextSize(const std::string_view& text, bool gridFitted) const
{
   if (text.empty())
      return {};

   auto layout =
      GetLayoutCacheForDPI(FontFace::BaseDPI(), gridFitted).Get(text);

   return { static_cast<float>(layout->GetWidth()),
            static_cast<float>(layout->GetHeight()) };
}

void Font::DrawText(
   FontRenderer& renderer, std::string_view text, Color color) const
{
   if(text.empty())
      return;

   renderer.Draw(
      *this,
      *GetLayoutCacheForDPI(renderer.GetDPI(), renderer.IsHinted()).Get(text),
      color);
}

FontFace& Font::GetFontFace() noexcept
{
   return *mFontFace;
}

const FontFace& Font::GetFontFace() const noexcept
{
   return *mFontFace;
}

Font::LayoutCache& Font::GetLayoutCacheForDPI(uint32_t dpi, bool hinted) const
{
   auto& cache = mLayoutCaches[hinted ? 0 : 1];
   auto it = cache.find(dpi);

   if (it != cache.end())
      return it->second;

   auto result = cache.emplace(
      dpi, LayoutCache([dpi, hinted, this](const std::string& key)
                       { return CreateTextLayout(dpi, key, hinted); }));

   return result.first->second;
}

std::shared_ptr<TextLayout>
Font::CreateTextLayout(uint32_t dpi, std::string text, bool hinted) const
{
   if (text.empty())
      return mEmptyLayout;

   return mFontFace->CreateTextLayout(
      FontSize { mFontInfo.GetPointSize(), dpi }, text, hinted);
}

} // namespace graphics::fonts
