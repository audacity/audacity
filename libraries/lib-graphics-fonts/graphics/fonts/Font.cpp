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
   const FontInfo& info, std::shared_ptr<FontFace> face, uint32_t dpi)
    : PainterFont(GetRendererIndependentID())
    , mFontInfo(info)
    , mFontFace(std::move(face))
    , mDPI(dpi)
    , mLayoutCache([this](const std::string& text)
                   { return CreateTextLayout(text); })
{
   mEmptyLayout =
      mFontFace->CreateTextLayout(FontSize { info.GetPointSize(), dpi }, std::string_view());
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

uint32_t Font::GetPixelSize() const noexcept
{
   return mFontFace->GetPixelSize(FontSize { mFontInfo.GetPointSize(), mDPI });
}

uint32_t Font::GetDPI() const noexcept
{
   return mDPI;
}

PainterFont::Metrics Font::GetFontMetrics() const
{
   return mFontFace->GetMetrics(FontSize { mFontInfo.GetPointSize(), mDPI });
}

Size Font::GetTextSize(const std::string_view& text) const
{
   auto layout = mLayoutCache.Get(text);
   
   return { static_cast<float>(layout->GetWidth()),
            static_cast<float>(layout->GetHeight()) };
}

void Font::DrawText(
   FontRenderer& renderer, std::string_view text, Color color) const
{
   renderer.Draw(*this, *mLayoutCache.Get(text), color);
}

FontFace& Font::GetFontFace() noexcept
{
   return *mFontFace;
}

const FontFace& Font::GetFontFace() const noexcept
{
   return *mFontFace;
}

std::shared_ptr<TextLayout> Font::CreateTextLayout(std::string text) const
{
   if (text.empty())
      return mEmptyLayout;
   
   return mFontFace->CreateTextLayout(
      FontSize { mFontInfo.GetPointSize(), mDPI }, text);
}

} // namespace graphics::fonts
