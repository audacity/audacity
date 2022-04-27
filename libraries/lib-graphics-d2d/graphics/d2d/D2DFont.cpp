/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  D2DFont.cpp

  Dmitry Vedenko

**********************************************************************/
#include "D2DFont.h"
#include "CodeConversions.h"

namespace
{
DWRITE_FONT_WEIGHT GetDWriteFontWeight(const FontInfo& info) noexcept
{
   return static_cast<DWRITE_FONT_WEIGHT>(info.GetFontWeight());
}

DWRITE_FONT_STYLE GetDWriteFontStyle(const FontInfo& info) noexcept
{
   switch (info.GetFontStyle())
   {
   case FontStyle::Normal:
      return DWRITE_FONT_STYLE_NORMAL;
   case FontStyle::Oblique:
      return DWRITE_FONT_STYLE_OBLIQUE;
   case FontStyle::Italic:
      return DWRITE_FONT_STYLE_ITALIC;
   default:
      return DWRITE_FONT_STYLE_NORMAL;
   }
}

DWRITE_FONT_STRETCH GetDWriteFontStretch(const FontInfo& info) noexcept
{
   switch (info.GetFontStretch())
   {
   case FontStretch::Undefined:
      return DWRITE_FONT_STRETCH_UNDEFINED;
   case FontStretch::UltraCondensed:
      return DWRITE_FONT_STRETCH_ULTRA_CONDENSED;
   case FontStretch::ExtraCondensed:
      return DWRITE_FONT_STRETCH_EXTRA_CONDENSED;
   case FontStretch::Condensed:
      return DWRITE_FONT_STRETCH_CONDENSED;
   case FontStretch::SemiCondensed:
      return DWRITE_FONT_STRETCH_SEMI_CONDENSED;
   case FontStretch::Normal:
      return DWRITE_FONT_STRETCH_NORMAL;
   case FontStretch::SemiExpanded:
      return DWRITE_FONT_STRETCH_SEMI_EXPANDED;
   case FontStretch::Expanded:
      return DWRITE_FONT_STRETCH_EXPANDED;
   case FontStretch::ExtraExpanded:
      return DWRITE_FONT_STRETCH_EXTRA_EXPANDED;
   case FontStretch::UltraExpanded:
      return DWRITE_FONT_STRETCH_ULTRA_EXPANDED;
   default:
      return DWRITE_FONT_STRETCH_NORMAL;
   }
}
}

D2DFont::D2DFont(
   const RendererID& rendererId, IDWriteFactory* factory, uint32_t dpi,
   const FontInfo& fontInfo)
    : PainterFont(rendererId)
    , mFontInfo(fontInfo)
    , mDWriteFactory(factory)
{
   LCID lcid = GetThreadLocale();
   
   wchar_t localName[LOCALE_NAME_MAX_LENGTH];
   if (LCIDToLocaleName(lcid, localName, LOCALE_NAME_MAX_LENGTH, 0) == 0)
      return;

   const std::wstring faceName = audacity::ToWString(fontInfo.GetFaceName());
   
   HRESULT result = mDWriteFactory->CreateTextFormat(
      faceName.c_str(), nullptr, GetDWriteFontWeight(fontInfo),
      GetDWriteFontStyle(fontInfo), GetDWriteFontStretch(fontInfo),
      fontInfo.GetPointSize() * dpi / 72.0, localName,
      mTextFormat.ReleaseAndGetAddressOf());

   if (result != S_OK)
      return;

   UpdateFontMetrics();
}

std::string_view D2DFont::GetFace() const
{
   return mFontInfo.GetFaceName();
}

float D2DFont::GetFontSize() const
{
   return mFontInfo.GetPointSize();
}

PainterFont::Metrics D2DFont::GetFontMetrics() const
{
   return mMetrics;
}

Size D2DFont::GetTextSize(const std::string_view& text) const
{
   return {};
}

bool D2DFont::IsValid() const
{
   return mTextFormat;
}

void D2DFont::UpdateFontMetrics()
{
   using namespace Microsoft::WRL;

   ComPtr<IDWriteFontCollection> fontCollection;

   HRESULT result = mTextFormat->GetFontCollection(
      fontCollection.GetAddressOf());

   if (result != S_OK)
   {
      mTextFormat.Reset();
      return;
   }

   const auto nameLength = mTextFormat->GetFontFamilyNameLength() + 1;

   std::wstring familyName(nameLength, L'\0');

   result = mTextFormat->GetFontFamilyName(familyName.data(), nameLength);

   if (result != S_OK)
   {
      mTextFormat.Reset();
      return;
   }

   UINT32 index;
   BOOL exists;

   result = fontCollection->FindFamilyName(familyName.c_str(), &index, &exists);

   if (result != S_OK || !exists)
   {
      mTextFormat.Reset();
      return;
   }

   ComPtr<IDWriteFontFamily> fontFamily;

   result = fontCollection->GetFontFamily(index, fontFamily.GetAddressOf());

   if (result != S_OK)
   {
      mTextFormat.Reset();
      return;
   }

   ComPtr<IDWriteFont> font;

   result = fontFamily->GetFirstMatchingFont(
      mTextFormat->GetFontWeight(), mTextFormat->GetFontStretch(),
      mTextFormat->GetFontStyle(), font.GetAddressOf());

   if (result != S_OK)
   {
      mTextFormat.Reset();
      return;
   }

   DWRITE_FONT_METRICS metrics;

   font->GetMetrics(&metrics);
   
   const float Ratio =
      mTextFormat->GetFontSize() / static_cast<float>(metrics.designUnitsPerEm);

   mMetrics.Ascent = metrics.ascent * Ratio;
   mMetrics.Descent = metrics.descent * Ratio;
   mMetrics.Linegap = metrics.lineGap * Ratio;
   mMetrics.LineHeight = mMetrics.Ascent + mMetrics.Descent;
}
