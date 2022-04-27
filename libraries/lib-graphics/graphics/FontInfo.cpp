/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  FontInfo.cpp

  Dmitry Vedenko

**********************************************************************/
#include "FontInfo.h"

#include <tuple>

FontInfo::FontInfo(std::string_view faceName, float pointSize)
    : mFaceName(faceName)
    , mPointSize(pointSize)
{
}

FontInfo& FontInfo::SetFaceName(std::string_view faceName)
{
   mFaceName = std::string(faceName);
   return *this;
}

std::string_view FontInfo::GetFaceName() const noexcept
{
   return mFaceName;
}

FontInfo& FontInfo::SetPointSize(float pointSize) noexcept
{
   mPointSize = pointSize;
   return *this;
}

float FontInfo::GetPointSize() const noexcept
{
   return mPointSize;
}

FontInfo& FontInfo::SetFontWeight(FontWeight weight) noexcept
{
   mFontWeight = weight;
   return *this;
}

FontWeight FontInfo::GetFontWeight() const noexcept
{
   return mFontWeight;
}

FontInfo& FontInfo::SetFontStyle(FontStyle style) noexcept
{
   mFontStyle = style;
   return *this;
}

FontStyle FontInfo::GetFontStyle() const noexcept
{
   return mFontStyle;
}

FontInfo& FontInfo::SetFontStretch(FontStretch stretch) noexcept
{
   mFontStretch = stretch;
   return *this;
}

FontStretch FontInfo::GetFontStretch() const noexcept
{
   return mFontStretch;
}

FontInfo& FontInfo::SetUnderlined(bool underlined) noexcept
{
   mUnderlined = underlined;
   return *this;
}

bool FontInfo::GetUnderlined() const noexcept
{
   return mUnderlined;
}

FontInfo& FontInfo::SetStrikethrough(bool strikethrough) noexcept
{
   mStrikethrough = strikethrough;
   return *this;
}

bool FontInfo::GetStrikethrough() const noexcept
{
   return mStrikethrough;
}

namespace
{
auto MakeTuple(const FontInfo& font)
{
   return std::make_tuple(
      font.GetFaceName(), font.GetPointSize(), font.GetFontWeight(),
      font.GetFontStyle(), font.GetFontStretch(), font.GetUnderlined(),
      font.GetStrikethrough());
}
}

bool operator==(const FontInfo& lhs, const FontInfo& rhs)
{
   return MakeTuple(lhs) == MakeTuple(rhs);
}

bool operator!=(const FontInfo& lhs, const FontInfo& rhs)
{
   return MakeTuple(lhs) != MakeTuple(rhs);
}

bool operator<(const FontInfo& lhs, const FontInfo& rhs)
{
   return MakeTuple(lhs) < MakeTuple(rhs);
}
