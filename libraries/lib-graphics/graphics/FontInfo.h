/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  FontInfo.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <string>
#include <string_view>

namespace graphics
{

// As defined in the OpenType specification
enum class FontWeight
{
   Thin = 100,
   ExtraLight = 200,
   Light = 300,
   SemiLight = 350,
   Normal = 400,
   Medium = 500,
   SemiBold = 600,
   Bold = 700,
   ExtraBold = 800,
   Heavy = 900,
   ExtraBlack = 950,
};

enum class FontStyle
{
   Normal,
   Oblique,
   Italic,
};

enum class FontStretch
{
   Undefined,
   UltraCondensed,
   ExtraCondensed,
   Condensed,
   SemiCondensed,
   Normal,
   SemiExpanded,
   Expanded,
   ExtraExpanded,
   UltraExpanded,
};

class GRAPHICS_API FontInfo final
{
public:
   FontInfo() = default;
   FontInfo(const FontInfo&) = default;
   FontInfo(FontInfo&&) = default;
   FontInfo& operator=(const FontInfo&) = default;
   FontInfo& operator=(FontInfo&&) = default;
   ~FontInfo() = default;

   FontInfo(std::string_view faceName, float pointSize);

   FontInfo& SetFaceName(std::string_view faceName);
   std::string_view GetFaceName() const noexcept;

   FontInfo& SetPointSize(float pointSize) noexcept;
   float GetPointSize() const noexcept;

   FontInfo& SetFontWeight(FontWeight weight) noexcept;
   FontWeight GetFontWeight() const noexcept;

   FontInfo& SetFontStyle(FontStyle style) noexcept;
   FontStyle GetFontStyle() const noexcept;

   FontInfo& SetFontStretch(FontStretch stretch) noexcept;
   FontStretch GetFontStretch() const noexcept;

   FontInfo& SetUnderlined(bool underlined) noexcept;
   bool GetUnderlined() const noexcept;

   FontInfo& SetStrikethrough(bool strikethrough) noexcept;
   bool GetStrikethrough() const noexcept;

private:
   std::string mFaceName;
   float mPointSize { 0.0 };
   FontWeight mFontWeight { FontWeight::Normal };
   FontStyle mFontStyle { FontStyle::Normal };
   FontStretch mFontStretch { FontStretch::Normal };

   bool mUnderlined { false };
   bool mStrikethrough { false };
};

GRAPHICS_API bool operator==(const FontInfo& lhs, const FontInfo& rhs);
GRAPHICS_API bool operator!=(const FontInfo& lhs, const FontInfo& rhs);
GRAPHICS_API bool operator<(const FontInfo& lhs, const FontInfo& rhs);

} // namespace graphics
