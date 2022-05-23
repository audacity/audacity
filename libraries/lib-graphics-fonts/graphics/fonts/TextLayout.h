/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  TextLayout.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <cstdint>
#include <vector>

namespace graphics::fonts
{
struct GRAPHICS_FONTS_API TextLayoutSymbol final
{
   uint32_t codepoint { 0 };
   uint32_t cluster { 0 };

   int32_t x { 0 };
   int32_t y { 0 };
}; // struct TextLayoutSymbol

class GRAPHICS_FONTS_API TextLayout final
{
public:
   TextLayout(
      std::vector<TextLayoutSymbol> symbols, uint32_t width, uint32_t height);
   
   TextLayout(const TextLayout&) = delete;
   TextLayout(TextLayout&&) = delete;
   TextLayout& operator=(const TextLayout&) = delete;
   TextLayout& operator=(TextLayout&&) = delete;

   const std::vector<TextLayoutSymbol>& GetSymbols() const noexcept;
   
   uint32_t GetWidth() const noexcept;
   uint32_t GetHeight() const noexcept;

   uint32_t GetSymbolOffset(size_t symbolIndex) const noexcept;

private:
   std::vector<TextLayoutSymbol> mSymbols;

   uint32_t mWidth { 0 };
   uint32_t mHeight { 0 };
}; // class TextLayout
} // namespace graphics::fonts
