/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  FontProvider.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <cstdint>
#include <memory>
#include <vector>

class FontInfo;

namespace graphics::fonts
{
class GRAPHICS_FONTS_API FontStream /* not final */
{
public:
   virtual ~FontStream() noexcept;

   virtual unsigned long StreamRead(
      unsigned long offset, unsigned char* buffer, unsigned long count) = 0;
};

class GRAPHICS_FONTS_API MemoryFontStream : public FontStream
{
public:
   explicit MemoryFontStream(std::vector<unsigned char> data);
   
   unsigned long StreamRead(
      unsigned long offset, unsigned char* buffer,
      unsigned long count) override;

private:
   std::vector<unsigned char> mData;
};

class GRAPHICS_FONTS_API FontProvider /* not final*/
{
public:
   virtual ~FontProvider() noexcept;

   virtual std::unique_ptr<FontStream> GetFontStream(const FontInfo& fontInfo) = 0;
}; // class FontProvider
} // namespace graphics::fonts
