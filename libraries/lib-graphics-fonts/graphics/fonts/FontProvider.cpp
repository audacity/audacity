/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  FontProvider.cpp

  Dmitry Vedenko

**********************************************************************/
#include "FontProvider.h"

#include <cstring>

namespace graphics::fonts
{
FontProvider::~FontProvider() noexcept
{
}

FontStream::~FontStream() noexcept
{
}

MemoryFontStream::MemoryFontStream(std::vector<uint8_t> data)
    : mData(std::move(data))
{
}

unsigned long MemoryFontStream::StreamRead(
   unsigned long offset, unsigned char* buffer, unsigned long count)
{
   if (count == 0)
      return offset < mData.size() ? 0 : 1;

   if (offset >= mData.size())
      return 0;

   if (offset + count > mData.size())
      count = mData.size() - offset;

   std::memcpy(buffer, mData.data() + offset, count);

   return count;
}
} // namespace graphics::fonts
