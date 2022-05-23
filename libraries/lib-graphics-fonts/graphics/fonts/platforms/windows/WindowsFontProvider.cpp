/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  WindowsFontProvider.h

  Dmitry Vedenko

**********************************************************************/
#include "WindowsFontProvider.h"

#include <cstdint>
#include <string>

#include "graphics/fonts/FontLibrary.h"

#include "CodeConversions.h"
#include "MemoryX.h"

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

namespace graphics::fonts::platforms::windows
{
namespace
{
   
auto val = ([]() { FontLibrary::SetFontProvider(std::make_unique<WindowsFontProvider>()); return true; })();

}


std::unique_ptr<graphics::fonts::FontStream>
WindowsFontProvider::GetFontStream(const FontInfo& fontInfo)
{
   // Face request is always performed without the size
   // WinAPI does not provide a way to get path to the font face by name and size
   // However, we can get the font face data

   LOGFONTW lf = {};

   lf.lfWeight = static_cast<LONG>(fontInfo.GetFontWeight());
   lf.lfItalic = fontInfo.GetFontStyle() == FontStyle::Italic;

   std::wstring faceName = audacity::ToWString(fontInfo.GetFaceName());
   
   std::memcpy(
      lf.lfFaceName, faceName.c_str(),
      std::min<size_t>(LF_FACESIZE, faceName.size()) * sizeof(wchar_t));

   HFONT font = ::CreateFontIndirectW(&lf);

   if (font == nullptr)
      return nullptr;

   auto fontDeleter = finally([font]() { DeleteObject(font); });
   
   HDC hdc = ::CreateCompatibleDC(NULL);

   if (!hdc)
      return nullptr;

   auto dcDeleter = finally([hdc]() { DeleteDC(hdc); });

   SelectObject(hdc, font);
   
   const size_t size = ::GetFontData(hdc, 0, 0, nullptr, 0);

   std::vector<unsigned char> data(size);

   if (::GetFontData(hdc, 0, 0, data.data(), size) != size)
      return nullptr;

   return std::make_unique<MemoryFontStream>(std::move(data));
}

} // namespace graphics::fonts::platforms::windows
