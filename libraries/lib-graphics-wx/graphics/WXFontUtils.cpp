/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  WXFontUtils.h

  Dmitry Vedenko

**********************************************************************/
#include "WXFontUtils.h"
#include "WXGraphicsContextPainter.h"
#include "CodeConversions.h"

class Painter;

std::shared_ptr<PainterFont> FontFromWXFont(Painter& painter, const wxFont& font)
{
   if (painter.GetRendererID() == WXGraphicsContextPainterRendererID())
   {
      auto& concretePainter = static_cast<WXGraphicsContextPainter&>(painter);
      return concretePainter.CreateFontFromWX(font);
   }

   return painter.CreateFont(FontInfoFromWXFont(font));
}

namespace
{
FontStyle GetFontStyle(const wxFont& font)
{
   switch (font.GetStyle())
   {
   case wxFONTSTYLE_NORMAL:
      return FontStyle::Normal;
   case wxFONTSTYLE_ITALIC:
      return FontStyle::Italic;
   case wxFONTSTYLE_SLANT:
      return FontStyle::Oblique;
   default:
      return FontStyle::Normal;
   }
}
} // namespace

FontInfo FontInfoFromWXFont(const wxFont& font)
{
   return FontInfo(audacity::ToUTF8(font.GetFaceName()), font.GetFractionalPointSize())
      .SetFontWeight(FontWeight(font.GetWeight()))
      .SetFontStyle(GetFontStyle(font))
      .SetUnderlined(font.GetUnderlined())
      .SetStrikethrough(font.GetStrikethrough());
}

wxFont wxFontFromFontInfo(const FontInfo& fontInfo)
{
   return wxFont(wxFontInfo(fontInfo.GetPointSize())
                    .FaceName(audacity::ToWXString(fontInfo.GetFaceName()))
                    .Weight(wxFontInfo::GetWeightClosestToNumericValue(
                       size_t(fontInfo.GetFontWeight())))
                    .Italic(fontInfo.GetFontStyle() != FontStyle::Normal)
                    .Underlined(fontInfo.GetUnderlined())
                    .Strikethrough(fontInfo.GetStrikethrough()));
}
