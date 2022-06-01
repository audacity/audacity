/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  WXFontUtils.h

  Dmitry Vedenko

**********************************************************************/
#include "WXFontUtils.h"
#include "WXGraphicsContextPainter.h"
#include "CodeConversions.h"

namespace graphics::wx
{

std::shared_ptr<PainterFont>
FontFromWXFont(Painter& painter, const wxFont& font)
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
std::string GetFaceForFontFamily(const wxFont& font)
{
   const auto family = font.GetFamily();

#if defined(__WXMAC__)
   switch (family)
   {
   case wxFONTFAMILY_DEFAULT:
      return "Lucida Grande";
   case wxFONTFAMILY_SCRIPT:
   case wxFONTFAMILY_ROMAN:
   case wxFONTFAMILY_DECORATIVE:
      return "Times";
   case wxFONTFAMILY_SWISS:
      return "Helvetica";
   case wxFONTFAMILY_MODERN:
   case wxFONTFAMILY_TELETYPE:
      return "Courier";
   default:
      return "Times";
   }
#elif defined(__WXMSW__)
   switch (family)
   {
   case wxFONTFAMILY_DECORATIVE:
      return "Old English";
   case wxFONTFAMILY_ROMAN:
      return "MS Serif";
   case wxFONTFAMILY_MODERN:
   case wxFONTFAMILY_TELETYPE:
      return "Courier New";
   case wxFONTFAMILY_DEFAULT:
   case wxFONTFAMILY_SWISS:
      return "MS Sans Serif";
   case wxFONTFAMILY_SCRIPT:
      return "Script";
   default:
      return "MS Sans Serif";
   }
#else
   switch (family)
   {
   case wxFONTFAMILY_DECORATIVE:
      return "lucida";
   case wxFONTFAMILY_ROMAN:
      return "times";
   case wxFONTFAMILY_MODERN:
      return "courier";
   case wxFONTFAMILY_DEFAULT:
   case wxFONTFAMILY_SWISS:
      return "helvetica";
   case wxFONTFAMILY_TELETYPE:
      return "lucidatypewriter";
   case wxFONTFAMILY_SCRIPT:
      return "utopia";
   default:
      return "times";
   }
#endif
}

std::string GetFaceName(const wxFont& font)
{
   if (!font.GetFaceName().empty())
      return audacity::ToUTF8(font.GetFaceName());

   return GetFaceForFontFamily(font);
}

} // namespace

FontInfo FontInfoFromWXFont(const wxFont& font)
{
   if (!font.IsOk())
      return {};

   return FontInfo(GetFaceName(font),
             font.GetFractionalPointSize())
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
} // namespace graphics::wx
