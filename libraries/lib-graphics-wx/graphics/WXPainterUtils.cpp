#include "WXColor.h"
/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  WXColor.cpp

  Dmitry Vedenko

**********************************************************************/

#include "WXColor.h"
#include "WXPainterUtils.h"

#include "WXGraphicsContextPainter.h"

#include <cassert>

namespace
{
PenStyle GetPenStyle(wxPenStyle style) noexcept
{
   switch (style)
   {
   case wxPENSTYLE_INVALID:
   case wxPENSTYLE_TRANSPARENT:
      return PenStyle::None;
   case wxPENSTYLE_SOLID:
      return PenStyle::Solid;
   case wxPENSTYLE_DOT:
      return PenStyle::Dot;
   case wxPENSTYLE_LONG_DASH:
      return PenStyle::LongDash;
   case wxPENSTYLE_SHORT_DASH:
      return PenStyle::ShortDash;
   case wxPENSTYLE_DOT_DASH:
      return PenStyle::DotDash;
   default:
      assert(false);
      return PenStyle::Solid;
   }
}
wxPenStyle GetPenStyle(PenStyle style) noexcept
{
   switch (style)
   {
   case PenStyle::None:
      return wxPENSTYLE_TRANSPARENT;
   case PenStyle::Solid:
      return wxPENSTYLE_SOLID;
   case PenStyle::Dot:
      return wxPENSTYLE_DOT;
   case PenStyle::LongDash:
      return wxPENSTYLE_LONG_DASH;
   case PenStyle::ShortDash:
      return wxPENSTYLE_SHORT_DASH;
   case PenStyle::DotDash:
      return wxPENSTYLE_DOT_DASH;
   default:
      assert(false);
      return wxPENSTYLE_INVALID;
   }
}
}

GRAPHICS_WX_API Pen PenFromWXPen(const wxPen& pen) noexcept
{
   return Pen(GetPenStyle(pen.GetStyle()), ColorFromWXPen(pen), pen.GetWidth());
}

GRAPHICS_WX_API Brush BrushFromWXBrush(const wxBrush& brush) noexcept
{
   return Brush(BrushStyle::Solid, ColorFromWXBrush(brush));
}

wxPen wxPenFromPen(const Pen& pen) noexcept
{
   return pen.GetStyle() != PenStyle::None ?
             wxPen(
                wxColorFromColor(pen.GetColor()), pen.GetWidth(),
                GetPenStyle(pen.GetStyle())) :
             wxNullPen;
}

wxBrush wxBrushFromBrush(const Brush& brush) noexcept
{
   return brush.GetStyle() == BrushStyle::Solid ?
             wxBrush(wxColorFromColor(brush.GetColor())) :
             wxNullBrush;
}

GRAPHICS_WX_API std::shared_ptr<PainterFont>
FontFromWXFont(Painter& painter, const wxFont& font)
{
   if (painter.GetRendererID() == WXGraphicsContextPainterRendererID())
   {
      auto& concretePainter = static_cast<WXGraphicsContextPainter&>(painter);
      return concretePainter.CreateFontFromWX(font);
   }

   assert(false);
   return painter.GetDefaultFont();
}

Rect RectFromWXRect(const wxRect& rect) noexcept
{
   return Rect {
      Point { static_cast<float>(rect.x), static_cast<float>(rect.y) },
      Size { static_cast<float>(rect.width), static_cast<float>(rect.height) }
   };
}
