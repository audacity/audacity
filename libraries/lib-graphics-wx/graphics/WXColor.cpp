#include "WXColor.h"
/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  WXColor.cpp

  Dmitry Vedenko

**********************************************************************/

#include <wx/brush.h>
#include <wx/colour.h>
#include <wx/pen.h>

Color ColorFromWXColor(const wxColour& color) noexcept
{
   return Color(color.Red(), color.Green(), color.Blue(), color.Alpha());
}

Color ColorFromWXPen(const wxPen& pen) noexcept
{
   return ColorFromWXColor(pen.GetColour());
}

Color ColorFromWXBrush(const wxBrush& brush) noexcept
{
   return ColorFromWXColor(brush.GetColour());
}
