/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  WXColor.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include "graphics/Painter.h"

#include <wx/brush.h>
#include <wx/pen.h>

class wxFont;

GRAPHICS_WX_API Pen PenFromWXPen(const wxPen& pen) noexcept;
GRAPHICS_WX_API Brush BrushFromWXBrush(const wxBrush& brush) noexcept;

GRAPHICS_WX_API wxPen wxPenFromPen(const Pen& pen) noexcept;
GRAPHICS_WX_API wxBrush wxBrushFromBrush(const Brush& brush) noexcept;

GRAPHICS_WX_API Rect RectFromWXRect(const wxRect& rect) noexcept;
