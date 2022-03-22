/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  WXColor.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include "graphics/Color.h"

class wxColour;
class wxPen;
class wxBrush;

GRAPHICS_WX_API Color ColorFromWXColor(const wxColour& color) noexcept;
GRAPHICS_WX_API Color ColorFromWXPen(const wxPen& pen) noexcept;
GRAPHICS_WX_API Color ColorFromWXBrush(const wxBrush& brush) noexcept;
