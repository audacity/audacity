/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  WXPainterFactory.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <memory>

class wxWindow;
class wxWindowDC;
class wxMemoryDC;
class wxPrinterDC;
class wxDC;

namespace graphics
{
class Painter;
}

namespace graphics::wx
{   

GRAPHICS_WX_API std::unique_ptr<Painter> CreatePainter(wxWindow* wnd);
GRAPHICS_WX_API std::unique_ptr<Painter> CreatePainterFromDC(wxDC& dc);

GRAPHICS_WX_API Painter& GetMeasuringPainter();

GRAPHICS_WX_API void ShutdownRenderingSystem();

} // namespace graphics::wx
