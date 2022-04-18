/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  WXPainterFactory.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <memory>

class Painter;
class wxWindow;
class wxWindowDC;
class wxMemoryDC;
class wxPrinterDC;
class wxDC;

GRAPHICS_WX_API std::unique_ptr<Painter> CreatePainter(wxWindow* wnd);
GRAPHICS_WX_API std::unique_ptr<Painter> CreatePainter(wxWindowDC& dc);
GRAPHICS_WX_API std::unique_ptr<Painter> CreatePainter(wxMemoryDC& dc);
GRAPHICS_WX_API std::unique_ptr<Painter> CreatePainter(wxPrinterDC& dc);
GRAPHICS_WX_API std::unique_ptr<Painter> CreatePainter(wxDC& dc);

GRAPHICS_WX_API Painter& GetMeasuringPainter();
