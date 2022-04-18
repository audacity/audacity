/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  WXPainterFactory.h

  Dmitry Vedenko

**********************************************************************/
#include "WXPainterFactory.h"

#include <wx/graphics.h>
#include <wx/window.h>

#include <wx/dcclient.h>
#include <wx/dcmemory.h>
#include <wx/dcprint.h>

#include "WXGraphicsContextPainter.h"

namespace
{
wxGraphicsRenderer* GetRenderer()
{
   return wxGraphicsRenderer::GetDirect2DRenderer();
}
}

std::unique_ptr<Painter> CreatePainter(wxWindow* wnd)
{
   return std::make_unique<WXGraphicsContextPainter>(
      GetRenderer()->CreateContext(wnd), wnd->GetFont());
}

std::unique_ptr<Painter> CreatePainter(wxWindowDC& dc)
{
   return std::make_unique<WXGraphicsContextPainter>(
      GetRenderer()->CreateContext(dc), dc.GetFont());
}

std::unique_ptr<Painter> CreatePainter(wxMemoryDC& dc)
{
   return std::make_unique<WXGraphicsContextPainter>(
      GetRenderer()->CreateContext(dc), dc.GetFont());
}

std::unique_ptr<Painter> CreatePainter(wxPrinterDC& dc)
{
   return std::make_unique<WXGraphicsContextPainter>(
      GetRenderer()->CreateContext(dc), dc.GetFont());
}

std::unique_ptr<Painter> CreatePainter(wxDC& dc)
{
   return std::make_unique<WXGraphicsContextPainter>(
      GetRenderer()->CreateContextFromUnknownDC(dc), dc.GetFont());
}

Painter& GetMeasuringPainter()
{
   static auto context = std::make_unique<WXGraphicsContextPainter>(
      GetRenderer()->CreateMeasuringContext(), *wxNORMAL_FONT);

   return *context;
}
