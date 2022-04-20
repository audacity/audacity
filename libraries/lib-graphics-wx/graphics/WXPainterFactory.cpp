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
   //return wxGraphicsRenderer::GetDefaultRenderer();
   return wxGraphicsRenderer::GetDirect2DRenderer();
}
}

std::unique_ptr<Painter> CreatePainter(wxWindow* wnd)
{
   return std::make_unique<WXGraphicsContextPainter>(
      GetRenderer(), wnd, wnd->GetFont());
}

std::unique_ptr<Painter> CreatePainterFromDC(wxDC& dc)
{
   return std::make_unique<WXGraphicsContextPainter>(
      GetRenderer(), &dc, dc.GetFont());
}

Painter& GetMeasuringPainter()
{
   static auto context = std::make_unique<WXGraphicsContextPainter>(
      GetRenderer(), *wxNORMAL_FONT);

   return *context;
}
