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

#ifdef WIN32
#  include "graphics/d2d/D2DRenderer.h"
#endif

#include "graphics/gl/GLRenderer.h"

#include "WXFontUtils.h"

namespace
{

class RendererProvider /* not final */
{
public:
   virtual ~RendererProvider() = default;

   virtual std::unique_ptr<Painter> CreatePainterFromWindow(wxWindow& wnd) = 0;
   virtual std::unique_ptr<Painter> CreatePainterFromDC(wxDC& dc) = 0;

   virtual Painter& GetMeasuringPainter() = 0;
};

class wxGraphicsRendererProvider final : public RendererProvider
{
public:
   ~wxGraphicsRendererProvider()
   {
      GetWXGraphicsRendererShutdownPublisher().Publish({});
   }

   wxGraphicsRenderer* GetRenderer() const
   {
      return wxGraphicsRenderer::GetDefaultRenderer();
   }

   std::unique_ptr<Painter>
   CreatePainterFromWindow(wxWindow& wnd) override
   {
      return std::make_unique<WXGraphicsContextPainter>(
         GetRenderer(), &wnd, wnd.GetFont());
   }

   std::unique_ptr<Painter>
   CreatePainterFromDC(wxDC& dc) override
   {
      auto font = dc.GetFont();

      return std::make_unique<WXGraphicsContextPainter>(
         GetRenderer(), &dc, font.IsOk() ? font : *wxNORMAL_FONT);
   }

   Painter& GetMeasuringPainter() override
   {
      if (!mMeasuringPainter)
      {
         mMeasuringPainter = std::make_unique<WXGraphicsContextPainter>(
            GetRenderer(), *wxNORMAL_FONT);
      }

      return *mMeasuringPainter;
   }

private:
   std::unique_ptr<Painter> mMeasuringPainter;
};

class OpenGLRendererProvider final : public RendererProvider
{
public:
   ~OpenGLRendererProvider()
   {
      graphics::gl::ShutdownSharedRenderer();
   }
   std::unique_ptr<Painter> CreatePainterFromWindow(wxWindow& wnd) override
   {
      return graphics::gl::GetSharedRenderer().CreateWindowPainter(
         wnd.GetHandle(), FontInfoFromWXFont(wnd.GetFont()));
   }

   std::unique_ptr<Painter> CreatePainterFromDC(wxDC& dc) override
   {
      return nullptr;
   }

   Painter& GetMeasuringPainter() override
   {
      if (!mMeasuringPainter)
      {
         mMeasuringPainter =
            graphics::gl::GetSharedRenderer().CreateMeasuringPainter(
               FontInfoFromWXFont(*wxNORMAL_FONT));
      }

      return *mMeasuringPainter;
   }

private:
   std::unique_ptr<Painter> mMeasuringPainter;
};

#ifdef WIN32
class D2DRendererProvider : public RendererProvider
{
public:
   ~D2DRendererProvider()
   {
      SharedD2DRenderer().Shutdown();
   }

   std::unique_ptr<Painter> CreatePainterFromWindow(wxWindow& wnd) override
   {
      return SharedD2DRenderer().CreateHWNDPainter(
         wnd.GetHWND(), FontInfoFromWXFont(wnd.GetFont()));
   }

   std::unique_ptr<Painter> CreatePainterFromDC(wxDC& dc) override
   {
      auto font = dc.GetFont();

      return SharedD2DRenderer().CreateHDCPainter(
         dc.GetHDC(), FontInfoFromWXFont(font.IsOk() ? font : *wxNORMAL_FONT));
   }

   Painter& GetMeasuringPainter() override
   {
      if (!mMeasuringPainter)
         mMeasuringPainter = SharedD2DRenderer().CreateMeasuringPainter(
            FontInfoFromWXFont(*wxNORMAL_FONT));

      return *mMeasuringPainter;
   }

private:
   std::unique_ptr<Painter> mMeasuringPainter;
};
#endif

std::unique_ptr<RendererProvider> Provider;

enum class RendererType
{
   Auto,
   D2D,
   GL,
   Fallback,
};

RendererProvider& GetRendererProvider(RendererType type)
{
   if (Provider != nullptr)
      return *Provider;

   if (Provider == nullptr && (type == RendererType::Auto || type == RendererType::GL))
   {
      auto& openGLRenderer = graphics::gl::GetSharedRenderer();

      if (openGLRenderer.IsAvailable())
         Provider = std::make_unique<OpenGLRendererProvider>();
   }

#ifdef WIN32
   if (Provider == nullptr && (type == RendererType::Auto || type == RendererType::D2D))
   {
      auto& d2dRenderer = SharedD2DRenderer();

      if (d2dRenderer.IsAvailable())
         Provider = std::make_unique<D2DRendererProvider>();
   }
#endif

   if (Provider == nullptr)
      Provider = std::make_unique<wxGraphicsRendererProvider>();

   return *Provider;
}

RendererType GetPreferredRenderType() noexcept
{
   return RendererType::Auto;
}
}

std::unique_ptr<Painter> CreatePainter(wxWindow* wnd)
{
   if (wnd == nullptr)
      return {};

   wnd->SetBackgroundStyle(wxBG_STYLE_PAINT);

   return GetRendererProvider(GetPreferredRenderType())
      .CreatePainterFromWindow(*wnd);
}

std::unique_ptr<Painter> CreatePainterFromDC(wxDC& dc)
{
   return GetRendererProvider(GetPreferredRenderType()).CreatePainterFromDC(dc);
}

Painter& GetMeasuringPainter()
{
   return GetRendererProvider(GetPreferredRenderType()).GetMeasuringPainter();
}

void ShutdownRenderingSystem()
{
   Provider.reset();
}
