/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  WXGraphicsContextPainter.cpp

  Dmitry Vedenko

**********************************************************************/
#include "WXGraphicsContextPainter.h"

#include <wx/graphics.h>
#include <wx/region.h>
#include <wx/dcgraph.h>
#include <wx/dcmemory.h>
#include <wx/window.h>

#include "graphics/RendererID.h"

#include "WXPainterUtils.h"

#include "WXColor.h"

namespace
{
auto rendererId = RegisterRenderer("wxGraphicsDefaultRenderer");

struct wxGraphicsContextPainterImage final : public PainterImage
{
   wxGraphicsContextPainterImage(Painter& painter, wxGraphicsRenderer& renderer, wxImage& img)
       : PainterImage(painter)
       , Bitmap(renderer.CreateBitmapFromImage(img))
       , Width(img.GetWidth())
       , Height(img.GetHeight())
       , HasAlpha(img.HasAlpha())
   {
   }

   wxGraphicsContextPainterImage(
      Painter& painter, const wxGraphicsContextPainterImage& rhs, uint32_t x,
      uint32_t y, uint32_t width, uint32_t height)
       : PainterImage(painter)
       , Bitmap(rhs.Bitmap.GetRenderer()->CreateSubBitmap(
            rhs.Bitmap, x, y, width, height))
       , Width(width)
       , Height(height)
       , HasAlpha(rhs.HasAlpha)
   {
   }

   wxGraphicsBitmap Bitmap;

   uint32_t Width;
   uint32_t Height;

   bool HasAlpha;

   uint32_t GetWidth() const override
   {
      return Width;
   }

   uint32_t GetHeight() const override
   {
      return Height;
   }
};

struct wxGraphicsContextPainterFont final : public PainterFont
{
   wxGraphicsContextPainterFont(
      Painter& painter, const wxFont& font)
       : PainterFont(painter)
       , Font(font)
       , FaceName(font.GetFaceName().ToUTF8().data())
       , FontSize(font.GetPixelSize().y)
   {
      wxMemoryDC dc;

      dc.SetFont(font);
      const auto dcMetrics = dc.GetFontMetrics();

      FontMetrics.Ascent = dcMetrics.ascent;
      FontMetrics.Descent = dcMetrics.descent;
      FontMetrics.Linegap = dcMetrics.externalLeading;
      FontMetrics.LineHeight = dcMetrics.height;
   }

   std::string_view GetFace() const override
   {
      return FaceName;
   }

   float GetFontSize() const override
   {
      throw FontSize;
   }

   Metrics GetFontMetrics() const override
   {
      return FontMetrics;
   }

   Size GetTextSize(const std::string_view& text) const override
   {
      return GetTextSize(nullptr, text);
   }

   Size GetTextSize(wxGraphicsContext* ctx, const std::string_view& text) const
   {
      if (ctx == nullptr)
      {
         MeasuringContext.reset(wxGraphicsContext::Create());
         ctx = MeasuringContext.get();
      }

      ctx->SetFont(Font, *wxBLACK);

      double width, height;

      ctx->GetTextExtent(
         wxString::FromUTF8(text.data(), text.size()), &width, &height);

      return { static_cast<float>(width), static_cast<float>(height) };
   }

   const wxGraphicsFont& GetGraphicsFont(wxGraphicsContext& ctx, Color color) const
   {
      if (CacheContext != &ctx)
      {
         CacheContext = &ctx;
         GraphicsFonts.clear();
      }

      auto it = GraphicsFonts.find(color.GetABGR());

      if (it != GraphicsFonts.end())
         return it->second;

      return GraphicsFonts.emplace(std::make_pair(
         color.GetABGR(), ctx.CreateFont(Font, wxColorFromColor(color)))).first->second;
   }
      
   wxFont Font;

   mutable std::unique_ptr<wxGraphicsContext> MeasuringContext;
   mutable wxGraphicsContext* CacheContext { nullptr };
   mutable std::unordered_map<uint32_t, wxGraphicsFont> GraphicsFonts;

   std::string FaceName;
   float FontSize;

   Metrics FontMetrics;
};
}

class WXGraphicsContextPainter::PaintTargetStack final
{
public:
   PaintTargetStack(wxGraphicsRenderer* renderer, wxWindow* window)
       : mRenderer(renderer)
       , mWindow(window)
       , mDC(nullptr)
   {
   }

   PaintTargetStack(wxGraphicsRenderer* renderer, wxDC* dc)
       : mRenderer(renderer)
       , mWindow(nullptr)
       , mDC(dc)
   {
   }

   PaintTargetStack(const PaintTargetStack&) = delete;
   PaintTargetStack(PaintTargetStack&&) = delete;
   PaintTargetStack& operator=(const PaintTargetStack&) = delete;
   PaintTargetStack& operator=(PaintTargetStack&&) = delete;

   void Push(wxGraphicsContextPainterImage& image)
   {
      mStack.emplace_back(std::make_unique<Surface>(
         *mRenderer, image.Width, image.Height, image.HasAlpha));
   }

   void Pop(wxGraphicsContextPainterImage& image)
   {     
      auto& currentItem = mStack.back();

      currentItem->GC = {};
      image.Bitmap = mRenderer->CreateBitmapFromImage(currentItem->Image);

      //currentItem->Image.SaveFile(wxString::Format(
      //   "C:\\devel\\temp\\audacity\\%lld_%d.png", time(nullptr), ++idx));

      mStack.pop_back();
   }

   bool BeginPaint()
   {
      if (mWindow)
      {
         mBaseContext.reset(mRenderer->CreateContext(mWindow));
      }
      else if (mDC)
      {
         mBaseContext.reset(mRenderer->CreateContext(mDC));
         mDC = nullptr;
      }
      else
      {
         assert(false);
      }

      return mBaseContext != nullptr;
   }

   void EndPaint()
   {
      mBaseContext.reset();
   }

   bool InPaintEnvent() const
   {
      return mBaseContext != nullptr || !mStack.empty();
   }

   wxGraphicsContext* GetCurrentContext() const
   {
      return mStack.empty() ? mBaseContext.get() : mStack.back()->GC.get();
   }

   wxGraphicsRenderer& GetRenderer() noexcept
   {
      return *mRenderer;
   }

   Size GetSize() const noexcept
   {
      if (InPaintEnvent())
      {
         auto ctx = GetCurrentContext();

         wxDouble width, height;
         ctx->GetSize(&width, &height);

         return { static_cast<float>(width), static_cast<float>(height) };
      }
      else if (mWindow)
      {
         const auto size = mWindow->GetSize();
         return { static_cast<float>(size.GetWidth()),
                  static_cast<float>(size.GetHeight()) };
      }
      else if (mDC)
      {
         int width, height;
         mDC->GetSize(&width, &height);
      }

      assert(false);
      return {};
   }

private:
   struct Surface final
   {
      Surface(wxGraphicsRenderer& renderer, uint32_t width, uint32_t height, bool alpha)
          : Image(width, height)
      {
         if (alpha)
         {
            Image.SetAlpha();
            
            std::memset(
               Image.GetAlpha(), 0, Image.GetWidth() * Image.GetHeight());
         }

         GC.reset(renderer.CreateContextFromImage(Image));
      }

      wxImage Image;
      std::unique_ptr<wxGraphicsContext> GC;
   };

   wxGraphicsRenderer* mRenderer;
   wxWindow* mWindow;
   wxDC* mDC;

   std::unique_ptr<wxGraphicsContext> mBaseContext;
   std::vector<std::unique_ptr<Surface>> mStack;
};

WXGraphicsContextPainter::WXGraphicsContextPainter(
   wxGraphicsRenderer* renderer, wxWindow* window, const wxFont& defaultFont)
    : mPaintTargetStack(std::make_unique<PaintTargetStack>(renderer, window))
    , mDefaultFont(std::make_shared<wxGraphicsContextPainterFont>(*this, defaultFont))
{

}

WXGraphicsContextPainter::WXGraphicsContextPainter(
   wxGraphicsRenderer* renderer, wxDC* dc, const wxFont& defaultFont)
    : mPaintTargetStack(std::make_unique<PaintTargetStack>(renderer, dc))
    , mDefaultFont(std::make_shared<wxGraphicsContextPainterFont>(
         *this, defaultFont))
{
}

WXGraphicsContextPainter::WXGraphicsContextPainter(
   wxGraphicsRenderer* renderer, const wxFont& defaultFont)
    : mPaintTargetStack(std::make_unique<PaintTargetStack>(renderer, static_cast<wxDC*>(nullptr)))
    , mDefaultFont(std::make_shared<wxGraphicsContextPainterFont>(
         *this, defaultFont))
{
}

WXGraphicsContextPainter::~WXGraphicsContextPainter()
{
}

Size WXGraphicsContextPainter::GetSize() const
{
   return mPaintTargetStack->GetSize();
}

RendererID WXGraphicsContextPainter::GetRendererID() const
{
   return rendererId;
}

std::shared_ptr<PainterFont> WXGraphicsContextPainter::CreateFont(
   const std::string_view& faceName, float pixelSize)
{
   wxFont font(
      int(pixelSize), wxFONTFAMILY_DEFAULT, wxFONTSTYLE_NORMAL,
      wxFONTWEIGHT_NORMAL, false,
      wxString::FromUTF8(faceName.data(), faceName.size()));

   return CreateFontFromWX(font);
}

std::shared_ptr<PainterFont>
WXGraphicsContextPainter::CreateFontFromWX(const wxFont& font)
{
   return std::make_shared<wxGraphicsContextPainterFont>(
      *this, font);
}

std::unique_ptr<PainterImage> WXGraphicsContextPainter::CreateImage(
   PainterImageFormat format, uint32_t width, uint32_t height, const void* data, const void* alphaData)
{
   wxImage image;

   if (data != nullptr)
   {
      switch (format)
      {
      case PainterImageFormat::RGB888:
         image.Create(
            width, height, static_cast<unsigned char*>(const_cast<void*>(data)),
            true);
         break;
      case PainterImageFormat::RGBA8888:
      {
         if (alphaData != nullptr)
         {
            image.Create(
               width, height,
               static_cast<unsigned char*>(const_cast<void*>(data)),
               static_cast<unsigned char*>(const_cast<void*>(alphaData)), true);
         }
         else
         {

            image.Create(width, height);
            image.SetAlpha();

            const unsigned char* u8Data =
               static_cast<const unsigned char*>(data);

            unsigned char* rgbPtr = image.GetData();
            unsigned char* alphaPtr = image.GetAlpha();

            for (uint32_t i = 0; i < width * height; ++i)
            {
               *rgbPtr++ = *u8Data++;
               *rgbPtr++ = *u8Data++;
               *rgbPtr++ = *u8Data++;
               *alphaPtr++ = *u8Data++;
            }
         }
      }
         break;
      default:
         assert(false);
         break;
      }
   }
   else
   {
      image.Create(width, height);

      if (format == PainterImageFormat::RGBA8888)
      {
         image.SetAlpha();
         std::memset(image.GetAlpha(), 0, width * height);
      }
   }

   return std::make_unique<wxGraphicsContextPainterImage>(
      *this, mPaintTargetStack->GetRenderer(), image);
}

std::unique_ptr<PainterImage> WXGraphicsContextPainter::GetSubImage(
   const PainterImage& image, uint32_t x, uint32_t y, uint32_t width, uint32_t height)
{
   if (GetRendererID() != image.GetRendererID())
      return {};

   return std::make_unique<wxGraphicsContextPainterImage>(
      *this, static_cast<const wxGraphicsContextPainterImage&>(image), x, y,
      width, height);
}

std::unique_ptr<PainterImage> WXGraphicsContextPainter::CreateDeviceImage(
   PainterImageFormat format, uint32_t width, uint32_t height)
{
   return CreateImage(format, width, height);
}

void WXGraphicsContextPainter::DoClear(const Rect& rect, Color color)
{
   if (!mPaintTargetStack->InPaintEnvent())
      return;

   auto context = mPaintTargetStack->GetCurrentContext();

   auto compostionMode = context->GetCompositionMode();
   context->SetCompositionMode(wxCOMPOSITION_SOURCE);

   UpdateBrush(color);
   
   context->DrawRectangle(
         rect.Origin.x, rect.Origin.y, rect.Size.width, rect.Size.height);

   UpdateBrush(GetCurrentBrush());

   context->SetCompositionMode(compostionMode);
}

void WXGraphicsContextPainter::Flush()
{
   if (!mPaintTargetStack->InPaintEnvent())
      return;

   mPaintTargetStack->GetCurrentContext()->Flush();
}

void WXGraphicsContextPainter::BeginPaint()
{
   assert(!mPaintTargetStack->InPaintEnvent());

   if (mPaintTargetStack->InPaintEnvent())
      return;

   mPaintTargetStack->BeginPaint();

   UpdateBrush(GetCurrentBrush());
   UpdatePen(GetCurrentPen());
   UpdateFont(*GetCurrentFont());
   UpdateAntiAliasingState(GetAntiAliasingEnabled());
   UpdateTransform(GetCurrentTransform());
   UpdateClipRect(GetCurrentClipRect());
}

void WXGraphicsContextPainter::EndPaint()
{
   mPaintTargetStack->EndPaint();
}

void WXGraphicsContextPainter::UpdateBrush(const Brush& brush)
{
   if (!mPaintTargetStack->InPaintEnvent())
      return;

   auto ctx = mPaintTargetStack->GetCurrentContext();
   
   if (brush.GetStyle() == BrushStyle::LinearGradient)
   {
      auto gradient = brush.GetGradientData();

      if (gradient != nullptr)
      {
         wxGraphicsGradientStops stops;

         for (auto stop : gradient->stops)
         {
            stops.Add(
               wxColour(
                  stop.color.GetRed(), stop.color.GetGreen(),
                  stop.color.GetBlue(), stop.color.GetAlpha()),
               stop.position);
         }
         
         ctx->SetBrush(ctx->CreateLinearGradientBrush(
            gradient->firstPoint.x, gradient->firstPoint.y,
            gradient->secondPoint.x, gradient->secondPoint.y, stops));
      }
      else
      {
         ctx->SetBrush(wxBrush());
      }
   }
   else
   {
      ctx->SetBrush(wxBrushFromBrush(brush));
   }
}

void WXGraphicsContextPainter::UpdatePen(const Pen& pen)
{
   if (!mPaintTargetStack->InPaintEnvent())
      return;

   mPaintTargetStack->GetCurrentContext()->SetPen(wxPenFromPen(pen));
}

void WXGraphicsContextPainter::UpdateTransform(const Transform& transform)
{
   if (!mPaintTargetStack->InPaintEnvent())
      return;

   auto ctx = mPaintTargetStack->GetCurrentContext();

   const wxGraphicsMatrix mtx = ctx->CreateMatrix(
      transform.GetScale().x, 0.0, 0.0, transform.GetScale().y,
      transform.GetTranslation().x, transform.GetTranslation().y);

   ctx->SetTransform(mtx);
}

void WXGraphicsContextPainter::UpdateClipRect(const Rect& rect)
{
   if (!mPaintTargetStack->InPaintEnvent())
      return;

   auto ctx = mPaintTargetStack->GetCurrentContext();

   ctx->ResetClip();
   if (std::isfinite(rect.Size.width) && std::isfinite(rect.Size.height))
   {
      ctx->Clip(
         rect.Origin.x, rect.Origin.y, rect.Size.width, rect.Size.height);
   }

   double x, y, w, h;
   ctx->GetClipBox(&x, &y, &w, &h);
}

bool WXGraphicsContextPainter::UpdateAntiAliasingState(bool enabled)
{
   if (!mPaintTargetStack->InPaintEnvent())
      return false;

   return mPaintTargetStack->GetCurrentContext()->SetAntialiasMode(
             enabled ? wxANTIALIAS_DEFAULT : wxANTIALIAS_NONE) ==
          wxANTIALIAS_DEFAULT;
}

size_t WXGraphicsContextPainter::BeginPath()
{
   if (!mPaintTargetStack->InPaintEnvent())
      return std::numeric_limits<size_t>::max();

   for (size_t i = 0; i < mPaths.size(); ++i)
   {
      if (mPaths[i] == nullptr)
      {
         mPaths[i] =
            std::make_unique<wxGraphicsPath>(mPaintTargetStack->GetCurrentContext()->CreatePath());

         return i;
      }
   }

   mPaths.emplace_back(std::make_unique<wxGraphicsPath>(mPaintTargetStack->GetCurrentContext()->CreatePath()));
   return mPaths.size() - 1;
}

void WXGraphicsContextPainter::MoveTo(size_t pathIndex, Point pt)
{
   if (pathIndex >= mPaths.size() || mPaths[pathIndex] == nullptr)
      return;

   mPaths[pathIndex]->MoveToPoint(pt.x, pt.y);
}

void WXGraphicsContextPainter::LineTo(size_t pathIndex, Point pt)
{
   if (pathIndex >= mPaths.size() || mPaths[pathIndex] == nullptr)
      return;

   mPaths[pathIndex]->AddLineToPoint(pt.x, pt.y);
}

void WXGraphicsContextPainter::AddRect(size_t pathIndex, const Rect& rect)
{
   if (pathIndex >= mPaths.size() || mPaths[pathIndex] == nullptr)
      return;

   mPaths[pathIndex]->AddRectangle(
      rect.Origin.x, rect.Origin.y, rect.Size.width, rect.Size.height);
}

void WXGraphicsContextPainter::EndPath(size_t pathIndex)
{
   if (pathIndex >= mPaths.size() || mPaths[pathIndex] == nullptr)
      return;

   mPaintTargetStack->GetCurrentContext()->FillPath(*mPaths[pathIndex]);
   mPaintTargetStack->GetCurrentContext()->StrokePath(*mPaths[pathIndex]);

   mPaths[pathIndex] = {};
}

void WXGraphicsContextPainter::DoDrawPolygon(const Point* pts, size_t count)
{
   if (!mPaintTargetStack->InPaintEnvent())
      return;

   mPoints.reserve(count + 1);

   for (size_t i = 0; i < count; ++i)
      mPoints.emplace_back(wxPoint2DDouble(pts[i].x, pts[i].y));

   mPoints.emplace_back(wxPoint2DDouble(pts[0].x, pts[0].y));

  mPaintTargetStack->GetCurrentContext()->DrawLines(mPoints.size(), mPoints.data());

  mPoints.clear();
}

void WXGraphicsContextPainter::DoDrawLines(const Point* pts, size_t count)
{
   if (!mPaintTargetStack->InPaintEnvent())
      return;

   assert(count % 2 == 0);

   if (count == 2)
   {
      mPaintTargetStack->GetCurrentContext()->StrokeLine(pts[0].x, pts[0].y, pts[1].x, pts[1].y);
   }
   else
   {
      mPoints.reserve(count / 2);
      mEndPoints.reserve(count / 2);

      for (size_t i = 0; i < count / 2; ++i)
      {
         mPoints.emplace_back(wxPoint2DDouble(pts[2 * i].x, pts[2 * i].y));
         mEndPoints.emplace_back(
            wxPoint2DDouble(pts[2 * i + 1].x, pts[2 * i + 1].y));
      }

      mPaintTargetStack->GetCurrentContext()->StrokeLines(
         mPoints.size(), mPoints.data(), mEndPoints.data());

      mPoints.clear();
      mEndPoints.clear();
   }
}

void WXGraphicsContextPainter::DoDrawRect(const Rect& rect)
{
   if (!mPaintTargetStack->InPaintEnvent())
      return;

   mPaintTargetStack->GetCurrentContext()->DrawRectangle(
      rect.Origin.x, rect.Origin.y, rect.Size.width, rect.Size.height);
}

void WXGraphicsContextPainter::DoDrawEllipse(const Rect& rect)
{
   if (!mPaintTargetStack->InPaintEnvent())
      return;

   mPaintTargetStack->GetCurrentContext()->DrawEllipse(
      rect.Origin.x, rect.Origin.y, rect.Size.width, rect.Size.height);
}

void WXGraphicsContextPainter::DoDrawText(
   Point origin, const PainterFont& font, Brush backgroundBrush,
   const std::string_view& text)
{
   if (!mPaintTargetStack->InPaintEnvent())
      return;

   if (font.GetRendererID() != GetRendererID())
      return;

   wxGraphicsContext* currentContext = mPaintTargetStack->GetCurrentContext();

   currentContext->SetFont(
      static_cast<const wxGraphicsContextPainterFont&>(font).GetGraphicsFont(
         *currentContext, GetCurrentBrush().GetColor()));

   const wxGraphicsBrush bgBrush =
      backgroundBrush.GetStyle() != BrushStyle::None ?
         currentContext->CreateBrush(wxBrushFromBrush(backgroundBrush)) :
         wxNullGraphicsBrush;
   
   currentContext->DrawText(
      wxString::FromUTF8(text.data(), text.size()), origin.x, origin.y, bgBrush);
}

void WXGraphicsContextPainter::DoDrawRotatedText(
   Point origin, float angle, const PainterFont& font, Brush backgroundBrush,
   const std::string_view& text)
{
   if (font.GetRendererID() != GetRendererID())
      return;

   wxGraphicsContext* currentContext = mPaintTargetStack->GetCurrentContext();

   currentContext->SetFont(
      static_cast<const wxGraphicsContextPainterFont&>(font).GetGraphicsFont(
         *currentContext, GetCurrentBrush().GetColor()));

   const wxGraphicsBrush bgBrush =
      backgroundBrush.GetStyle() != BrushStyle::None ?
         currentContext->CreateBrush(wxBrushFromBrush(backgroundBrush)) :
         wxNullGraphicsBrush;

   currentContext->DrawText(
      wxString::FromUTF8(text.data(), text.size()), origin.x, origin.y, angle,
      bgBrush);
}

void WXGraphicsContextPainter::DoDrawImage(
   const PainterImage& painterImage, const Rect& rect, const Rect& imageRect)
{
   if (!mPaintTargetStack->InPaintEnvent())
      return;

   if (painterImage.GetRendererID() != GetRendererID())
      return;

   if (rect.Size.IsZero() || imageRect.Size.IsZero())
      return;

   wxGraphicsContext* currentContext = mPaintTargetStack->GetCurrentContext();

   const auto& image =
      static_cast<const wxGraphicsContextPainterImage&>(painterImage);
   
   auto gcImage = image.Bitmap;

   if (
      !imageRect.Origin.IsZero() || uint32_t(imageRect.Size.width) != image.Width ||
      uint32_t(imageRect.Size.height) != image.Height)
   {
      gcImage = currentContext->CreateSubBitmap(
         gcImage, imageRect.Origin.x, imageRect.Origin.y, imageRect.Size.width,
         imageRect.Size.height);
   }
   
   currentContext->DrawBitmap(
      gcImage, rect.Origin.x, rect.Origin.y, rect.Size.width, rect.Size.height);
}

std::shared_ptr<PainterFont> WXGraphicsContextPainter::GetDefaultFont() const
{
   return mDefaultFont;
}

Size WXGraphicsContextPainter::DoGetTextSize(
   const PainterFont& font, const std::string_view& text) const
{
   if (font.GetRendererID() != GetRendererID())
       font.GetTextSize(text);

   return static_cast<const wxGraphicsContextPainterFont&>(font).GetTextSize(
      mPaintTargetStack->GetCurrentContext(), text);
}

void WXGraphicsContextPainter::UpdateFont(const PainterFont&)
{

}

void WXGraphicsContextPainter::DoDrawRoundedRect(const Rect& rect, float radius)
{
   if (!mPaintTargetStack->InPaintEnvent())
      return;

   mPaintTargetStack->GetCurrentContext()->DrawRoundedRectangle(
      rect.Origin.x, rect.Origin.y, rect.Size.width, rect.Size.height, radius);
}

void WXGraphicsContextPainter::PushPaintTarget(PainterImage& image)
{
   if (image.GetRendererID() != GetRendererID())
      return;

   mPaintTargetStack->Push(static_cast<wxGraphicsContextPainterImage&>(image));

   UpdatePen(GetCurrentPen());
   UpdateBrush(GetCurrentBrush());
   UpdateFont(*GetCurrentFont());
   UpdateAntiAliasingState(GetAntiAliasingEnabled());
}

void WXGraphicsContextPainter::PopPaintTarget(PainterImage& image)
{
   mPaintTargetStack->Pop(static_cast<wxGraphicsContextPainterImage&>(image));
}

RendererID WXGraphicsContextPainterRendererID()
{
   return rendererId;
}
