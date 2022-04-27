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
#include "WXFontUtils.h"
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

   void Push(std::shared_ptr<wxGraphicsContextPainterImage> image)
   {
      mPaintTargetsStack.emplace_back(std::make_unique<Surface>(
         *mRenderer, image->Width, image->Height, image->HasAlpha));
   }

   void Pop(std::shared_ptr<wxGraphicsContextPainterImage> image)
   {     
      auto& currentItem = mPaintTargetsStack.back();

      currentItem->GC = {};
      image->Bitmap = mRenderer->CreateBitmapFromImage(currentItem->Image);

      mPaintTargetsStack.pop_back();
   }

   bool BeginPaint()
   {
      assert(mPaintTargetsStack.empty());

      if (!mPaintTargetsStack.empty())
         return false;
      
      if (mWindow)
      {
         mPaintTargetsStack.emplace_back(
            std::make_unique<Surface>(*mRenderer, mWindow));
      }
      else if (mDC)
      {
         mPaintTargetsStack.emplace_back(
            std::make_unique<Surface>(*mRenderer, mDC));
         
         mDC = nullptr;
      }
      else
      {
         assert(false);
      }

      return !mPaintTargetsStack.empty();
   }

   void EndPaint()
   {
      assert(mPaintTargetsStack.size() == 1);
      mPaintTargetsStack.clear();
   }

   bool InPaintEnvent() const
   {
      return !mPaintTargetsStack.empty();
   }

   wxGraphicsContext* GetCurrentContext() const
   {
      return !mPaintTargetsStack.empty() ? mPaintTargetsStack.back()->GC.get() :
                                           nullptr;
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

   bool IsSamePen(const Pen& pen) const
   {
      return !mPaintTargetsStack.empty() &&
             mPaintTargetsStack.back()->CurrentPen == pen;
   }

   void SetPen(const Pen& pen)
   {
      if (!mPaintTargetsStack.empty())
         mPaintTargetsStack.back()->SetPen(pen);
   }

   void SetBrush(const Brush& brush)
   {
      if (!mPaintTargetsStack.empty())
         mPaintTargetsStack.back()->SetBrush(brush);
   }

   void SetFont(const std::shared_ptr<wxGraphicsContextPainterFont>& font)
   {
      if (!mPaintTargetsStack.empty())
         mPaintTargetsStack.back()->SetFont(font);
   }

   void UpdateFont()
   {
      if (!mPaintTargetsStack.empty())
         mPaintTargetsStack.back()->UpdateFont();
   }

   void SetTransform(const Transform& transform)
   {
      if (!mPaintTargetsStack.empty())
         mPaintTargetsStack.back()->SetTransform(transform);
   }

   void SetClipRect(const Rect& rect)
   {
      if (!mPaintTargetsStack.empty())
         mPaintTargetsStack.back()->SetClipRect(rect);
   }

   bool SetAntialiasing(bool enabled)
   {
      if (!mPaintTargetsStack.empty())
         return mPaintTargetsStack.back()->SetAntialiasing(enabled);

      return false;
   }

private:
   struct Surface final
   {
      Surface(wxGraphicsRenderer& renderer, wxWindow* window)
          : GC(renderer.CreateContext(window))
      {
         InitGCState();
      }

      Surface(wxGraphicsRenderer& renderer, wxDC* dc)
          : GC(renderer.CreateContext(dc))
      {
         InitGCState();
      }
      
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

         InitGCState();
      }

      void InitGCState()
      {
         GC->SetPen(wxPenFromPen(CurrentPen));
         GC->SetBrush(wxBrushFromBrush(CurrentBrush));
         
         CurrentAntialiasing =
            GC->SetAntialiasMode(
               CurrentAntialiasing ? wxANTIALIAS_DEFAULT : wxANTIALIAS_NONE) &&
            CurrentAntialiasing;
      }

      void SetPen(const Pen& pen)
      {
         if (CurrentPen != pen)
         {
            CurrentPen = pen;
            GC->SetPen(wxPenFromPen(pen));
         }
      }

      void SetBrush(const Brush& brush)
      {
         if (CurrentBrush != brush)
         {
            CurrentBrush = brush;

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

                  GC->SetBrush(GC->CreateLinearGradientBrush(
                     gradient->firstPoint.x, gradient->firstPoint.y,
                     gradient->secondPoint.x, gradient->secondPoint.y, stops));
               }
               else
               {
                  GC->SetBrush(wxBrush());
               }
            }
            else
            {
               GC->SetBrush(wxBrushFromBrush(brush));
            }
         }
      }

      void SetFont(const std::shared_ptr<wxGraphicsContextPainterFont>& font)
      {
         if (font == nullptr)
            return;

         if (CurrentFont == nullptr || font->Font != CurrentFont->Font)
         {
            CurrentFont = font;
            FontDirty = true;
         }
      }

      void UpdateFont()
      {
         if (CurrentFont == nullptr)
            return;

         if (FontDirty || CurrentTextColor != CurrentBrush.GetColor())
         {
            CurrentTextColor = CurrentBrush.GetColor();
            FontDirty = false;

            GC->SetFont(CurrentFont->GetGraphicsFont(*GC, CurrentTextColor));
         }
      }

      void SetTransform(const Transform& transform)
      {
         if (CurrentTransform != transform)
         {
            CurrentTransform = transform;
            
            const wxGraphicsMatrix mtx = GC->CreateMatrix(
               transform.GetScale().x, 0.0, 0.0, transform.GetScale().y,
               transform.GetTranslation().x, transform.GetTranslation().y);

            GC->SetTransform(mtx);
         }
      }

      void SetClipRect(const Rect& rect)
      {
         if (CurrentClipRect != rect)
         {
            CurrentClipRect = rect;

            GC->ResetClip();

            if (
               std::isfinite(rect.Size.width) &&
               std::isfinite(rect.Size.height))
            {
               GC->Clip(
                  rect.Origin.x, rect.Origin.y, rect.Size.width,
                  rect.Size.height);
            }
         }
      }

      bool SetAntialiasing(bool enabled)
      {
         if (CurrentAntialiasing != enabled)
         {
            const bool supported = GC->SetAntialiasMode(
               CurrentAntialiasing ? wxANTIALIAS_DEFAULT : wxANTIALIAS_NONE);

            CurrentAntialiasing = supported && enabled;

            return supported;
         }

         return false;
      }

      wxImage Image;
      std::unique_ptr<wxGraphicsContext> GC;

      Pen CurrentPen { Pen::NoPen };
      Brush CurrentBrush;
      
      std::shared_ptr<wxGraphicsContextPainterFont> CurrentFont;
      Color CurrentTextColor { Colors::Black };
      
      Transform CurrentTransform;
      Rect CurrentClipRect { NoClippingRect };

      bool FontDirty { true };
      bool CurrentAntialiasing { true };
   };

   wxGraphicsRenderer* mRenderer;
   wxWindow* mWindow;
   wxDC* mDC;

   std::vector<std::unique_ptr<Surface>> mPaintTargetsStack;
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

std::shared_ptr<PainterFont> WXGraphicsContextPainter::CreateFont(const FontInfo& fontInfo)
{
   return CreateFontFromWX(wxFontFromFontInfo(fontInfo));
}

std::shared_ptr<PainterFont>
WXGraphicsContextPainter::CreateFontFromWX(const wxFont& font)
{
   return std::make_shared<wxGraphicsContextPainterFont>(
      *this, font);
}

std::shared_ptr<PainterImage> WXGraphicsContextPainter::CreateImage(
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

   return std::make_shared<wxGraphicsContextPainterImage>(
      *this, mPaintTargetStack->GetRenderer(), image);
}

std::shared_ptr<PainterImage> WXGraphicsContextPainter::GetSubImage(
   const std::shared_ptr<PainterImage>& image, uint32_t x, uint32_t y,
   uint32_t width, uint32_t height)
{
   if (GetRendererID() != image->GetRendererID())
      return {};

   return std::make_shared<wxGraphicsContextPainterImage>(
      *this, static_cast<const wxGraphicsContextPainterImage&>(*image), x, y,
      width, height);
}

std::shared_ptr<PainterImage> WXGraphicsContextPainter::CreateDeviceImage(
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

   FlushCachedPath();

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
   UpdateFont(GetCurrentFont());
   UpdateAntiAliasingState(GetAntiAliasingEnabled());
   UpdateTransform(GetCurrentTransform());
   UpdateClipRect(GetCurrentClipRect());
}

void WXGraphicsContextPainter::EndPaint()
{
   FlushCachedPath();
   mPaintTargetStack->EndPaint();
}

void WXGraphicsContextPainter::UpdateBrush(const Brush& brush)
{
   if (!mPaintTargetStack->InPaintEnvent())
      return;

   mPaintTargetStack->SetBrush(brush);
}

void WXGraphicsContextPainter::UpdatePen(const Pen& pen)
{
   if (!mPaintTargetStack->InPaintEnvent())
      return;

   if (!mPaintTargetStack->IsSamePen(pen))
   {
      FlushCachedPath();
      mPaintTargetStack->SetPen(pen);
   }
}

void WXGraphicsContextPainter::UpdateTransform(const Transform& transform)
{
   if (!mPaintTargetStack->InPaintEnvent())
      return;

   mPaintTargetStack->SetTransform(transform);
}

void WXGraphicsContextPainter::UpdateClipRect(const Rect& rect)
{
   if (!mPaintTargetStack->InPaintEnvent())
      return;

   mPaintTargetStack->SetClipRect(rect);
}

bool WXGraphicsContextPainter::UpdateAntiAliasingState(bool enabled)
{
   if (!mPaintTargetStack->InPaintEnvent())
      return false;

   return mPaintTargetStack->SetAntialiasing(enabled);
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

   FlushCachedPath();

   mPaintTargetStack->GetCurrentContext()->FillPath(*mPaths[pathIndex]);
   mPaintTargetStack->GetCurrentContext()->StrokePath(*mPaths[pathIndex]);

   mPaths[pathIndex] = {};
}

void WXGraphicsContextPainter::DoDrawPolygon(const Point* pts, size_t count)
{
   if (!mPaintTargetStack->InPaintEnvent())
      return;

   FlushCachedPath();

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

   auto& path = GetCachedPath();

   for (size_t i = 0; i < count / 2; ++i)
   {
      path.MoveToPoint(pts[2 * i].x, pts[2 * i].y);
      path.AddLineToPoint(pts[2 * i + 1].x, pts[2 * i + 1].y);
   }

   /* if (count == 2)
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
   }*/
}

void WXGraphicsContextPainter::DoDrawRect(const Rect& rect)
{
   if (!mPaintTargetStack->InPaintEnvent())
      return;

   FlushCachedPath();

   mPaintTargetStack->GetCurrentContext()->DrawRectangle(
      rect.Origin.x, rect.Origin.y, rect.Size.width, rect.Size.height);
}

void WXGraphicsContextPainter::DoDrawEllipse(const Rect& rect)
{
   if (!mPaintTargetStack->InPaintEnvent())
      return;

   FlushCachedPath();

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

   FlushCachedPath();

   wxGraphicsContext* currentContext = mPaintTargetStack->GetCurrentContext();

   mPaintTargetStack->UpdateFont();
   
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

   FlushCachedPath();

   wxGraphicsContext* currentContext = mPaintTargetStack->GetCurrentContext();

   mPaintTargetStack->UpdateFont();

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
   
   FlushCachedPath();

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

void WXGraphicsContextPainter::UpdateFont(const std::shared_ptr<PainterFont>& font)
{
   if (!mPaintTargetStack->InPaintEnvent() || font->GetRendererID() != GetRendererID())
      return;

   mPaintTargetStack->SetFont(
      std::static_pointer_cast<wxGraphicsContextPainterFont>(font));
}

void WXGraphicsContextPainter::DoDrawRoundedRect(const Rect& rect, float radius)
{
   if (!mPaintTargetStack->InPaintEnvent())
      return;

   FlushCachedPath();

   mPaintTargetStack->GetCurrentContext()->DrawRoundedRectangle(
      rect.Origin.x, rect.Origin.y, rect.Size.width, rect.Size.height, radius);
}

void WXGraphicsContextPainter::PushPaintTarget(
   const std::shared_ptr<PainterImage>& image)
{
   if (image->GetRendererID() != GetRendererID())
      return;

   if (mPaintTargetStack->InPaintEnvent())
      FlushCachedPath();

   mPaintTargetStack->Push(std::static_pointer_cast<wxGraphicsContextPainterImage>(image));

   UpdatePen(GetCurrentPen());
   UpdateBrush(GetCurrentBrush());
   UpdateFont(GetCurrentFont());
   UpdateAntiAliasingState(GetAntiAliasingEnabled());
}

void WXGraphicsContextPainter::PopPaintTarget(
   const std::shared_ptr<PainterImage>& image)
{
   FlushCachedPath();
   mPaintTargetStack->Pop(
      std::static_pointer_cast<wxGraphicsContextPainterImage>(image));
}

wxGraphicsPath& WXGraphicsContextPainter::GetCachedPath()
{
   if (mCachedPath == nullptr)
   {
      mCachedPath = std::make_unique<wxGraphicsPath>(
         mPaintTargetStack->GetCurrentContext()->CreatePath());
   }

   return *mCachedPath;
}

void WXGraphicsContextPainter::FlushCachedPath()
{
   if (mCachedPath == nullptr)
      return;

   mPaintTargetStack->GetCurrentContext()->StrokePath(*mCachedPath);
   mCachedPath.reset();
}

RendererID WXGraphicsContextPainterRendererID()
{
   return rendererId;
}
