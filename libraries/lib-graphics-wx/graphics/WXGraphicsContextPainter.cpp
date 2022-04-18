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

#include "graphics/RendererID.h"

#include "WXPainterUtils.h"

#include "WXColor.h"

namespace
{
auto rendererId = RegisterRenderer("wxGraphicsDefaultRenderer");

struct wxGraphicsContextPainterImage final : public PainterImage
{
   wxGraphicsContextPainterImage(Painter& painter, wxGraphicsContext& ctx, wxImage& img)
       : PainterImage(painter)
       , Bitmap(ctx.CreateBitmapFromImage(img))
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
      Painter& painter, wxGraphicsContext& ctx, const wxFont& font)
       : PainterFont(painter)
       , Font(font)
       , CacheContext(&ctx)
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
      CacheContext->SetFont(Font, *wxBLACK);

      double width, height;

      CacheContext->GetTextExtent(
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
      

   //wxGraphicsFont GraphicsFont;
   wxFont Font;

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
   explicit PaintTargetStack(wxGraphicsContext* graphicsContext)
       : mBaseContext(graphicsContext)
   {
   }

   PaintTargetStack(const PaintTargetStack&) = delete;
   PaintTargetStack(PaintTargetStack&&) = delete;
   PaintTargetStack& operator=(const PaintTargetStack&) = delete;
   PaintTargetStack& operator=(PaintTargetStack&&) = delete;

   void Push(wxGraphicsContextPainterImage& image)
   {
      mStack.emplace_back(std::make_unique<Surface>(
         *mBaseContext, image.Width, image.Height, image.HasAlpha));
   }

   void Pop(wxGraphicsContextPainterImage& image)
   {
      auto& currentItem = mStack.back();

      currentItem->GC = {};
      image.Bitmap = mBaseContext->CreateBitmap(currentItem->Bitmap);

      mStack.pop_back();
   }

   wxGraphicsContext& GetCurrentContext()
   {
      return mStack.empty() ? *mBaseContext : *mStack.back()->GC;
   }

private:
   struct Surface final
   {
      Surface(wxGraphicsContext& ctx, uint32_t width, uint32_t height, bool alpha)
          : Bitmap(width, height, alpha ? 32 : 24)
          , DC(Bitmap)
          , GC(ctx.GetRenderer()->CreateContext(DC))
      {
      }

      wxBitmap Bitmap;
      wxMemoryDC DC;
      std::unique_ptr<wxGraphicsContext> GC;
   };

   std::unique_ptr<wxGraphicsContext> mBaseContext;
   std::vector<std::unique_ptr<Surface>> mStack;
};

WXGraphicsContextPainter::WXGraphicsContextPainter(
   wxGraphicsContext* graphicsContext, const wxFont& defaultFont)
    : mPaintTargetStack(std::make_unique<PaintTargetStack>(graphicsContext))
    , mDefaultFont(std::make_shared<wxGraphicsContextPainterFont>(*this, *graphicsContext, defaultFont))
{

}

WXGraphicsContextPainter::~WXGraphicsContextPainter()
{
}

Size WXGraphicsContextPainter::GetSize() const
{
   wxDouble width, height;
   mPaintTargetStack->GetCurrentContext().GetSize(&width, &height);

   return Size { static_cast<float>(width), static_cast<float>(height) };
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
      *this, mPaintTargetStack->GetCurrentContext(), font);
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
         image.SetAlpha();
   }

   return std::make_unique<wxGraphicsContextPainterImage>(
      *this, mPaintTargetStack->GetCurrentContext(), image);
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
   mPaintTargetStack->GetCurrentContext().ClearRectangle(
      rect.Origin.x, rect.Origin.y, rect.Size.width, rect.Size.height);

   if (color != Colors::Transparent)
      mPaintTargetStack->GetCurrentContext().DrawRectangle(
         rect.Origin.x, rect.Origin.y, rect.Size.width, rect.Size.height);
}

void WXGraphicsContextPainter::Flush()
{
   mPaintTargetStack->GetCurrentContext().Flush();
}

void WXGraphicsContextPainter::UpdateBrush(const Brush& brush)
{
   auto& ctx = mPaintTargetStack->GetCurrentContext();
   
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
         
         ctx.SetBrush(ctx.CreateLinearGradientBrush(
            gradient->firstPoint.x, gradient->firstPoint.y,
            gradient->secondPoint.x, gradient->secondPoint.y, stops));
      }
      else
      {
         ctx.SetBrush(wxBrush());
      }
   }
   else
   {
      ctx.SetBrush(wxBrushFromBrush(brush));
   }
}

void WXGraphicsContextPainter::UpdatePen(const Pen& pen)
{
   mPaintTargetStack->GetCurrentContext().SetPen(wxPenFromPen(pen));
}

void WXGraphicsContextPainter::UpdateTransform(const Transform& transform)
{
   const wxGraphicsMatrix mtx = mPaintTargetStack->GetCurrentContext().CreateMatrix(
      transform.GetScale().x, 0.0, 0.0, transform.GetScale().y,
      transform.GetTranslation().x, transform.GetTranslation().y);

   mPaintTargetStack->GetCurrentContext().SetTransform(mtx);
}

void WXGraphicsContextPainter::UpdateClipRect(const Rect& rect)
{
   mPaintTargetStack->GetCurrentContext().ResetClip();
   mPaintTargetStack->GetCurrentContext().Clip(wxRegion(
      rect.Origin.x, rect.Origin.y, rect.Size.width, rect.Size.height));
}

bool WXGraphicsContextPainter::UpdateAntiAliasingState(bool enabled)
{
   return mPaintTargetStack->GetCurrentContext().SetAntialiasMode(
             enabled ? wxANTIALIAS_DEFAULT : wxANTIALIAS_NONE) ==
          wxANTIALIAS_DEFAULT;
}

size_t WXGraphicsContextPainter::BeginPath()
{
   for (size_t i = 0; i < mPaths.size(); ++i)
   {
      if (mPaths[i] == nullptr)
      {
         mPaths[i] =
            std::make_unique<wxGraphicsPath>(mPaintTargetStack->GetCurrentContext().CreatePath());

         return i;
      }
   }

   mPaths.emplace_back(std::make_unique<wxGraphicsPath>(mPaintTargetStack->GetCurrentContext().CreatePath()));
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

   mPaintTargetStack->GetCurrentContext().FillPath(*mPaths[pathIndex]);
   mPaintTargetStack->GetCurrentContext().StrokePath(*mPaths[pathIndex]);

   mPaths[pathIndex] = {};
}

void WXGraphicsContextPainter::DoDrawPolygon(const Point* pts, size_t count)
{
   mPoints.reserve(count);

   for (size_t i = 0; i < count; ++i)
      mPoints.emplace_back(wxPoint2DDouble(pts[i].x, pts[i].y)); 

  mPaintTargetStack->GetCurrentContext().DrawLines(mPoints.size(), mPoints.data());

  mPoints.clear();
}

void WXGraphicsContextPainter::DoDrawLines(const Point* pts, size_t count)
{
   assert(count % 2 == 0);

   if (count == 2)
   {
      mPaintTargetStack->GetCurrentContext().StrokeLine(pts[0].x, pts[0].y, pts[1].x, pts[1].y);
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

      mPaintTargetStack->GetCurrentContext().StrokeLines(
         mPoints.size(), mPoints.data(), mEndPoints.data());

      mPoints.clear();
      mEndPoints.clear();
   }
}

void WXGraphicsContextPainter::DoDrawRect(const Rect& rect)
{
   mPaintTargetStack->GetCurrentContext().DrawRectangle(
      rect.Origin.x, rect.Origin.y, rect.Size.width, rect.Size.height);
}

void WXGraphicsContextPainter::DoDrawEllipse(const Rect& rect)
{
   mPaintTargetStack->GetCurrentContext().DrawEllipse(
      rect.Origin.x, rect.Origin.y, rect.Size.width, rect.Size.height);
}

void WXGraphicsContextPainter::DoDrawText(
   Point origin, const PainterFont& font, Brush backgroundBrush,
   const std::string_view& text)
{
   if (font.GetRendererID() != GetRendererID())
      return;

   wxGraphicsContext& currentContext = mPaintTargetStack->GetCurrentContext();

   currentContext.SetFont(
      static_cast<const wxGraphicsContextPainterFont&>(font).GetGraphicsFont(
         currentContext, GetCurrentBrush().GetColor()));

   const wxGraphicsBrush bgBrush =
      backgroundBrush.GetStyle() != BrushStyle::None ?
         currentContext.CreateBrush(wxBrushFromBrush(backgroundBrush)) :
         wxNullGraphicsBrush;
   
   currentContext.DrawText(
      wxString::FromUTF8(text.data(), text.size()), origin.x, origin.y, bgBrush);
}

void WXGraphicsContextPainter::DoDrawRotatedText(
   Point origin, float angle, const PainterFont& font, Brush backgroundBrush,
   const std::string_view& text)
{
   if (font.GetRendererID() != GetRendererID())
      return;

   wxGraphicsContext& currentContext = mPaintTargetStack->GetCurrentContext();

   currentContext.SetFont(
      static_cast<const wxGraphicsContextPainterFont&>(font).GetGraphicsFont(
         currentContext, GetCurrentBrush().GetColor()));

   const wxGraphicsBrush bgBrush =
      backgroundBrush.GetStyle() != BrushStyle::None ?
         currentContext.CreateBrush(wxBrushFromBrush(backgroundBrush)) :
         wxNullGraphicsBrush;

   currentContext.DrawText(
      wxString::FromUTF8(text.data(), text.size()), origin.x, origin.y, angle,
      bgBrush);
}

void WXGraphicsContextPainter::DoDrawImage(
   const PainterImage& painterImage, const Rect& rect, const Rect& imageRect)
{
   if (painterImage.GetRendererID() != GetRendererID())
      return;

   if (rect.Size.IsZero() || imageRect.Size.IsZero())
      return;

   wxGraphicsContext& currentContext = mPaintTargetStack->GetCurrentContext();

   const auto& image =
      static_cast<const wxGraphicsContextPainterImage&>(painterImage);
   
   auto gcImage = image.Bitmap;

   if (
      !imageRect.Origin.IsZero() || uint32_t(imageRect.Size.width) != image.Width ||
      uint32_t(imageRect.Size.height) != image.Height)
   {
      gcImage = currentContext.CreateSubBitmap(
         gcImage, imageRect.Origin.x, imageRect.Origin.y, imageRect.Size.width,
         imageRect.Size.height);
   }
   
   currentContext.DrawBitmap(
      gcImage, rect.Origin.x, rect.Origin.y, rect.Size.width, rect.Size.height);
}

std::shared_ptr<PainterFont> WXGraphicsContextPainter::GetDefaultFont() const
{
   return mDefaultFont;
}

Size WXGraphicsContextPainter::DoGetTextSize(
   const PainterFont& font, const std::string_view& text) const
{
   return font.GetTextSize(text);
}

void WXGraphicsContextPainter::UpdateFont(const PainterFont&)
{

}

void WXGraphicsContextPainter::DoDrawRoundedRect(const Rect& rect, float radius)
{
   mPaintTargetStack->GetCurrentContext().DrawRoundedRectangle(
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
