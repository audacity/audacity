/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  D2DPainter.cpp

  Dmitry Vedenko

**********************************************************************/

#include "D2DPainter.h"



#include "D2DRenderer.h"
#include "D2DFontCollection.h"
#include "D2DFont.h"

#include "bitmaps/D2DSubBitmap.h"
#include "bitmaps/D2DRenderTargetBitmap.h"

#ifdef CreateFont
#undef CreateFont
#endif

D2DPainter::D2DPainter(
   D2DRenderer& renderer, std::shared_ptr<D2DRenderTarget> target,
   const FontInfo& defaultFont)
    : mRenderer(renderer)
    , mDefaultFont(defaultFont)
{
   mRenderTargetStack.emplace_back(std::move(target));
}

D2DRenderTarget* D2DPainter::GetCurrentRenderTarget() const
{
   return !mRenderTargetStack.empty() ? mRenderTargetStack.back().get() :
                                        nullptr;
}

RendererID D2DPainter::GetRendererID() const
{
   return mRenderer.GetRendererID();
}

Size D2DPainter::GetSize() const
{
   if (mRenderTargetStack.empty())
      return {};

   return GetCurrentRenderTarget()->GetSize();
}

std::shared_ptr<PainterFont> D2DPainter::CreateFont(const FontInfo& fontInfo)
{
   return mRenderer.GetFontCollection()->GetFont(fontInfo, 96);
}

std::shared_ptr<PainterFont> D2DPainter::GetDefaultFont() const
{
   return mRenderer.GetFontCollection()->GetFont(mDefaultFont, 96);
}

std::shared_ptr<PainterImage> D2DPainter::CreateImage(
   PainterImageFormat format, uint32_t width, uint32_t height,
   const void* data /*= nullptr*/, const void* alphaData /*= nullptr*/)
{
   return mRenderer.CreateImage(format, width, height, data, alphaData);
}

std::shared_ptr<PainterImage> D2DPainter::GetSubImage(
   const std::shared_ptr<PainterImage>& image, uint32_t x, uint32_t y, uint32_t width,
   uint32_t height)
{
   if (image->GetRendererID() != GetRendererID())
      return {};
   
   return std::make_shared<D2DSubBitmap>(
      std::static_pointer_cast<D2DBitmap>(image),
      Rect { Point { static_cast<float>(x), static_cast<float>(y) },
             Size { static_cast<float>(width), static_cast<float>(height) } });
}

std::shared_ptr<PainterImage> D2DPainter::CreateDeviceImage(
   PainterImageFormat format, uint32_t width, uint32_t height)
{
   return std::make_shared<D2DRenderTargetBitmap>(
      mRenderer, width, height, format == PainterImageFormat::RGBA8888);
}

void D2DPainter::Flush()
{
   if (mRenderTargetStack.empty())
      return;

   mRenderTargetStack.back()->Flush();
}

void D2DPainter::BeginPaint()
{
   if (mRenderTargetStack.empty())
      return;

   mRenderTargetStack.back()->BeginDraw();
}

void D2DPainter::EndPaint()
{
   if (mRenderTargetStack.empty())
      return;

   mRenderTargetStack.back()->EndDraw();
}

void D2DPainter::DoClear(const Rect& rect, Color color)
{
   if (mRenderTargetStack.empty())
      return;

   mRenderTargetStack.back()->Clear(rect, color);
}

void D2DPainter::UpdateBrush(const Brush& brush)
{
   if (mRenderTargetStack.empty())
      return;

   mRenderTargetStack.back()->SetCurrentBrush(brush);
}

void D2DPainter::UpdatePen(const Pen& pen)
{
   if (mRenderTargetStack.empty())
      return;

   mRenderTargetStack.back()->SetCurrentPen(pen);
}

void D2DPainter::UpdateTransform(const Transform& transform)
{
   if (mRenderTargetStack.empty())
      return;

   mRenderTargetStack.back()->SetTransform(transform);
}

void D2DPainter::UpdateClipRect(const Rect& rect)
{
   if (mRenderTargetStack.empty())
      return;

   mRenderTargetStack.back()->SetClipRect(rect);
}

bool D2DPainter::UpdateAntiAliasingState(bool enabled)
{
   return false;
}

void D2DPainter::UpdateFont(const std::shared_ptr<PainterFont>& font)
{
}

size_t D2DPainter::BeginPath()
{
   return -1;
}

void D2DPainter::MoveTo(size_t pathIndex, Point pt)
{
}

void D2DPainter::LineTo(size_t pathIndex, Point pt)
{
}

void D2DPainter::AddRect(size_t pathIndex, const Rect& rect)
{
}

void D2DPainter::EndPath(size_t pathIndex)
{
}

void D2DPainter::DoDrawPolygon(const Point* pts, size_t count)
{
   if (mRenderTargetStack.empty())
      return;

   mRenderTargetStack.back()->DrawPolygon(pts, count);
}

void D2DPainter::DoDrawLines(const Point* pts, size_t count)
{
   if (mRenderTargetStack.empty())
      return;

   mRenderTargetStack.back()->DrawLines(pts, count);
}

void D2DPainter::DoDrawRect(const Rect& rect)
{
   if (mRenderTargetStack.empty())
      return;

   mRenderTargetStack.back()->DrawRect(rect);
}

void D2DPainter::DoDrawRoundedRect(const Rect& rect, float radius)
{
   if (mRenderTargetStack.empty())
      return;

   mRenderTargetStack.back()->DrawRoundedRect(rect, radius);
}

void D2DPainter::DoDrawEllipse(const Rect& rect)
{
   if (mRenderTargetStack.empty())
      return;

   mRenderTargetStack.back()->DrawEllipse(rect);
}

void D2DPainter::DoDrawImage(
   const PainterImage& image, const Rect& destRect, const Rect& imageRect)
{
   if (mRenderTargetStack.empty() || image.GetRendererID() != GetRendererID())
      return;

   mRenderTargetStack.back()->DrawImage(static_cast<const D2DBitmap&>(image), destRect, imageRect);
}

void D2DPainter::DoDrawText(
   Point origin, const PainterFont& font, Brush backgroundBrush,
   const std::string_view& text)
{
}

void D2DPainter::DoDrawRotatedText(
   Point origin, float angle, const PainterFont& font, Brush backgroundBrush,
   const std::string_view& text)
{
}

Size D2DPainter::DoGetTextSize(
   const PainterFont& font, const std::string_view& text) const
{
   return {};
}

void D2DPainter::PushPaintTarget(const std::shared_ptr<PainterImage>& image)
{
   if (image->GetRendererID() != GetRendererID())
      return;

   auto renderTarget =
      std::static_pointer_cast<D2DBitmap>(image)->GetRenderTarget(
         *mRenderTargetStack.front());

   if (!renderTarget)
      return;

   mRenderTargetStack.emplace_back(renderTarget);

   renderTarget->BeginDraw();
}

void D2DPainter::PopPaintTarget(const std::shared_ptr<PainterImage>&)
{
   if (mRenderTargetStack.size() <= 1)
      return;
   
   mRenderTargetStack.back()->EndDraw();
   mRenderTargetStack.pop_back();
}
