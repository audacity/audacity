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
#include "D2DPathGeometry.h"

#include "bitmaps/D2DSubBitmap.h"
#include "bitmaps/D2DRenderTargetBitmap.h"

#ifdef CreateFont
#undef CreateFont
#endif

namespace graphics::d2d
{

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
   const std::shared_ptr<PainterImage>& image, uint32_t x, uint32_t y,
   uint32_t width, uint32_t height)
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

   mRenderTargetStack.back()->SetCurrentTransform(transform);
}

void D2DPainter::UpdateClipRect(const Rect& rect)
{
   if (mRenderTargetStack.empty())
      return;

   mRenderTargetStack.back()->SetClipRect(rect);
}

bool D2DPainter::UpdateAntiAliasingState(bool enabled)
{
   if (mRenderTargetStack.empty())
      return false;

   return mRenderTargetStack.back()->SetAntialisingEnabled(enabled);
}

void D2DPainter::UpdateFont(const std::shared_ptr<PainterFont>&)
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

   mRenderTargetStack.back()->DrawImage(
      static_cast<const D2DBitmap&>(image), destRect, imageRect);
}

void D2DPainter::DoDrawText(
   Point origin, const PainterFont& font, Brush backgroundBrush,
   const std::string_view& text)
{
   if (mRenderTargetStack.empty() || font.GetRendererID() != GetRendererID())
      return;

   auto& d2dFont = static_cast<const D2DFont&>(font);

   d2dFont.Draw(*mRenderTargetStack.back(), origin, backgroundBrush, text);
}

void D2DPainter::DoDrawRotatedText(
   Point origin, float angle, const PainterFont& font, Brush backgroundBrush,
   const std::string_view& text)
{
   if (mRenderTargetStack.empty() || font.GetRendererID() != GetRendererID())
      return;

   auto& d2dFont = static_cast<const D2DFont&>(font);

   auto renderTarget = mRenderTargetStack.back();
   auto d2dRenderTarget = renderTarget->GetD2DRenderTarget();

   auto currentTransform = renderTarget->GetCurrentD2DTransform();

   const float rad = -static_cast<float>(angle * M_PI / 180.0);
   const float c = std::cos(rad);
   const float s = std::sin(rad);

   const D2D1::Matrix3x2F mtx(
      c * currentTransform.m11 + s * currentTransform.m12,
      c * currentTransform.m21 + s * currentTransform.m22,
      -s * currentTransform.m11 + c * currentTransform.m12,
      -s * currentTransform.m21 + c * currentTransform.m22,
      origin.x + currentTransform.dx, origin.y + currentTransform.dy);

   d2dRenderTarget->SetTransform(mtx);

   d2dFont.Draw(*renderTarget, Point {}, backgroundBrush, text);

   d2dRenderTarget->SetTransform(currentTransform);
}

Size D2DPainter::DoGetTextSize(
   const PainterFont& font, const std::string_view& text) const
{
   return font.GetTextSize(text);
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

std::shared_ptr<PainterPath> D2DPainter::CreatePath()
{
   return mRenderer.CreatePathGeometry();
}

void D2DPainter::DrawPath(const PainterPath& path)
{
   if (mRenderTargetStack.empty() || path.GetRendererID() != GetRendererID())
      return;

   auto& d2dPath = static_cast<const D2DPathGeometry&>(path);
   d2dPath.Draw(*mRenderTargetStack.back());
}

} // namespace graphics::d2d
