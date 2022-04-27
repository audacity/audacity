/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  D2DRenderTarget.h

  Dmitry Vedenko

**********************************************************************/

#include "D2DRenderTarget.h"

#include <algorithm>
#include <cassert>

#include "D2DRenderer.h"
#include "D2DRenderTargetResource.h"
#include "D2DBitmap.h"

#include "graphics/Painter.h"

namespace
{
D2D1_COLOR_F GetD2DColor(Color color) noexcept
{
   return { color.GetRed() / 255.0f,  color.GetGreen() / 255.0f,
            color.GetBlue() / 255.0f, color.GetAlpha() / 255.0f };
}

D2D1::Matrix3x2F GetD2DTransform(const Transform& transform) noexcept
{
   return D2D1::Matrix3x2F(
      transform.GetScale().x, 0.0f, 0.0f, transform.GetScale().y,
      transform.GetTranslation().x, transform.GetTranslation().y);
}
}

D2DRenderTarget::D2DRenderTarget(D2DRenderer& renderer)
    : mRenderer(renderer)
{
}

D2DRenderTarget::~D2DRenderTarget()
{
   ReleaseResources();
}

D2DRenderTarget& D2DRenderTarget::GetRootRenderTarget() noexcept
{
   if (mParentTarget != nullptr)
      return mParentTarget->GetRootRenderTarget();

   return *this;
}

ID2D1RenderTarget* D2DRenderTarget::GetD2DRenderTarget() noexcept
{
   return mRenderTarget.Get();
}

Size D2DRenderTarget::GetSize() const
{
   if (mRenderTarget == nullptr)
      return {};
   
   const auto size = mRenderTarget->GetSize();
   return { size.width, size.height };
}

void D2DRenderTarget::BeginDraw()
{
   mRenderTarget->BeginDraw();
}

bool D2DRenderTarget::EndDraw()
{
   if (mHasClipRect)
      SetClipRect(NoClippingRect);
   
   auto result = mRenderTarget->EndDraw();

   if (result == D2DERR_RECREATE_TARGET)
   {
      ReleaseResources();
      HandleContextLoss();
   }

   HandlePostDrawAction(result == S_OK);
   
   return result == S_OK;
}

void D2DRenderTarget::Flush()
{
   if (D2DERR_RECREATE_TARGET == mRenderTarget->Flush())
   {
      ReleaseResources();
      HandleContextLoss();
   }
}

void D2DRenderTarget::Clear(const Rect& rect, Color color)
{
   mRenderTarget->PushAxisAlignedClip(
      D2D1::RectF(
         rect.Origin.x, rect.Origin.y, rect.Origin.x + rect.Size.width,
         rect.Origin.y + rect.Size.height),
      D2D1_ANTIALIAS_MODE_ALIASED);
   
   mRenderTarget->Clear(GetD2DColor(color));

   mRenderTarget->PopAxisAlignedClip();
}

void D2DRenderTarget::SetClipRect(const Rect& rect)
{
   if (mHasClipRect)
      mRenderTarget->PopAxisAlignedClip();

   mHasClipRect = rect != NoClippingRect;

   if (mHasClipRect)
   {
      mRenderTarget->PushAxisAlignedClip(
         D2D1::RectF(
            rect.Origin.x, rect.Origin.y, rect.Origin.x + rect.Size.width,
            rect.Origin.y + rect.Size.height),
         D2D1_ANTIALIAS_MODE_ALIASED);
   }
}

void D2DRenderTarget::SetTransform(const Transform& transform) noexcept
{
   mCurrentD2D1Transform = GetD2DTransform(transform);
}

D2D1::Matrix3x2F D2DRenderTarget::GetTransform() const noexcept
{
   return mCurrentD2D1Transform;
}

void D2DRenderTarget::SetCurrentBrush(const Brush& brush)
{
   if (
      brush == mCurrentBrush &&
      (brush.GetStyle() == BrushStyle::None || mCurrentD2DBrush != nullptr))
      return;

   mCurrentBrush = brush;
   
   if (brush.GetStyle() == BrushStyle::None)
   {
      mCurrentD2DBrush = nullptr;
   }
   else if (brush.GetStyle() == BrushStyle::Solid)
   {
      if (mSolidBrush == nullptr)
      {         
         if (
            S_OK !=
            mRenderTarget->CreateSolidColorBrush(
               GetD2DColor(brush.GetColor()), mSolidBrush.GetAddressOf()))
            return;
      }
      else
      {
         mSolidBrush->SetColor(GetD2DColor(brush.GetColor()));
      }

      mCurrentD2DBrush = mSolidBrush.Get();
   }
   else if (brush.GetStyle() == BrushStyle::LinearGradient)
   {
      auto it = std::find_if(
         mLinearGradientBrushes.begin(), mLinearGradientBrushes.end(),
         [brush](const auto& pair) { return pair.first == brush; });

      if (it != mLinearGradientBrushes.end())
         mCurrentD2DBrush = it->second.Get();
      else
         mCurrentD2DBrush = CreateLinearGradientBrush(brush);
   }
   else
   {
      assert(false);
      mCurrentD2DBrush = nullptr;
   }
}

Brush D2DRenderTarget::GetCurrentBrush() const
{
   return mCurrentBrush;
}

ID2D1Brush* D2DRenderTarget::GetCurrentD2DBrush() const
{
   return mCurrentD2DBrush;
}

void D2DRenderTarget::SetCurrentPen(const Pen& pen)
{
   if (
      pen == mCurrentPen &&
      (pen.GetStyle() == PenStyle::None || mCurrentD2DPen != nullptr))
      return;

   mCurrentPen = pen;

   if (mCurrentD2DPen == nullptr)
   {
      mRenderTarget->CreateSolidColorBrush(
         GetD2DColor(pen.GetColor()), mCurrentD2DPen.GetAddressOf());
   }
   else
   {
      mCurrentD2DPen->SetColor(GetD2DColor(pen.GetColor()));
   }
}

Pen D2DRenderTarget::GetCurrentPen() const
{
   return mCurrentPen;
}

ID2D1Brush* D2DRenderTarget::GetCurrentD2DPen() const
{
   return mCurrentPen.GetStyle() != PenStyle::None ? mCurrentD2DPen.Get() :
                                                     nullptr;
}

void D2DRenderTarget::DrawPolygon(const Point* pts, size_t count)
{
   if (count == 1)
   {
      return;
   }
   else if (count == 2)
   {
      DrawLines(pts, count);
   }
   else
   {
      if (mCurrentD2DBrush != nullptr)
      {
         
      }

      if (mCurrentPen != Pen::NoPen)
      {
      }
   }
}

void D2DRenderTarget::DrawLines(const Point* ptr, size_t count)
{
   if (mCurrentPen == Pen::NoPen)
      return;

   assert(count % 2 == 0);

   for (size_t i = 0; i < count; i += 2)
   {
      const auto& p1 = ptr[i];
      const auto& p2 = ptr[i + 1];

      mRenderTarget->DrawLine(
         D2D1::Point2F(p1.x, p1.y), D2D1::Point2F(p2.x, p2.y),
         mCurrentD2DPen.Get(), mCurrentPen.GetWidth(),
         mRenderer.GetStrokeStyle(mCurrentPen));
   }
}

void D2DRenderTarget::DrawRect(const Rect& rect)
{
   const auto d2dRect = D2D1::RectF(
      rect.Origin.x, rect.Origin.y, rect.Origin.x + rect.Size.width,
      rect.Origin.y + rect.Size.height);
   
   if (mCurrentD2DBrush != nullptr)
   {
      mRenderTarget->FillRectangle(d2dRect, mCurrentD2DBrush);
   }

   if (mCurrentPen.GetStyle() != Pen::NoPen)
   {
      mRenderTarget->DrawRectangle(
         d2dRect, mCurrentD2DPen.Get(), mCurrentPen.GetWidth(),
         mRenderer.GetStrokeStyle(mCurrentPen));
   }
}

void D2DRenderTarget::DrawRoundedRect(const Rect& rect, float radius)
{
   const auto d2dRect = D2D1::RectF(
      rect.Origin.x, rect.Origin.y, rect.Origin.x + rect.Size.width,
      rect.Origin.y + rect.Size.height);

   const D2D1_ROUNDED_RECT d2dRoundedRect { d2dRect, radius, radius };

   if (mCurrentD2DBrush != nullptr)
   {
      mRenderTarget->FillRoundedRectangle(d2dRoundedRect, mCurrentD2DBrush);
   }

   if (mCurrentPen.GetStyle() != Pen::NoPen)
   {
      mRenderTarget->DrawRoundedRectangle(
         d2dRoundedRect, mCurrentD2DPen.Get(), mCurrentPen.GetWidth(),
         mRenderer.GetStrokeStyle(mCurrentPen));
   }
}

void D2DRenderTarget::DrawEllipse(const Rect& rect)
{
   const auto center = D2D1::Point2F(
      rect.Origin.x + rect.Size.width / 2.0f,
      rect.Origin.y + rect.Size.height / 2.0f);

   const D2D1_ELLIPSE d2dEllipse { center, rect.Size.width, rect.Size.height };

   if (mCurrentD2DBrush != nullptr)
   {
      mRenderTarget->FillEllipse(d2dEllipse, mCurrentD2DBrush);
   }

   if (mCurrentPen.GetStyle() != Pen::NoPen)
   {
      mRenderTarget->DrawEllipse(
         d2dEllipse, mCurrentD2DPen.Get(), mCurrentPen.GetWidth(),
         mRenderer.GetStrokeStyle(mCurrentPen));
   }
}

void D2DRenderTarget::DrawImage(
   const D2DBitmap& image, const Rect& destRect, const Rect& imageRect)
{
   const_cast<D2DBitmap&>(image).DrawBitmap(*this, destRect, imageRect);
}

void D2DRenderTarget::ReleaseResources()
{
   for (auto weakResource : mRenderTargetResources)
   {
      auto resource = weakResource.lock();

      if (resource != nullptr)
         resource->ReleaseResource(*this);
   }

   mRenderTargetResources.clear();

   mSolidBrush.Get();
   mLinearGradientBrushes.clear();

   mCurrentD2DBrush = nullptr;
}

ID2D1LinearGradientBrush*
D2DRenderTarget::CreateLinearGradientBrush(Brush brush)
{
   auto gradientData = brush.GetGradientData();

   assert(gradientData != nullptr);

   if (gradientData == nullptr)
      return nullptr;

   std::vector<D2D1_GRADIENT_STOP> stops;
   stops.reserve(gradientData->stops.size());

   for (const auto& stop : gradientData->stops)
   {
      stops.emplace_back(
         D2D1_GRADIENT_STOP { stop.position, GetD2DColor(stop.color) });
   }

   Microsoft::WRL::ComPtr<ID2D1GradientStopCollection> gradientStopCollection;

   if (
      S_OK !=
      mRenderTarget->CreateGradientStopCollection(
         stops.data(), stops.size(), gradientStopCollection.GetAddressOf()))
      return nullptr;

   const D2D1_LINEAR_GRADIENT_BRUSH_PROPERTIES linearGradientBrushProperties {
      { gradientData->firstPoint.x, gradientData->firstPoint.y },
      { gradientData->secondPoint.x, gradientData->secondPoint.y }
   };

   const D2D1_BRUSH_PROPERTIES brushProperties {
      brush.GetColor().GetAlpha() / 255.0f, mCurrentD2D1Transform
   };

   Microsoft::WRL::ComPtr<ID2D1LinearGradientBrush> linearGradientBrush;

   if (
      S_OK !=
      mRenderTarget->CreateLinearGradientBrush(
         linearGradientBrushProperties, brushProperties,
         gradientStopCollection.Get(), linearGradientBrush.GetAddressOf()))
      return nullptr;

   mLinearGradientBrushes.emplace_back(brush, linearGradientBrush);

   return linearGradientBrush.Get();
}

void D2DRenderTarget::HandleContextLoss()
{
}

void D2DRenderTarget::HandlePostDrawAction(bool successful)
{
}

void D2DRenderTarget::TrackResource(
   const std::shared_ptr<D2DRenderTargetResource>& resource)
{
   mRenderTargetResources.emplace_back(resource);
}
