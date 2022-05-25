/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  D2DRenderTargetBitmap.cpp

  Dmitry Vedenko

**********************************************************************/
#include "D2DRenderTargetBitmap.h"

#include "graphics/d2d/D2DPainter.h"
#include "graphics/d2d/D2DRenderer.h"

D2DRenderTargetBitmap::D2DRenderTargetBitmap(
   D2DRenderer& renderer, uint32_t width, uint32_t height,
   bool withAlpha)
    : D2DRenderTarget(renderer)
    , D2DBitmap(renderer)
    , mWidth(width)
    , mHeight(height)
    , mHasAlpha(withAlpha)
{}

Size D2DRenderTargetBitmap::GetSize() const
{
   return { static_cast<float>(mWidth), static_cast<float>(mHeight) };
}

void D2DRenderTargetBitmap::DrawBitmap(
   D2DRenderTarget& target, const Rect& targetRect, const Rect& sourceRect)
{
   auto it = mRTDependentData.find(&target.GetRootRenderTarget());

   if (it == mRTDependentData.end())
      return;

   D2D1_RECT_F d2dTargetRect = { targetRect.origin.x, targetRect.origin.y,
                                 targetRect.origin.x + targetRect.size.width,
                                 targetRect.origin.y + targetRect.size.height };

   D2D1_RECT_F d2dSourceRect = { sourceRect.origin.x, sourceRect.origin.y,
                                 sourceRect.origin.x + sourceRect.size.width,
                                 sourceRect.origin.y + sourceRect.size.height };

   const auto brush = target.GetCurrentBrush();

   const float opacity = brush.GetStyle() == BrushStyle::Solid ?
                            brush.GetColor().GetAlpha() / 255.0f :
                            1.0f;

   target.GetD2DRenderTarget()->DrawBitmap(
      it->second.Bitmap.Get(), d2dTargetRect, opacity,
      D2D1_BITMAP_INTERPOLATION_MODE_LINEAR,
      d2dSourceRect);
}

std::shared_ptr<D2DRenderTarget>
D2DRenderTargetBitmap::GetRenderTarget(D2DRenderTarget& parentRenderTarget)
{
   auto rootRenderTarget = &parentRenderTarget.GetRootRenderTarget();
   auto it = mRTDependentData.find(rootRenderTarget);

   if (it == mRTDependentData.end())
   {
      if (!AcquireResource(parentRenderTarget))
         return {};
      
      it = mRTDependentData.find(rootRenderTarget);
   }

   mParentTarget = &parentRenderTarget;
   mRenderTarget = it->second.RenderTarget;
   
   return std::static_pointer_cast<D2DRenderTargetBitmap>(shared_from_this());
}

void D2DRenderTargetBitmap::DrawFinished(D2DRenderTarget&)
{
}

uint32_t D2DRenderTargetBitmap::GetWidth() const
{
   return mWidth;
}

uint32_t D2DRenderTargetBitmap::GetHeight() const
{
   return mHeight;
}

bool D2DRenderTargetBitmap::IsValid(Painter& painter) const
{
   if (painter.GetRendererID() != GetRendererID())
      return false;

   auto& d2dPainter = static_cast<D2DPainter&>(painter);
   auto renderTarget = d2dPainter.GetCurrentRenderTarget();

   if (renderTarget == nullptr)
      return false;

   return mRTDependentData.find(&renderTarget->GetRootRenderTarget()) !=
          mRTDependentData.end();
}

bool D2DRenderTargetBitmap::DoAcquireResource(D2DRenderTarget& target)
{
   Microsoft::WRL::ComPtr<ID2D1BitmapRenderTarget> renderTarget;

   D2D1_SIZE_F desiredSize = { FLOAT(mWidth), FLOAT(mHeight) };
   D2D1_PIXEL_FORMAT pixelFormat = { DXGI_FORMAT_UNKNOWN,
                                     mHasAlpha ? D2D1_ALPHA_MODE_PREMULTIPLIED :
                                                 D2D1_ALPHA_MODE_IGNORE };

   D2D1_COMPATIBLE_RENDER_TARGET_OPTIONS options = {
      D2D1_COMPATIBLE_RENDER_TARGET_OPTIONS_NONE
   };

   auto result = target.GetD2DRenderTarget()->CreateCompatibleRenderTarget(
      &desiredSize, nullptr, &pixelFormat, options,
         renderTarget.GetAddressOf());

   if (S_OK != result)
      return false;

   Microsoft::WRL::ComPtr<ID2D1Bitmap> bitmap;

   result = renderTarget->GetBitmap(bitmap.GetAddressOf());

   if (S_OK != result)
      return false;

   RTDependentData data { bitmap, renderTarget };

   mRTDependentData.emplace(std::make_pair(&target, data));

   return true;
}

void D2DRenderTargetBitmap::DoReleaseResource(D2DRenderTarget& target)
{
   auto it = mRTDependentData.find(&target);

   if (it != mRTDependentData.end())
      mRTDependentData.erase(it);
}

void D2DRenderTargetBitmap::CleanupDirect2DResources()
{
   mRTDependentData.clear();
}
