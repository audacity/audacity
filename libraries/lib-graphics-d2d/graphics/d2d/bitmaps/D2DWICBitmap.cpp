/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  D2DWICBitmap.cpp

  Dmitry Vedenko

**********************************************************************/
#include "D2DWICBitmap.h"

#include "graphics/d2d/render_targets/D2DWICRenderTarget.h"
#include "graphics/d2d/D2DRenderer.h"

D2DWICBitmap::D2DWICBitmap(
   D2DRenderer& renderer,
   const Microsoft::WRL::ComPtr<IWICBitmap>& wicBitmap, bool withAlhpa)
    : D2DBitmap(renderer)
    , mWICBitmap(wicBitmap)
    , mHasAlpha(withAlhpa)
{
}

void D2DWICBitmap::DrawBitmap(
   D2DRenderTarget& target, const Rect& targetRect, const Rect& sourceRect)
{
   auto bitmap = GetBitmap(target);

   if (!bitmap)
   {
      return;
   }

   D2D1_RECT_F d2dTargetRect = { targetRect.Origin.x, targetRect.Origin.y,
                                 targetRect.Origin.x + targetRect.Size.width,
                                 targetRect.Origin.y + targetRect.Size.height };

   D2D1_RECT_F d2dSourceRect = { sourceRect.Origin.x, sourceRect.Origin.y,
                                 sourceRect.Origin.x + sourceRect.Size.width,
                                 sourceRect.Origin.y + sourceRect.Size.height };

   const auto brush = target.GetCurrentBrush();

   const float opacity = brush.GetStyle() == BrushStyle::Solid ?
      brush.GetColor().GetAlpha() / 255.0f : 1.0f;

   target.GetD2DRenderTarget()->DrawBitmap(
      bitmap, d2dTargetRect, opacity, D2D1_BITMAP_INTERPOLATION_MODE_LINEAR,
      d2dSourceRect);
}

std::shared_ptr<D2DRenderTarget>
D2DWICBitmap::GetRenderTarget(D2DRenderTarget& parentRenderTarget)
{
   if (!mRenderTarget)
      mRenderTarget = std::make_shared<D2DWICRenderTarget>(*this);

   if(mRenderTarget->SetParent(parentRenderTarget))   
      return mRenderTarget;

   return nullptr;
}

void D2DWICBitmap::DrawFinished(D2DRenderTarget& renderTarget)
{
   if (&renderTarget != mRenderTarget.get())
      return;

   DoReleaseResource(renderTarget.GetRootRenderTarget());
}

uint32_t D2DWICBitmap::GetWidth() const
{
   if (mWICBitmap == nullptr)
      return 0;
   
   UINT width, height;

   if (S_OK != mWICBitmap->GetSize(&width, &height))
      return 0;
   
   return width;
}

uint32_t D2DWICBitmap::GetHeight() const
{
   if (mWICBitmap == nullptr)
      return 0;

   UINT width, height;

   if (S_OK != mWICBitmap->GetSize(&width, &height))
      return 0;

   return height;
}

IWICBitmap* D2DWICBitmap::GetWICBitmap()
{
   return mWICBitmap.Get();
}

D2DRenderer& D2DWICBitmap::GetRenderer()
{
   return mRenderer;
}

ID2D1Bitmap* D2DWICBitmap::GetBitmap(D2DRenderTarget& target)
{
   auto& rootTarget = target.GetRootRenderTarget();

   auto it = mRenderTargetResources.find(&rootTarget);

   if (it != mRenderTargetResources.end())
      return it->second.Get();

   if (!AcquireResource(rootTarget))
      return nullptr;

   it = mRenderTargetResources.find(&rootTarget);

   return it != mRenderTargetResources.end() ? it->second.Get() : nullptr;
}

bool D2DWICBitmap::DoAcquireResource(D2DRenderTarget& target)
{
   if (mWICBitmap == nullptr)
      return false;
   
   auto d2dRenderTarget = target.GetD2DRenderTarget();

   if (d2dRenderTarget == nullptr)
      return false;

   Microsoft::WRL::ComPtr<ID2D1Bitmap> bitmap;

   auto result = d2dRenderTarget->CreateBitmapFromWicBitmap(
      mWICBitmap.Get(), bitmap.GetAddressOf());

   if (S_OK != result)
      return false;

   mRenderTargetResources.emplace(&target, bitmap);

   return true;
}

void D2DWICBitmap::DoReleaseResource(D2DRenderTarget& target)
{
   auto it = mRenderTargetResources.find(&target);

   if (it != mRenderTargetResources.end())
      mRenderTargetResources.erase(it);
}

void D2DWICBitmap::CleanupDirect2DResources()
{
   mWICBitmap.Reset();
   mRenderTargetResources.clear();
}
