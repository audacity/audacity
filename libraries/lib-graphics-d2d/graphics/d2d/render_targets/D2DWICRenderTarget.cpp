/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  D2DWICRenderTarget.cpp

  Dmitry Vedenko

**********************************************************************/
#include "D2DWICRenderTarget.h"
#include "../bitmaps/D2DWICBitmap.h"

D2DWICRenderTarget::D2DWICRenderTarget(D2DWICBitmap& bitmap)
    : D2DRenderTarget(bitmap.GetRenderer())
    , D2DRenderTargetResource(bitmap.GetRenderer())
    , mOwner(bitmap)
{
}

bool D2DWICRenderTarget::SetParent(D2DRenderTarget& parentRenderTarget)
{
   if (mParentTarget == &parentRenderTarget && mRenderTarget != nullptr)
      return true;

   mParentTarget = &parentRenderTarget;

   return AcquireResource(parentRenderTarget);
}

void D2DWICRenderTarget::HandlePostDrawAction(bool successful)
{
   if (successful)
      mOwner.DrawFinished(*this);
}

bool D2DWICRenderTarget::DoAcquireResource(D2DRenderTarget& target)
{
   auto wicBitmap = mOwner.GetWICBitmap();

   if (wicBitmap != nullptr)
      return false;

   Microsoft::WRL::ComPtr<ID2D1Factory> factory;
   target.GetD2DRenderTarget()->GetFactory(factory.GetAddressOf());

   auto result =
      factory->CreateWicBitmapRenderTarget(wicBitmap, nullptr, &mRenderTarget);

   return S_OK == result;
}

void D2DWICRenderTarget::DoReleaseResource(D2DRenderTarget& target)
{
   if (mParentTarget == &target)
      return;

   mParentTarget = nullptr;
   mRenderTarget.Reset();
}

void D2DWICRenderTarget::CleanupDirect2DResources()
{
}
