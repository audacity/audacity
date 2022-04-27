/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  D2DWindowRenderTarget.cpp

  Dmitry Vedenko

**********************************************************************/
#include "D2DWindowRenderTarget.h"

#include "graphics/d2d/D2DRenderer.h"

D2DWindowRenderTarget::D2DWindowRenderTarget(
   D2DRenderer& renderer, HWND hwnd)
    : D2DRenderTarget(renderer)
    , mRenderer(renderer)
    , mHWND(hwnd)
{
   CreateRenderTarget();
}

void D2DWindowRenderTarget::BeginDraw()
{
   if (!mHWNDRenderTarget)
      return;
   
   const auto size = GetD2DSize();

   if (size.width != mCurrentSize.width || size.height != mCurrentSize.height)
   {
      mCurrentSize = size;
      mHWNDRenderTarget->Resize(size);
   }

   mHWNDRenderTarget->BeginDraw();
}

D2D1_SIZE_U D2DWindowRenderTarget::GetD2DSize() const
{
   RECT rc;
   GetClientRect(mHWND, &rc);

   return D2D1::SizeU(rc.right - rc.left, rc.bottom - rc.top);
}

Size D2DWindowRenderTarget::GetSize() const
{
   const auto size = GetD2DSize();

   return { static_cast<float>(size.width), static_cast<float>(size.height) };
}

bool D2DWindowRenderTarget::IsValid() const
{
   return mHWNDRenderTarget != nullptr;
}

void D2DWindowRenderTarget::HandleContextLoss()
{
   CreateRenderTarget();
}

void D2DWindowRenderTarget::CreateRenderTarget()
{
   const auto size = GetD2DSize();

   mCurrentSize = size;

   auto result = mRenderer.GetD2DFactory()->CreateHwndRenderTarget(
      D2D1::RenderTargetProperties(),
      D2D1::HwndRenderTargetProperties(mHWND, size),
      mHWNDRenderTarget.ReleaseAndGetAddressOf());

   if (result == S_OK)
      mRenderTarget = mHWNDRenderTarget;
}
