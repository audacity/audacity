/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  D2DDCRenderTarget.cpp

  Dmitry Vedenko

**********************************************************************/
#include "D2DDCRenderTarget.h"

#include "graphics/d2d/D2DRenderer.h"

D2DDCRenderTarget::D2DDCRenderTarget(D2DRenderer& renderer, HDC dc)
    : D2DRenderTarget(renderer)
    , mRenderer(renderer)
    , mDC(dc)
{
   CreateRenderTarget();
}

void D2DDCRenderTarget::BeginDraw()
{
   if (!mDCRenderTarget)
      return;

   const auto size = GetD2DSize();
   
   RECT rect { 0, 0, static_cast<LONG>(size.width),
               static_cast<LONG>(size.height) };

   auto result = mDCRenderTarget->BindDC(mDC, &rect);

   if (result != S_OK)
      return;

   mDCRenderTarget->BeginDraw();
}

D2D1_SIZE_U D2DDCRenderTarget::GetD2DSize() const
{
   BITMAP structBitmapHeader;
   memset(&structBitmapHeader, 0, sizeof(BITMAP));

   HGDIOBJ hBitmap = GetCurrentObject(mDC, OBJ_BITMAP);
   GetObject(hBitmap, sizeof(BITMAP), &structBitmapHeader);

   return D2D1::SizeU(structBitmapHeader.bmWidth, structBitmapHeader.bmHeight);
}

Size D2DDCRenderTarget::GetSize() const
{
   const auto size = GetD2DSize();

   return { static_cast<float>(size.width), static_cast<float>(size.height) };
}

bool D2DDCRenderTarget::IsValid() const
{
   return mRenderTarget != nullptr;
}

void D2DDCRenderTarget::HandleContextLoss()
{
   CreateRenderTarget();
}

void D2DDCRenderTarget::CreateRenderTarget()
{
   const auto size = GetD2DSize();

   mCurrentSize = size;

   D2D1_RENDER_TARGET_PROPERTIES props = D2D1::RenderTargetProperties(
      D2D1_RENDER_TARGET_TYPE_DEFAULT,
      D2D1::PixelFormat(DXGI_FORMAT_B8G8R8A8_UNORM, D2D1_ALPHA_MODE_IGNORE), 0,
      0, D2D1_RENDER_TARGET_USAGE_NONE, D2D1_FEATURE_LEVEL_DEFAULT);
   

   auto result = mRenderer.GetD2DFactory()->CreateDCRenderTarget(
      &props,
      mDCRenderTarget.ReleaseAndGetAddressOf());

   if (result != S_OK)
      return;

   mRenderTarget = mDCRenderTarget;
}
