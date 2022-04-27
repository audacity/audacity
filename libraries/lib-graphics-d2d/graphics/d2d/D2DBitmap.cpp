/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  D2DBitmap.h

  Dmitry Vedenko

**********************************************************************/

#include "D2DBitmap.h"

/*
D2DBitmap::D2DBitmap(
   const RendererID& rendererId,
   const Microsoft::WRL::ComPtr<IWICBitmap>& wicBitmap, bool withAlhpa)
    : PainterImage(rendererId)
    , mHasAlpha(withAlhpa)
{
   mWICBitmap = wicBitmap;

   mWICBitmap->GetSize(&mWidth, &mHeight);

   WICPixelFormatGUID pixelFormat;
   mWICBitmap->GetPixelFormat(&pixelFormat);
}

D2DBitmap::D2DBitmap(
   const RendererID& rendererId, uint32_t width, uint32_t height, bool withAlhpa)
    : PainterImage(rendererId)
    , mWidth(width)
    , mHeight(height)
    , mHasAlpha(withAlhpa)
{
}

uint32_t D2DBitmap::GetWidth() const
{
   return mWidth;
}

uint32_t D2DBitmap::GetHeight() const
{
   return mHeight;
}

bool D2DBitmap::IsValid() const
{
   return mHasAlpha;
}

ID2D1Bitmap* D2DBitmap::GetBitmap(ID2D1RenderTarget* renderTarget)
{
   auto it = mRTDependentData.find(renderTarget);

   if (it != mRTDependentData.end())
      return it->second.Bitmap.Get();

   if (mWICBitmap)
   {
      Microsoft::WRL::ComPtr<ID2D1Bitmap> bitmap;

      if (
         S_OK != renderTarget->CreateBitmapFromWicBitmap(
                    mWICBitmap.Get(), bitmap.GetAddressOf()))
         return {};

      mRTDependentData.emplace(
         std::make_pair(renderTarget, RTDependentData { bitmap }));

      return bitmap.Get();
   }
   else
   {
      return CreateCompatibleRenderTarget(renderTarget).Bitmap.Get();
   }
}

ID2D1RenderTarget* D2DBitmap::GetRenderTarget(ID2D1RenderTarget* renderTarget)
{
   auto it = mRTDependentData.find(renderTarget);

   if (it != mRTDependentData.end())
   {
      if (!it->second.RenderTarget)
      {
         Microsoft::WRL::ComPtr<ID2D1Factory> factory;
         renderTarget->GetFactory(factory.GetAddressOf());

         if (
            S_OK != factory->CreateWicBitmapRenderTarget(
                       mWICBitmap.Get(), nullptr,
                       it->second.RenderTarget.GetAddressOf()))
            return {};
      }

      return it->second.RenderTarget.Get();
   }

   return CreateCompatibleRenderTarget(renderTarget).RenderTarget.Get();
}

void D2DBitmap::Release(ID2D1RenderTarget* renderTarget)
{
   auto it = mRTDependentData.find(renderTarget);

   if (it != mRTDependentData.end())
      mRTDependentData.erase(it);
}

D2DBitmap::RTDependentData
D2DBitmap::CreateCompatibleRenderTarget(ID2D1RenderTarget* renderTarget)
{
   Microsoft::WRL::ComPtr<ID2D1BitmapRenderTarget> rt;

   D2D1_SIZE_F desiredSize = { FLOAT(mWidth), FLOAT(mHeight) };
   D2D1_PIXEL_FORMAT pixelFormat = { DXGI_FORMAT_B8G8R8A8_UNORM,
                                     mHasAlpha ? D2D1_ALPHA_MODE_PREMULTIPLIED :
                                                 D2D1_ALPHA_MODE_STRAIGHT };

   D2D1_COMPATIBLE_RENDER_TARGET_OPTIONS options = {
      D2D1_COMPATIBLE_RENDER_TARGET_OPTIONS_NONE
   };

   if (
      S_OK !=
      renderTarget->CreateCompatibleRenderTarget(
         &desiredSize, nullptr, &pixelFormat, options, rt.GetAddressOf()))
      return {};

   Microsoft::WRL::ComPtr<ID2D1Bitmap> bitmap;

   if (S_OK != rt->GetBitmap(bitmap.GetAddressOf()))
      return {};

   RTDependentData data { bitmap, rt };

   mRTDependentData.emplace(std::make_pair(renderTarget, data));

   return data;
}
*/

D2DBitmap::D2DBitmap(const RendererID& rendererId)
    : PainterImage(rendererId)
{
}
