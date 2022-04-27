/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  D2DBitmap.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <memory>

#include "graphics/Painter.h"
#include "D2DRenderTargetResource.h"

class D2DRenderTarget;

class D2DBitmap /* not final */ :
    public PainterImage,
    public D2DRenderTargetResource
{
public:
   explicit D2DBitmap(const RendererID& rendererId);

   virtual void DrawBitmap(
      D2DRenderTarget& target, const Rect& targetRect, const Rect& sourceRect) = 0;

   virtual std::shared_ptr<D2DRenderTarget> GetRenderTarget(D2DRenderTarget& parentRenderTarget) = 0;
   virtual void DrawFinished(D2DRenderTarget& renderTarget) = 0;
};
/*
class D2DBitmap final : public PainterImage, public std::enable_shared_from_this<D2DBitmap>
{
public:
   D2DBitmap(
      const RendererID& rendererId,
      const Microsoft::WRL::ComPtr<IWICBitmap>& wicBitmap, bool withAlhpa);
   
   D2DBitmap(
      const RendererID& rendererId, uint32_t width, uint32_t height,
      bool withAlhpa);

   uint32_t GetWidth() const override;
   uint32_t GetHeight() const override;
   bool IsValid() const override;

   ID2D1Bitmap* GetBitmap(ID2D1RenderTarget* renderTarget);
   ID2D1RenderTarget* GetRenderTarget(ID2D1RenderTarget* renderTarget);
   
   void Release(ID2D1RenderTarget* renderTarget);

private:
   struct RTDependentData final
   {
      Microsoft::WRL::ComPtr<ID2D1Bitmap> Bitmap;
      Microsoft::WRL::ComPtr<ID2D1RenderTarget> RenderTarget; 
   };

   RTDependentData
   CreateCompatibleRenderTarget(ID2D1RenderTarget* renderTarget);
   
   using RTDependentDataMap = std::map<ID2D1RenderTarget*, RTDependentData>;

   RTDependentDataMap mRTDependentData;

   Microsoft::WRL::ComPtr<IWICBitmap> mWICBitmap;

   uint32_t mWidth { 0 };
   uint32_t mHeight { 0 };
   
   bool mHasAlpha { false };
};
*/
