/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  D2DWICBitmap.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <d2d1.h>
#include <wincodec.h>
#include <wrl.h>

#include <map>
#include <memory>

#include "../D2DBitmap.h"

class D2DWICRenderTarget;
class D2DRenderer;

class D2DWICBitmap final : public D2DBitmap
{
public:
   D2DWICBitmap(
      D2DRenderer& renderer,
      const Microsoft::WRL::ComPtr<IWICBitmap>& wicBitmap, bool withAlhpa);
   
   void DrawBitmap(
      D2DRenderTarget& target, const Rect& targetRect,
      const Rect& sourceRect) override;

   std::shared_ptr<D2DRenderTarget>
   GetRenderTarget(D2DRenderTarget& parentRenderTarget) override;

   void DrawFinished(D2DRenderTarget& renderTarget) override;

   uint32_t GetWidth() const override;

   uint32_t GetHeight() const override;

   IWICBitmap* GetWICBitmap();

   D2DRenderer& GetRenderer();

private:
   ID2D1Bitmap* GetBitmap(D2DRenderTarget& target);
   
   bool DoAcquireResource(D2DRenderTarget& target) override;
   void DoReleaseResource(D2DRenderTarget& target) override;

   void CleanupDirect2DResources() override;

   Microsoft::WRL::ComPtr<IWICBitmap> mWICBitmap;

   using RTResourcesMap = std::map<D2DRenderTarget*, Microsoft::WRL::ComPtr<ID2D1Bitmap>>;

   RTResourcesMap mRenderTargetResources;

   std::shared_ptr<D2DWICRenderTarget> mRenderTarget;

   bool mHasAlpha { false };
};
