/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  D2DRenderTargetBitmap.h

  Dmitry Vedenko

**********************************************************************/
#include "graphics/d2d/D2DRenderTarget.h"
#include "graphics/d2d/D2DBitmap.h"

#include <map>

namespace graphics::d2d
{
   
class D2DRenderTargetBitmap final : public D2DRenderTarget, public D2DBitmap
{
public:
   D2DRenderTargetBitmap(
      D2DRenderer& renderer, uint32_t width, uint32_t height, bool withAlpha);

   Size GetSize() const override;

   void DrawBitmap(
      D2DRenderTarget& target, const Rect& targetRect,
      const Rect& sourceRect) override;

   std::shared_ptr<D2DRenderTarget>
   GetRenderTarget(D2DRenderTarget& parentRenderTarget) override;

   void DrawFinished(D2DRenderTarget& renderTarget) override;

   uint32_t GetWidth() const override;

   uint32_t GetHeight() const override;

   bool IsValid(Painter& painter) const override;

private:
   bool DoAcquireResource(D2DRenderTarget& target) override;
   void DoReleaseResource(D2DRenderTarget& target) override;

   void CleanupDirect2DResources() override;

   struct RTDependentData final
   {
      Microsoft::WRL::ComPtr<ID2D1Bitmap> Bitmap;
      Microsoft::WRL::ComPtr<ID2D1BitmapRenderTarget> RenderTarget;
   };

   using RTDependentDataMap = std::map<D2DRenderTarget*, RTDependentData>;
   RTDependentDataMap mRTDependentData;

   uint32_t mWidth;
   uint32_t mHeight;
   bool mHasAlpha;
};

} // namespace graphics::d2d
