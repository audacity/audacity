/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  D2DRenderTarget.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <d2d1.h>
#include <wrl.h>

#include <memory>
#include <vector>

#include "graphics/Pen.h"
#include "graphics/Brush.h"
#include "graphics/Transform.h"

#include "graphics/RendererID.h"

#include "GraphicsObjectCache.h"

#include "D2DTrackedResource.h"

class D2DRenderTargetResource;
class D2DRenderer;
class D2DBitmap;
class D2DPathGeometry;

class D2DRenderTarget /* not final */
{
public:
   explicit D2DRenderTarget(D2DRenderer& renderer);
   virtual ~D2DRenderTarget();

   D2DRenderTarget& GetRootRenderTarget() noexcept;
   ID2D1RenderTarget* GetD2DRenderTarget() noexcept;

   bool SetAntialisingEnabled(bool enabled) noexcept;

   virtual Size GetSize() const;

   virtual void BeginDraw();
   bool EndDraw();

   void Flush();

   void Clear(const Rect& rect, Color color);

   void SetClipRect(const Rect& rect);

   void SetCurrentTransform(const Transform& transform) noexcept;
   Transform GetCurrentTransform() const noexcept;
   D2D1::Matrix3x2F GetCurrentD2DTransform() const noexcept;

   void SetCurrentBrush(const Brush& brush);
   Brush GetCurrentBrush() const;
   ID2D1Brush* GetCurrentD2DBrush() const;

   void SetCurrentPen(const Pen& pen);
   Pen GetCurrentPen() const;
   ID2D1Brush* GetCurrentD2DPen() const;

   void DrawPolygon(const Point* pts, size_t count);
   void DrawLines(const Point* ptr, size_t count);
   void DrawRect(const Rect& rect);
   void DrawRoundedRect(const Rect& rect, float radius);
   void DrawEllipse(const Rect& rect);

   void DrawImage(
      const D2DBitmap& image, const Rect& destRect, const Rect& imageRect);

protected:
   void ReleaseResources();

   virtual void HandleContextLoss();
   virtual void HandlePostDrawAction(bool successful);

   Microsoft::WRL::ComPtr<ID2D1LinearGradientBrush>
   CreateLinearGradientBrush(Brush brush);

private:
   void TrackResource(const std::shared_ptr<D2DRenderTargetResource>& resource);

protected:
   D2DRenderer& mRenderer;
   D2DRenderTarget* mParentTarget { nullptr };

   Microsoft::WRL::ComPtr<ID2D1RenderTarget> mRenderTarget;

private:
   std::vector<std::weak_ptr<D2DRenderTargetResource>> mRenderTargetResources;

   Brush mCurrentBrush { Brush::NoBrush };
   ID2D1Brush* mCurrentD2DBrush { nullptr };

   Microsoft::WRL::ComPtr<ID2D1SolidColorBrush> mSolidBrush;

   using LinearGradientBrushes = GraphicsObjectCache<Brush,Microsoft::WRL::ComPtr<ID2D1LinearGradientBrush>, 8, false>;
   LinearGradientBrushes mLinearGradientBrushes;

   Pen mCurrentPen { Pen::NoPen };
   Microsoft::WRL::ComPtr<ID2D1SolidColorBrush> mCurrentD2DPen;

   D2D1::Matrix3x2F mCurrentD2D1Transform { D2D1::Matrix3x2F::Identity() };

   Transform mCurrentTransform;

   std::shared_ptr<D2DPathGeometry> mSharedPath;

   Observer::Subscription mRendererShutdownSubscription;

   bool mHasClipRect { false };
   bool mAntialiasingEnabled { false };

   friend class D2DRenderTargetResource;
};
