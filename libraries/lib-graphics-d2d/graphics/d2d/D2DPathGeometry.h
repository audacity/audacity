/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  D2DPathGeometry.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <d2d1.h>
#include <wrl.h>

#include "graphics/Painter.h"
#include "D2DTrackedResource.h"

namespace graphics::d2d
{

class D2DRenderer;
class D2DRenderTarget;

class D2DPathGeometry final : public PainterPath, public D2DTrackedResource
{
public:
   explicit D2DPathGeometry(D2DRenderer& renderer);

   void EndFigure(bool closed) override;
   void DoLineTo(Point pt) override;
   void DoMoveTo(Point pt) override;
   void DoAddRect(const Rect& rect) override;

   void Draw(D2DRenderTarget& target);
   void Draw(D2DRenderTarget& target) const;

private:
   ID2D1GeometrySink* GetSink();

   void CleanupDirect2DResources() override;

   Microsoft::WRL::ComPtr<ID2D1PathGeometry> mPathGeometry;
   Microsoft::WRL::ComPtr<ID2D1GeometrySink> mSink;

   bool mFigureOpen { false };
};

} // namespace graphics::d2d
