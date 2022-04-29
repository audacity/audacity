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
class D2DRenderer;

class D2DBitmap /* not final */ :
    public PainterImage,
    public D2DRenderTargetResource
{
public:
   explicit D2DBitmap(D2DRenderer& renderer);

   virtual void DrawBitmap(
      D2DRenderTarget& target, const Rect& targetRect, const Rect& sourceRect) = 0;

   virtual std::shared_ptr<D2DRenderTarget> GetRenderTarget(D2DRenderTarget& parentRenderTarget) = 0;
   virtual void DrawFinished(D2DRenderTarget& renderTarget) = 0;
};
