/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  D2DWICRenderTarget.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include "../D2DRenderTarget.h"
#include "../D2DRenderTargetResource.h"

#include <d2d1.h>
#include <wrl.h>

#include <map>

namespace graphics::d2d
{

class D2DWICBitmap;

class D2DWICRenderTarget final :
    public D2DRenderTarget,
    public D2DRenderTargetResource
{
public:
   explicit D2DWICRenderTarget(D2DWICBitmap& bitmap);

   bool SetParent(D2DRenderTarget& parentRenderTarget);

   virtual void HandlePostDrawAction(bool successful) override;

private:
   bool DoAcquireResource(D2DRenderTarget& target) override;
   void DoReleaseResource(D2DRenderTarget& target) override;

   void CleanupDirect2DResources() override;

   D2DWICBitmap& mOwner;
};

} // namespace graphics::d2d
