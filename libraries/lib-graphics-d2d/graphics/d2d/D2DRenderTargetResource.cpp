/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  D2DRenderTarget.h

  Dmitry Vedenko

**********************************************************************/
#include "D2DRenderTargetResource.h"
#include "D2DRenderTarget.h"

D2DRenderTargetResource::D2DRenderTargetResource(D2DRenderer& renderer)
    : D2DTrackedResource(renderer)
{
}

bool D2DRenderTargetResource::AcquireResource(D2DRenderTarget& target)
{
   auto& rootTarget = target.GetRootRenderTarget();
   
   if (DoAcquireResource(rootTarget))
   {
      rootTarget.TrackResource(GetSharedPtr<D2DRenderTargetResource>());
      return true;
   }

   return false;
}

void D2DRenderTargetResource::ReleaseResource(D2DRenderTarget& target)
{
   DoReleaseResource(target.GetRootRenderTarget());
}
