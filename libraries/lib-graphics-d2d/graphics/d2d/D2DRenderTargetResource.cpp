/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  D2DRenderTarget.h

  Dmitry Vedenko

**********************************************************************/
#include "D2DRenderTargetResource.h"
#include "D2DRenderTarget.h"

bool D2DRenderTargetResource::AcquireResource(D2DRenderTarget& target)
{
   auto& rootTarget = target.GetRootRenderTarget();
   
   if (DoAcquireResource(rootTarget))
   {
      rootTarget.TrackResource(shared_from_this());
      return true;
   }

   return false;
}

void D2DRenderTargetResource::ReleaseResource(D2DRenderTarget& target)
{
   DoReleaseResource(target.GetRootRenderTarget());
}
