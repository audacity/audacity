/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  D2DRenderTarget.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <memory>

#include "D2DTrackedResource.h"

class D2DRenderTarget;
class D2DRenderer;

class D2DRenderTargetResource /* not final */ :
    public D2DTrackedResource
{
public:
   explicit D2DRenderTargetResource(D2DRenderer& renderer);

   bool AcquireResource(D2DRenderTarget& target);
   void ReleaseResource(D2DRenderTarget& target);

protected:
   virtual bool DoAcquireResource(D2DRenderTarget& target) = 0;
   virtual void DoReleaseResource(D2DRenderTarget& target) = 0;

};
