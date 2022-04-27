/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  D2DRenderTarget.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <memory>

class D2DRenderTarget;

class D2DRenderTargetResource /* not final */ :
    public std::enable_shared_from_this<D2DRenderTargetResource>
{
public:
   virtual ~D2DRenderTargetResource() = default;

   bool AcquireResource(D2DRenderTarget& target);
   void ReleaseResource(D2DRenderTarget& target);

protected:
   virtual bool DoAcquireResource(D2DRenderTarget& target) = 0;
   virtual void DoReleaseResource(D2DRenderTarget& target) = 0;

};
