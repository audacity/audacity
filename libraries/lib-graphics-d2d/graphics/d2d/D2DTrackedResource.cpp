/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  D2DTrackedResource.cpp

  Dmitry Vedenko

**********************************************************************/
#include "D2DTrackedResource.h"

#include "D2DRenderer.h"

namespace graphics::d2d
{

D2DTrackedResource::D2DTrackedResource(D2DRenderer& renderer)
    : mRenderer(renderer)
    , mShutdownSubscription(renderer.Subscribe([this](const D2DShutdownMessage&)
                                               { CleanupDirect2DResources(); }))
{
}

D2DRenderer& D2DTrackedResource::GetRenderer() noexcept
{
   return mRenderer;
}

const D2DRenderer& D2DTrackedResource::GetRenderer() const noexcept
{
   return mRenderer;
}

} // namespace graphics::d2d
