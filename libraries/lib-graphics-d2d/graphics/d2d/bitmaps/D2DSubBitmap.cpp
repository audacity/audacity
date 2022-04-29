/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  D2DWICBitmap.cpp

  Dmitry Vedenko

**********************************************************************/
#include "D2DSubBitmap.h"

#include "graphics/d2d/D2DRenderTarget.h"

#include <algorithm>

D2DSubBitmap::D2DSubBitmap(std::shared_ptr<D2DBitmap> parent, const Rect& rect)
    : D2DBitmap(parent->GetRenderer())
    , mParent(std::move(parent))
    , mRect(rect)
{
}

void D2DSubBitmap::DrawBitmap(
   D2DRenderTarget& target, const Rect& targetRect, const Rect& sourceRect)
{
   const auto maxAvailableWidth = std::max(
      0.0f, std::min(GetWidth() - sourceRect.Origin.x, sourceRect.Size.width));

   const auto maxAvailableHeight = std::max(
      0.0f, std::min(GetHeight() - sourceRect.Origin.y, sourceRect.Size.height));
   
   Rect updatedSourceRect { sourceRect.Origin + mRect.Origin,
                            Size { maxAvailableWidth, maxAvailableHeight } };

   if (updatedSourceRect.Size.IsZero())
      return;
   
   mParent->DrawBitmap(target, targetRect, updatedSourceRect);
}

std::shared_ptr<D2DRenderTarget>
D2DSubBitmap::GetRenderTarget(D2DRenderTarget& parentRenderTarget)
{
   auto renderTarget = mParent->GetRenderTarget(parentRenderTarget);

   if (renderTarget)
      renderTarget->SetClipRect(mRect);

   return renderTarget;
}

void D2DSubBitmap::DrawFinished(D2DRenderTarget& renderTarget)
{
   mParent->DrawFinished(renderTarget);
}

uint32_t D2DSubBitmap::GetWidth() const
{
   return mRect.Size.width;
}

uint32_t D2DSubBitmap::GetHeight() const
{
   return mRect.Size.height;
}

bool D2DSubBitmap::IsValid(Painter& painter) const
{
   return mParent->IsValid(painter);
}

bool D2DSubBitmap::DoAcquireResource(D2DRenderTarget& target)
{
   return AcquireResource(target);
}

void D2DSubBitmap::DoReleaseResource(D2DRenderTarget& target)
{
   return mParent->ReleaseResource(target);
}

void D2DSubBitmap::CleanupDirect2DResources()
{
}
