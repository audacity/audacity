/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  D2DWindowRenderTarget.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <d2d1.h>
#include <wrl.h>

#include "graphics/d2d/D2DRenderTarget.h"

namespace graphics::d2d
{

class D2DRenderer;

class D2DWindowRenderTarget : public D2DRenderTarget
{
public:
   D2DWindowRenderTarget(D2DRenderer& renderer, HWND hwnd);

   void BeginDraw() override;

   D2D1_SIZE_U GetD2DSize() const;

   Size GetSize() const override;

   bool IsValid() const;

protected:
   void HandleContextLoss() override;

private:
   void CreateRenderTarget();

   const D2DRenderer& mRenderer;
   HWND mHWND;

   D2D1_SIZE_U mCurrentSize {};

   Microsoft::WRL::ComPtr<ID2D1HwndRenderTarget> mHWNDRenderTarget;
};

} // namespace graphics::d2d
