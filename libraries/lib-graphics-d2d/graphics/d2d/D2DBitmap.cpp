/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  D2DBitmap.h

  Dmitry Vedenko

**********************************************************************/

#include "D2DBitmap.h"
#include "D2DRenderer.h"

D2DBitmap::D2DBitmap(D2DRenderer& renderer)
    : PainterImage(renderer.GetRendererID())
    , D2DRenderTargetResource(renderer)
{
}
