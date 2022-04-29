/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  D2DFontCollection.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <dwrite.h>
#include <map>
#include <memory>

#include "graphics/RendererID.h"

class FontInfo;
class D2DFont;

class D2DFontCollection final
{
public:
   D2DFontCollection(
      const RendererID& rendererId, IDWriteFactory* factory);

   ~D2DFontCollection();

   std::shared_ptr<D2DFont> GetFont(const FontInfo& fontInfo, uint32_t dpi);

private:
   using FontCollection = std::map<std::pair<uint32_t, FontInfo>, std::shared_ptr<D2DFont>>;
   FontCollection mFontCollection;

   RendererID mRendererId;
   IDWriteFactory* mFactory;
};
