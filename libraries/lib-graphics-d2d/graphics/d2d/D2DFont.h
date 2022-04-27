/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  D2DFont.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <dwrite.h>
#include <wrl.h>

#include "graphics/Painter.h"
#include "graphics/FontInfo.h"

class D2DFont final : public PainterFont
{
public:
   D2DFont(
      const RendererID& rendererId, IDWriteFactory* factory, uint32_t dpi,
      const FontInfo& fontInfo);

   std::string_view GetFace() const override;

   float GetFontSize() const override;

   Metrics GetFontMetrics() const override;

   Size GetTextSize(const std::string_view& text) const override;

   bool IsValid() const;

private:
   void UpdateFontMetrics();

   FontInfo mFontInfo;
   Metrics mMetrics;

   IDWriteFactory* mDWriteFactory;
   
   Microsoft::WRL::ComPtr<IDWriteTextFormat> mTextFormat;
};
