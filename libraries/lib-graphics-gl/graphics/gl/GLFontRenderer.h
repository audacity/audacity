/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  GLFontRenderer.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <memory>

#include "graphics/fonts/FontRenderer.h"

namespace graphics::gl
{
class PaintTarget;
class GLRenderer;
class Context;

class GLFontRenderer final : public fonts::FontRenderer
{
public:
   explicit GLFontRenderer(GLRenderer& renderer);
   ~GLFontRenderer();

   void SetCurrentPaintTarget(PaintTarget& paintTarget, Context& context);
   void ResetPaintTarget();

   uint32_t GetDPI() const noexcept override;
   bool IsHinted() const noexcept override;

   void
   Draw(const fonts::Font& font, const fonts::TextLayout& layout, Color textColor) override;

   void UpdateGPUCache();

   void SetScale(float scale) noexcept;
   void SetHintingEnabled(bool hinting) noexcept;

private:
   GLRenderer& mRenderer;
   
   PaintTarget* mCurrentPaintTarget { nullptr };
   Context* mContext { nullptr };

   class SymbolsCache;
   std::unique_ptr<SymbolsCache> mSymbolsCache;

   float mScale { 1.0f };
   
   bool mHintingEnabled { true };
};
} // namespace graphics::gl
