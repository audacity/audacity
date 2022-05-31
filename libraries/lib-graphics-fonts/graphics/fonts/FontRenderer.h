/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  FontRenderer.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <string_view>

#include "graphics/Point.h"
#include "graphics/Brush.h"

namespace graphics::fonts
{
class Font;
class TextLayout;

class GRAPHICS_FONTS_API FontRenderer /* not final */
{
public:
   virtual ~FontRenderer() noexcept;

   virtual uint32_t GetDPI() const noexcept = 0;
   virtual void Draw(const Font& font, const TextLayout& layout, Color textColor) = 0;
}; // class FontRenderer
} // namespace graphics::fonts
