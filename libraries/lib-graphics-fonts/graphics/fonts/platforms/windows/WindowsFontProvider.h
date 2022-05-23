/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  WindowsFontProvider.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include "graphics/fonts/FontProvider.h"

namespace graphics::fonts::platforms::windows
{
   
class WindowsFontProvider final : public FontProvider
{
public:
 std::unique_ptr<FontStream> GetFontStream(const FontInfo& fontInfo) override;

}; // class WindowsFontProvider

} // namespace graphics::fonts::platforms::windows
