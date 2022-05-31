/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  MacosFontProvider.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include "graphics/fonts/FontProvider.h"

namespace graphics::fonts::platforms::macos
{

class MacosFontProvider final : public FontProvider
{
public:
 std::unique_ptr<FontStream> GetFontStream(const FontInfo& fontInfo) override;

}; // class MacosFontProvider

} // namespace graphics::fonts::platforms::macos
