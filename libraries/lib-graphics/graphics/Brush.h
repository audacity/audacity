/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  Brush.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include "Color.h"

enum class BrushStyle
{
   None,
   Solid
};

class GRAPHICS_API Brush final
{
public:
   static const Brush NoBrush;

   Brush() = default;
   Brush(const Brush&) = default;
   Brush(Brush&&) = default;
   Brush& operator=(const Brush&) = default;
   Brush& operator=(Brush&&) = default;

   Brush(BrushStyle style);
   Brush(Color color);
   Brush(BrushStyle style, Color color);

   void SetStyle(BrushStyle style) noexcept;
   BrushStyle GetStyle() const noexcept;

   void SetColor(Color color) noexcept;
   Color GetColor() const noexcept;

   friend GRAPHICS_API bool operator==(const Brush& lhs, const Brush& rhs) noexcept;
   friend GRAPHICS_API bool operator!=(const Brush& lhs, const Brush& rhs) noexcept;

private:
   BrushStyle mStyle { BrushStyle::Solid };
   Color mColor { Colors::Black };
};
