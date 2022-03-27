/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  Brush.cpp

  Dmitry Vedenko

**********************************************************************/
#include "Brush.h"

const Brush Brush::NoBrush(BrushStyle::None);

Brush::Brush(BrushStyle style)
    : Brush(style, Colors::Black)
{
}

Brush::Brush(Color color)
    : Brush(BrushStyle::Solid, color)
{
}

Brush::Brush(BrushStyle style, Color color)
    : mStyle(style)
    , mColor(color)
{
}

void Brush::SetStyle(BrushStyle style) noexcept
{
   mStyle = style;
}

BrushStyle Brush::GetStyle() const noexcept
{
   return mStyle;
}

void Brush::SetColor(Color color) noexcept
{
   mColor = color;
}

Color Brush::GetColor() const noexcept
{
   return mColor;
}

bool operator==(const Brush& lhs, const Brush& rhs) noexcept
{
   return lhs.mStyle == rhs.mStyle && lhs.mColor == rhs.mColor;
}

bool operator!=(const Brush& lhs, const Brush& rhs) noexcept
{
   return !(lhs == rhs);
}
