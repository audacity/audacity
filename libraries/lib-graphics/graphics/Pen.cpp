/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  Pen.cpp

  Dmitry Vedenko

**********************************************************************/
#include "Pen.h"

namespace graphics
{

const Pen Pen::NoPen(PenStyle::None);

Pen::Pen(PenStyle style)
    : Pen(style, Colors::Black, 1)
{
}

Pen::Pen(Color color)
    : Pen(PenStyle::Solid, color, 1)
{
}

Pen::Pen(Color color, uint32_t width)
    : Pen(PenStyle::Solid, color, width)
{
}

Pen::Pen(PenStyle style, Color color)
    : Pen(style, color, 1)
{
}

Pen::Pen(PenStyle style, Color color, uint32_t width)
    : mStyle(style)
    , mColor(color)
    , mWidth(width)
{
}

void Pen::SetStyle(PenStyle style) noexcept
{
   mStyle = style;
}

PenStyle Pen::GetStyle() const noexcept
{
   return mStyle;
}

void Pen::SetColor(Color color) noexcept
{
   mColor = color;
}

Color Pen::GetColor() const noexcept
{
   return mColor;
}

void Pen::SetWidth(uint32_t width) noexcept
{
   mWidth = width;
}

uint32_t Pen::GetWidth() const noexcept
{
   return mWidth;
}

bool operator==(const Pen& lhs, const Pen& rhs) noexcept
{
   return lhs.mStyle == rhs.mStyle && lhs.mColor == rhs.mColor &&
          lhs.mWidth == rhs.mWidth;
}

bool operator!=(const Pen& lhs, const Pen& rhs) noexcept
{
   return !(lhs == rhs);
}

} // namespace graphics
