/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  Pen.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <cstdint>

#include "Color.h"

enum class PenStyle
{
   None,
   Solid,
   Dot,
   LongDash,
   ShortDash,
   DotDash
};

class GRAPHICS_API Pen final
{
public:
   static const Pen NoPen;

   Pen() = default;
   Pen(const Pen&) = default;
   Pen(Pen&&) = default;
   Pen& operator=(const Pen&) = default;
   Pen& operator=(Pen&&) = default;

   Pen(PenStyle style);
   Pen(Color color);
   Pen(Color color, uint32_t width);
   Pen(PenStyle style, Color color);
   Pen(PenStyle style, Color color, uint32_t width);

   void SetStyle(PenStyle style) noexcept;
   PenStyle GetStyle() const noexcept;

   void SetColor(Color color) noexcept;
   Color GetColor() const noexcept;

   void SetWidth(uint32_t width) noexcept;
   uint32_t GetWidth() const noexcept;

   friend GRAPHICS_API bool operator==(const Pen& lhs, const Pen& rhs) noexcept;
   friend GRAPHICS_API bool operator!=(const Pen& lhs, const Pen& rhs) noexcept;

private:
   PenStyle mStyle { PenStyle::Solid };

   Color mColor { Colors::Black };
   uint32_t mWidth { 1 };
};
