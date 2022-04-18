/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  Brush.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <memory>
#include <vector>
#include <variant>

#include "Color.h"
#include "Point.h"

enum class BrushStyle
{
   None,
   Solid,
   LinearGradient,
};

struct BrushGradientStop final
{
   float position { 0.0f };
   Color color;
};

using BrushGradientStops = std::vector<BrushGradientStop>;

struct BrushGradientData final
{
   Point firstPoint;
   Point secondPoint;

   BrushGradientStops stops;
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

   Brush(Point start, Point end, Color startColor, Color endColor);
   Brush(float startX, float startY, float endX, float endY, Color startColor, Color endColor);

   Brush(Point start, Point end, BrushGradientStops stops);
   Brush(float startX, float startY, float endX, float endY, BrushGradientStops stops);

   ~Brush();

   void SetStyle(BrushStyle style) noexcept;
   BrushStyle GetStyle() const noexcept;

   void SetColor(Color color) noexcept;
   Color GetColor() const noexcept;

   const BrushGradientData* GetGradientData() const noexcept;

   friend GRAPHICS_API bool operator==(const Brush& lhs, const Brush& rhs) noexcept;
   friend GRAPHICS_API bool operator!=(const Brush& lhs, const Brush& rhs) noexcept;

private:
   BrushStyle mStyle { BrushStyle::Solid };
   Color mColor { Colors::Black };

   std::shared_ptr<BrushGradientData> mBrushData;
};
