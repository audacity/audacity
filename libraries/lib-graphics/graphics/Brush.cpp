/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  Brush.cpp

  Dmitry Vedenko

**********************************************************************/
#include "Brush.h"

#include <algorithm>

namespace graphics
{

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

Brush::Brush(Point start, Point end, Color startColor, Color endColor)
    : Brush(
         start, end,
         BrushGradientStops({ { 0.0f, startColor }, { 1.0f, endColor } }))
{
}

Brush::Brush(
   float startX, float startY, float endX, float endY, Color startColor,
   Color endColor)
    : Brush(
         Point { startX, startY }, Point { endX, endY }, startColor, endColor)
{
}

Brush::Brush(Point start, Point end, BrushGradientStops stops)
    : mStyle(BrushStyle::LinearGradient)
    , mBrushData(std::make_shared<BrushGradientData>())
{
   mBrushData->firstPoint = start;
   mBrushData->secondPoint = end;
   mBrushData->stops = std::move(stops);
}

Brush::Brush(
   float startX, float startY, float endX, float endY, BrushGradientStops stops)
    : Brush(Point { startX, startY }, Point { endX, endY }, std::move(stops))
{
}

Brush::~Brush()
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

const BrushGradientData* Brush::GetGradientData() const noexcept
{
   return mBrushData.get();
}

namespace
{
bool CompareBrushGradientData(
   const std::shared_ptr<BrushGradientData>& lhs,
   const std::shared_ptr<BrushGradientData>& rhs)
{
   if (lhs == rhs)
      return true;

   if (!lhs || !rhs)
      return false;

   return lhs->firstPoint == rhs->firstPoint &&
          lhs->secondPoint == rhs->secondPoint &&
          lhs->stops.size() == rhs->stops.size() &&
          std::equal(
             lhs->stops.begin(), lhs->stops.end(), rhs->stops.begin(),
             rhs->stops.end(),
             [](BrushGradientStop lhs, BrushGradientStop rhs) {
                return lhs.position == rhs.position && lhs.color == rhs.color;
             });
}
} // namespace

bool operator==(const Brush& lhs, const Brush& rhs) noexcept
{
   return lhs.mStyle == rhs.mStyle && lhs.mColor == rhs.mColor &&
          CompareBrushGradientData(lhs.mBrushData, rhs.mBrushData);
}

bool operator!=(const Brush& lhs, const Brush& rhs) noexcept
{
   return !(lhs == rhs);
}

} // namespace graphics
