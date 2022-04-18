/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  Color.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <algorithm>
#include <cstdint>
#include <optional>

struct GRAPHICS_API Color final
{
public:
   constexpr Color() noexcept : mABGR(0) {}
   constexpr Color(const Color&) noexcept = default;
   constexpr Color(Color&&) noexcept = default;
   Color& operator=(const Color&) noexcept = default;
   Color& operator=(Color&&) noexcept = default;

   constexpr Color(uint8_t rr, uint8_t gg, uint8_t bb, uint8_t aa = 255) noexcept
       : mRed(rr)
       , mGreen(gg)
       , mBlue(bb)
       , mAlpha(aa)
   {
   }

   static constexpr Color FromARGB(uint32_t argb) noexcept
   {
      return Color(argb);
   }

   static constexpr Color FromFloatRGBA(float r, float g, float b, float a) noexcept
   {
      return Color(
         Color::Scale(255, r), Color::Scale(255, g), Color::Scale(255, b),
         Color::Scale(255, a));
   }

   constexpr Color WithOpacity(float opacity) const noexcept
   {
      return Color(mRed, mGreen, mBlue, Scale(mAlpha, opacity));
   }

   constexpr Color WithFloatAlpha(float alpha) const noexcept
   {
      return Color(mRed, mGreen, mBlue, Scale(255, alpha));
   }

   constexpr Color WithAlpha(uint8_t alpha) const noexcept
   {
      return Color(mRed, mGreen, mBlue, alpha);
   }

   constexpr Color WithoutAlpha() const noexcept
   {
      return Color(mRed, mGreen, mBlue, uint8_t(255));
   }

   constexpr uint8_t GetRed() const noexcept
   {
      return mRed;
   }

   constexpr uint8_t GetGreen() const noexcept
   {
      return mGreen;
   }

   constexpr uint8_t GetBlue() const noexcept
   {
      return mBlue;
   }

   constexpr uint8_t GetAlpha() const noexcept
   {
      return mAlpha;
   }

   constexpr uint32_t GetRGBA() const noexcept
   {
      return (uint32_t(mRed) << 24) + (uint32_t(mGreen) << 16) + (uint32_t(mBlue) << 8) +
             (uint32_t(mAlpha));
   }

   constexpr uint32_t GetABGR() const noexcept
   {
      return mABGR;
   }

   constexpr uint32_t GetRGB() const noexcept
   {
      return (uint32_t(mRed) << 16) + (uint32_t(mGreen) << 8) + (uint32_t(mBlue));
   }

   friend constexpr Color operator+(Color lhs, Color rhs) noexcept
   {
      return Color(
         Add(lhs.mRed, rhs.mRed), Add(lhs.mGreen, rhs.mGreen), Add(lhs.mBlue, rhs.mBlue),
         Add(lhs.mAlpha, rhs.mAlpha));
   }

   friend constexpr Color operator-(Color lhs, Color rhs) noexcept
   {
      return Color(
         Sub(lhs.mRed, rhs.mRed), Sub(lhs.mGreen, rhs.mGreen), Sub(lhs.mBlue, rhs.mBlue),
         Sub(lhs.mAlpha, rhs.mAlpha));
   }

   friend constexpr Color operator*(Color lhs, Color rhs) noexcept
   {
      return Color(
         Mul(lhs.mRed, rhs.mRed), Mul(lhs.mGreen, rhs.mGreen), Mul(lhs.mBlue, rhs.mBlue),
         Mul(lhs.mAlpha, rhs.mAlpha));
   }

   template <typename ScaleType>
   friend constexpr Color lerp(Color lhs, Color rhs, ScaleType t);

   Color& operator+=(Color rhs) noexcept
   {
      return *this = *this + rhs;
   }

   Color& operator-=(Color rhs) noexcept
   {
      return *this = *this - rhs;
   }

   Color& operator*=(Color rhs) noexcept
   {
      return *this = *this * rhs;
   }

   bool operator==(const Color& rhs) const noexcept
   {
      return mABGR == rhs.mABGR;
   }
   bool operator!=(const Color& rhs) const noexcept
   {
      return mABGR != rhs.mABGR;
   }
private:
   static constexpr uint8_t Add(uint8_t a, uint8_t b) noexcept
   {
      return uint8_t((a + b) & 0xFF);
   }

   static constexpr uint8_t Sub(uint8_t a, uint8_t b) noexcept
   {
      return (a > b) ? a - b : 0;
   }

   static constexpr uint8_t Mul(uint8_t a, uint8_t b) noexcept
   {
      return uint8_t((uint16_t(a) * uint16_t(b) / 255) & 0xFF);
   }

   static constexpr uint8_t Scale(uint8_t a, float s) noexcept
   {
      return uint8_t(a * std::max(0.0f, std::min(1.0f, s)) + 0.5f);
   }

   template <typename ScaleType>
   static constexpr uint8_t Lerp(uint8_t a, uint8_t b, ScaleType t)
   {
      static_assert(std::is_floating_point<ScaleType>::value);

      const auto lerpValue = (a + (b - a) * t);

      const auto roundLerpValue = static_cast<int16_t>(lerpValue + ScaleType(0.5));

      const auto ui8LerpValue =
         static_cast<uint8_t>(std::max<decltype(roundLerpValue)>(
            0, std::min<decltype(roundLerpValue)>(255, roundLerpValue)));

      return ui8LerpValue;
   }

   explicit constexpr Color(uint32_t abgr) noexcept
       : mABGR(abgr)
   {
   }

   union
   {
      uint32_t mABGR;
      struct
      {
         uint8_t mRed, mGreen, mBlue, mAlpha;
      };
   };
};


template <typename ScaleType>
constexpr Color lerp(Color lhs, Color rhs, ScaleType t)
{
   return { Color::Lerp(lhs.mRed, rhs.mRed, t), Color::Lerp(lhs.mGreen, rhs.mGreen, t),
            Color::Lerp(lhs.mBlue, rhs.mBlue, t), Color::Lerp(lhs.mAlpha, rhs.mAlpha, t) };
}

GRAPHICS_API constexpr Color ColorFromRGA(uint32_t rgba) noexcept
{
   return Color(
      uint8_t((rgba >> 24) & 0xFF), uint8_t((rgba >> 16) & 0xFF),
      uint8_t((rgba >> 8) & 0xFF), uint8_t((rgba >> 0) & 0xFF));
}

GRAPHICS_API constexpr Color ColorFromABGR(uint32_t abgr) noexcept
{
   return Color::FromARGB(abgr);
}

GRAPHICS_API constexpr Color ColorFromRGB(uint32_t rgb) noexcept
{
   return Color(
      uint8_t((rgb >> 16) & 0xFF), uint8_t((rgb >> 8) & 0xFF),
      uint8_t((rgb >> 0) & 0xFF), uint8_t(0xFF));
}

GRAPHICS_API constexpr Color ColorFromARGB(uint32_t argb) noexcept
{
   return Color(
      uint8_t((argb >> 16) & 0xFF), uint8_t((argb >> 8) & 0xFF),
      uint8_t((argb >> 0) & 0xFF), uint8_t((argb >> 24) & 0xFF));
}

GRAPHICS_API constexpr Color
ColorFromFloatRGBA(float r, float g, float b, float a) noexcept
{
   return Color::FromFloatRGBA(r, g, b, a);
}

namespace Colors
{
constexpr Color White = ColorFromABGR(0xFFFFFFFF);
constexpr Color Black = ColorFromABGR(0xFF000000);
constexpr Color Transparent = ColorFromABGR(0x00000000);
constexpr Color Red = ColorFromABGR(0xFF0000FF);
constexpr Color Green = ColorFromABGR(0xFF00FF00);
constexpr Color Blue = ColorFromABGR(0xFFFF0000);
}
