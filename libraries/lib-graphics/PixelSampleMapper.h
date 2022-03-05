/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  PixelSampleMapper.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <cstdint>
#include <functional>
#include <variant>

class sampleCount;

class GRAPHICS_API PixelSampleMapper final
{
public:
   PixelSampleMapper() = default;
   PixelSampleMapper(const PixelSampleMapper&) = default;
   PixelSampleMapper(PixelSampleMapper&&) = default;
   PixelSampleMapper& operator=(const PixelSampleMapper&) = default;
   PixelSampleMapper& operator=(PixelSampleMapper&&) = default;

   PixelSampleMapper(double t0, double rate, double samplesPerPixel) noexcept;

   void applyBias(double bias) noexcept;

   double applyCorrection(
      const PixelSampleMapper& oldMapper, size_t oldLen, size_t newLen);

   sampleCount GetFirstSample(uint32_t column) const;
   sampleCount GetLastSample(uint32_t column) const;
   std::pair<sampleCount, sampleCount> GetSampleRange(uint32_t column) const;

   using CustomMapper = std::function<sampleCount(uint32_t)>;
   void setCustomMapper(CustomMapper mapper);

   bool IsValid() const;
   bool IsLinear() const noexcept;

private:
   struct LinearMapper final
   {
      double mInitialValue {};
      double mSamplesPerPixel {};

      sampleCount operator()(uint32_t column) const noexcept;

      explicit operator bool() const noexcept;
   };
   // GCC 9.3.0 fails horribly if you do not initialize variant explicitly here
   std::variant<LinearMapper, CustomMapper> mMapper { LinearMapper {} };
};
