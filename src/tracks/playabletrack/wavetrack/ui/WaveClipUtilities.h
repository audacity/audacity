/**********************************************************************

  Audacity: A Digital Audio Editor

  @file WaveClipUtilities.h

  Paul Licameli split from WaveClip.h

*******************************************************************/

#ifndef __AUDACITY_WAVE_CLIP_UTILITIES__
#define __AUDACITY_WAVE_CLIP_UTILITIES__

#include <vector>
#include <cstddef>
#include <cstdint>
#include <variant>

class sampleCount;

class AUDACITY_DLL_API PixelSampleMapper final
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

private:
   struct LinearMapper final
   {
      double mInitialValue {};
      double mSamplesPerPixel {};

      sampleCount operator()(uint32_t column) const noexcept;

      explicit operator bool() const noexcept;
   };

   std::variant<LinearMapper, CustomMapper> mMapper;
};

AUDACITY_DLL_API
void findCorrection(const std::vector<sampleCount> &oldWhere, size_t oldLen,
   size_t newLen, double t0, double rate, double samplesPerPixel,
   int &oldX0, double &correction);

AUDACITY_DLL_API
void fillWhere(std::vector<sampleCount> &where,
   size_t len, double bias, double correction,
   double t0, double rate, double samplesPerPixel);

#endif
