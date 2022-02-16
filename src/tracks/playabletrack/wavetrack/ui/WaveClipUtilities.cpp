/**********************************************************************

  Audacity: A Digital Audio Editor

  @file WaveClipUtilities.cpp

  Paul Licameli split from WaveClip.cpp

**********************************************************************/

#include "WaveClipUtilities.h"

#include <cmath>
#include <cassert>

#include <wx/debug.h>
#include "SampleCount.h"
#include "MemoryX.h"

void findCorrection(const std::vector<sampleCount> &oldWhere, size_t oldLen,
   size_t newLen, double t0, double rate, double samplesPerPixel,
   int &oldX0, double &correction)
{
   // Mitigate the accumulation of location errors
   // in copies of copies of ... of caches.
   // Look at the loop that populates "where" below to understand this.

   // Find the sample position that is the origin in the old cache.
   const double oldWhere0 = oldWhere[1].as_double() - samplesPerPixel;
   const double oldWhereLast = oldWhere0 + oldLen * samplesPerPixel;
   // Find the length in samples of the old cache.
   const double denom = oldWhereLast - oldWhere0;

   // What sample would go in where[0] with no correction?
   const double guessWhere0 = t0 * rate;

   if ( // Skip if old and NEW are disjoint:
      oldWhereLast <= guessWhere0 ||
      guessWhere0 + newLen * samplesPerPixel <= oldWhere0 ||
      // Skip unless denom rounds off to at least 1.
      denom < 0.5)
   {
      // The computation of oldX0 in the other branch
      // may underflow and the assertion would be violated.
      oldX0 =  oldLen;
      correction = 0.0;
   }
   else
   {
      // What integer position in the old cache array does that map to?
      // (even if it is out of bounds)
      oldX0 = floor(0.5 + oldLen * (guessWhere0 - oldWhere0) / denom);
      // What sample count would the old cache have put there?
      const double where0 = oldWhere0 + double(oldX0) * samplesPerPixel;
      // What correction is needed to align the NEW cache with the old?
      const double correction0 = where0 - guessWhere0;
      correction = std::max(-samplesPerPixel, std::min(samplesPerPixel, correction0));
      wxASSERT(correction == correction0);
   }
}

void fillWhere(
   std::vector<sampleCount> &where, size_t len, double bias, double correction,
   double t0, double rate, double samplesPerPixel)
{
   // Be careful to make the first value non-negative
   const double w0 = 0.5 + correction + bias + t0 * rate;
   where[0] = sampleCount( std::max(0.0, floor(w0)) );
   // DV: Note, that the loop below potentially makes where to be non monotonic
   for (decltype(len) x = 1; x < len + 1; x++)
      where[x] = sampleCount( floor(w0 + double(x) * samplesPerPixel) );
}

PixelSampleMapper::PixelSampleMapper(
   double t0, double rate, double samplesPerPixel) noexcept
    : mMapper(LinearMapper { (0.5 + t0 * rate), samplesPerPixel })
{
   assert((0.5 + t0 * rate) >= 0.0);
}

void PixelSampleMapper::applyBias(double bias) noexcept
{
   if (mMapper.index() == 0)
      std::get_if<0>(&mMapper)->mInitialValue += bias;
}

double PixelSampleMapper::applyCorrection(
   const PixelSampleMapper& oldMapper, size_t oldLen, size_t newLen)
{
   assert(mMapper.index() == 0);
   assert(oldMapper.mMapper.index() == 0);

   LinearMapper* currentMapper = std::get_if<LinearMapper>(&mMapper);

   if (currentMapper == nullptr)
      return {};

   const LinearMapper* oldLinearMapper =
      std::get_if<LinearMapper>(&oldMapper.mMapper);

   if (oldLinearMapper == nullptr)
      return {};

   // Find the sample position that is the origin in the old cache.
   const double oldWhere0 =
      (*oldLinearMapper)(1).as_double() - currentMapper->mSamplesPerPixel;
   const double oldWhereLast =
      oldWhere0 + oldLen * currentMapper->mSamplesPerPixel;
   // Find the length in samples of the old cache.
   const double denom = oldWhereLast - oldWhere0;

   // What sample would go in where[0] with no correction?
   const double guessWhere0 =
      currentMapper->mInitialValue - 0.5; // Why do we ignore initial bias?

   if ( // Skip if old and NEW are disjoint:
      oldWhereLast <= guessWhere0 ||
      guessWhere0 + newLen * currentMapper->mSamplesPerPixel <= oldWhere0 ||
      // Skip unless denom rounds off to at least 1.
      denom < 0.5)
   {
      // The computation of oldX0 in the other branch
      // may underflow and the assertion would be violated.
      return oldLen;
   }
   else
   {
      // What integer position in the old cache array does that map to?
      // (even if it is out of bounds)
      const auto oldX0 = std::floor(0.5 + oldLen * (guessWhere0 - oldWhere0) / denom);
      // What sample count would the old cache have put there?
      const double where0 =
         oldWhere0 + double(oldX0) * currentMapper->mSamplesPerPixel;
      // What correction is needed to align the NEW cache with the old?
      const double correction0 = where0 - guessWhere0;
      const double correction = std::max(
         -currentMapper->mSamplesPerPixel,
         std::min(currentMapper->mSamplesPerPixel, correction0));

      wxASSERT(correction == correction0);

      currentMapper->mInitialValue += correction;

      return oldX0;
   }
}

sampleCount PixelSampleMapper::GetFirstSample(uint32_t column) const
{
   return Visit([column](const auto& mapper) {
      return mapper(column);
   }, mMapper);
}

sampleCount PixelSampleMapper::GetLastSample(uint32_t column) const
{
   return GetFirstSample(column + 1);
}

std::pair<sampleCount, sampleCount>
PixelSampleMapper::GetSampleRange(uint32_t column) const
{
   return { GetFirstSample(column), GetLastSample(column) };
}

void PixelSampleMapper::setCustomMapper(CustomMapper mapper)
{
   mMapper = std::move(mapper);
}

bool PixelSampleMapper::IsValid() const
{
   return Visit([](const auto& mapper) { return !!mapper; }, mMapper);
}

sampleCount PixelSampleMapper::LinearMapper::operator()(uint32_t column) const noexcept
{
   // Previous code used floor, but "required" mInitialValue to be positive.
   // For positive values, let's just trunc the value, as it is at least twice
   // as fast.
   return sampleCount(mInitialValue + column * mSamplesPerPixel);
}

PixelSampleMapper::LinearMapper::operator bool() const noexcept
{
   return mSamplesPerPixel > 0.0;
}
