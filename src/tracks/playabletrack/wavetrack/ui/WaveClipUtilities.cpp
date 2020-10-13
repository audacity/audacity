/**********************************************************************

  Audacity: A Digital Audio Editor

  @file WaveClipUtilities.cpp

  Paul Licameli split from WaveClip.cpp

**********************************************************************/

#include "WaveClipUtilities.h"

#include <cmath>
#include <wx/debug.h>
#include "SampleCount.h"

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
   for (decltype(len) x = 1; x < len + 1; x++)
      where[x] = sampleCount( floor(w0 + double(x) * samplesPerPixel) );
}
