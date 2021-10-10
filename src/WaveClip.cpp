/**********************************************************************

  Audacity: A Digital Audio Editor

  WaveClip.cpp

  ?? Dominic Mazzoni
  ?? Markus Meyer

*******************************************************************//**

\class WaveClip
\brief This allows multiple clips to be a part of one WaveTrack.

*//****************************************************************//**

\class WaveCache
\brief Cache used with WaveClip to cache wave information (for drawing).

*//*******************************************************************/

#include "WaveClip.h"



#include <math.h>
#include <vector>
#include <wx/log.h>

#include "Sequence.h"
#include "Spectrum.h"
#include "Prefs.h"
#include "Envelope.h"
#include "Resample.h"
#include "WaveTrack.h"
#include "Profiler.h"
#include "InconsistencyException.h"
#include "UserException.h"

#include "prefs/SpectrogramSettings.h"
#include "widgets/ProgressDialog.h"

#ifdef _OPENMP
#include <omp.h>
#endif

class WaveCache {
public:
   WaveCache()
      : dirty(-1)
      , start(-1)
      , pps(0)
      , rate(-1)
      , where(0)
      , min(0)
      , max(0)
      , rms(0)
      , bl(0)
   {
   }

   WaveCache(size_t len_, double pixelsPerSecond, double rate_, double t0, int dirty_)
      : dirty(dirty_)
      , len(len_)
      , start(t0)
      , pps(pixelsPerSecond)
      , rate(rate_)
      , where(1 + len)
      , min(len)
      , max(len)
      , rms(len)
      , bl(len)
   {
   }

   ~WaveCache()
   {
   }

   int          dirty;
   const size_t len { 0 }; // counts pixels, not samples
   const double start;
   const double pps;
   const int    rate;
   std::vector<sampleCount> where;
   std::vector<float> min;
   std::vector<float> max;
   std::vector<float> rms;
   std::vector<int> bl;
};

static void ComputeSpectrumUsingRealFFTf
   (float * __restrict buffer, const FFTParam *hFFT,
    const float * __restrict window, size_t len, float * __restrict out)
{
   size_t i;
   if(len > hFFT->Points * 2)
      len = hFFT->Points * 2;
   for(i = 0; i < len; i++)
      buffer[i] *= window[i];
   for( ; i < (hFFT->Points * 2); i++)
      buffer[i] = 0; // zero pad as needed
   RealFFTf(buffer, hFFT);
   // Handle the (real-only) DC
   float power = buffer[0] * buffer[0];
   if(power <= 0)
      out[0] = -160.0;
   else
      out[0] = 10.0 * log10f(power);
   for(i = 1; i < hFFT->Points; i++) {
      const int index = hFFT->BitReversed[i];
      const float re = buffer[index], im = buffer[index + 1];
      power = re * re + im * im;
      if(power <= 0)
         out[i] = -160.0;
      else
         out[i] = 10.0*log10f(power);
   }
}

WaveClip::WaveClip(const SampleBlockFactoryPtr &factory,
                   sampleFormat format, int rate, int colourIndex)
{
   mRate = rate;
   mColourIndex = colourIndex;
   mSequence = std::make_unique<Sequence>(factory, format);

   mEnvelope = std::make_unique<Envelope>(true, 1e-7, 2.0, 1.0);

   mWaveCache = std::make_unique<WaveCache>();
   mSpecCache = std::make_unique<SpecCache>();
   mSpecPxCache = std::make_unique<SpecPxCache>(1);
}

WaveClip::WaveClip(const WaveClip& orig,
                   const SampleBlockFactoryPtr &factory,
                   bool copyCutlines)
{
   // essentially a copy constructor - but you must pass in the
   // current sample block factory, because we might be copying
   // from one project to another

   mSequenceOffset = orig.mSequenceOffset;
   mTrimLeft = orig.mTrimLeft;
   mTrimRight = orig.mTrimRight;
   mRate = orig.mRate;
   mColourIndex = orig.mColourIndex;
   mSequence = std::make_unique<Sequence>(*orig.mSequence, factory);

   mEnvelope = std::make_unique<Envelope>(*orig.mEnvelope);

   mWaveCache = std::make_unique<WaveCache>();
   mSpecCache = std::make_unique<SpecCache>();
   mSpecPxCache = std::make_unique<SpecPxCache>(1);

   mName = orig.mName;

   if ( copyCutlines )
      for (const auto &clip: orig.mCutLines)
         mCutLines.push_back
            ( std::make_unique<WaveClip>( *clip, factory, true ) );

   mIsPlaceholder = orig.GetIsPlaceholder();
}

WaveClip::WaveClip(const WaveClip& orig,
                   const SampleBlockFactoryPtr &factory,
                   bool copyCutlines,
                   double t0, double t1)
{
   // Copy only a range of the other WaveClip

   mSequenceOffset = orig.mSequenceOffset;
   
   mRate = orig.mRate;
   mColourIndex = orig.mColourIndex;

   mWaveCache = std::make_unique<WaveCache>();
   mSpecCache = std::make_unique<SpecCache>();
   mSpecPxCache = std::make_unique<SpecPxCache>(1);

   mIsPlaceholder = orig.GetIsPlaceholder();

   auto s0 = orig.TimeToSequenceSamples(t0);
   auto s1 = orig.TimeToSequenceSamples(t1);

   mSequence = orig.mSequence->Copy(factory, s0, s1);

   mEnvelope = std::make_unique<Envelope>(
      *orig.mEnvelope,
      GetSequenceStartTime() + s0.as_double()/mRate,
      GetSequenceStartTime() + s1.as_double()/mRate
   );

   if ( copyCutlines )
      // Copy cutline clips that fall in the range
      for (const auto &ppClip : orig.mCutLines)
      {
         const WaveClip* clip = ppClip.get();
         double cutlinePosition = orig.GetSequenceStartTime() + clip->GetSequenceStartTime();
         if (cutlinePosition >= t0 && cutlinePosition <= t1)
         {
            auto newCutLine =
               std::make_unique< WaveClip >( *clip, factory, true );
            newCutLine->SetSequenceStartTime( cutlinePosition - t0 );
            mCutLines.push_back(std::move(newCutLine));
         }
      }
}


WaveClip::~WaveClip()
{
}

bool WaveClip::GetSamples(samplePtr buffer, sampleFormat format,
                   sampleCount start, size_t len, bool mayThrow) const
{
   return mSequence->Get(buffer, format, start + TimeToSamples(mTrimLeft), len, mayThrow);
}

/*! @excsafety{Strong} */
void WaveClip::SetSamples(constSamplePtr buffer, sampleFormat format,
                   sampleCount start, size_t len)
{
   // use Strong-guarantee
   mSequence->SetSamples(buffer, format, start + TimeToSamples(mTrimLeft), len);

   // use No-fail-guarantee
   MarkChanged();
}

BlockArray* WaveClip::GetSequenceBlockArray()
{
   return &mSequence->GetBlockArray();
}

const BlockArray* WaveClip::GetSequenceBlockArray() const
{
   return &mSequence->GetBlockArray();
}

///Delete the wave cache - force redraw.  Thread-safe
void WaveClip::ClearWaveCache()
{
   mWaveCache = std::make_unique<WaveCache>();
}

namespace {

inline
void findCorrection(const std::vector<sampleCount> &oldWhere, size_t oldLen,
         size_t newLen,
         double t0, double rate, double samplesPerPixel,
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

inline void
fillWhere(std::vector<sampleCount> &where, size_t len, double bias, double correction,
          double t0, double rate, double samplesPerPixel)
{
   // Be careful to make the first value non-negative
   const double w0 = 0.5 + correction + bias + t0 * rate;
   where[0] = sampleCount( std::max(0.0, floor(w0)) );
   for (decltype(len) x = 1; x < len + 1; x++)
      where[x] = sampleCount( floor(w0 + double(x) * samplesPerPixel) );
}

}

//
// Getting high-level data from the track for screen display and
// clipping calculations
//

bool WaveClip::GetWaveDisplay(WaveDisplay &display, double t0,
                               double pixelsPerSecond) const
{
   t0 += GetTrimLeft();

   const bool allocated = (display.where != 0);

   const size_t numPixels = (int)display.width;

   size_t p0 = 0;         // least column requiring computation
   size_t p1 = numPixels; // greatest column requiring computation, plus one

   float *min;
   float *max;
   float *rms;
   int *bl;
   std::vector<sampleCount> *pWhere;

   if (allocated) {
      // assume ownWhere is filled.
      min = &display.min[0];
      max = &display.max[0];
      rms = &display.rms[0];
      bl = &display.bl[0];
      pWhere = &display.ownWhere;
   }
   else {
      const double tstep = 1.0 / pixelsPerSecond;
      const double samplesPerPixel = mRate * tstep;

      // Make a tolerant comparison of the pps values in this wise:
      // accumulated difference of times over the number of pixels is less than
      // a sample period.
      const bool ppsMatch = mWaveCache &&
         (fabs(tstep - 1.0 / mWaveCache->pps) * numPixels < (1.0 / mRate));

      const bool match =
         mWaveCache &&
         ppsMatch &&
         mWaveCache->len > 0 &&
         mWaveCache->dirty == mDirty;

      if (match &&
         mWaveCache->start == t0 &&
         mWaveCache->len >= numPixels) {

         // Satisfy the request completely from the cache
         display.min = &mWaveCache->min[0];
         display.max = &mWaveCache->max[0];
         display.rms = &mWaveCache->rms[0];
         display.bl = &mWaveCache->bl[0];
         display.where = &mWaveCache->where[0];
         return true;
      }

      std::unique_ptr<WaveCache> oldCache(std::move(mWaveCache));

      int oldX0 = 0;
      double correction = 0.0;
      size_t copyBegin = 0, copyEnd = 0;
      if (match) {
         findCorrection(oldCache->where, oldCache->len, numPixels,
            t0, mRate, samplesPerPixel,
            oldX0, correction);
         // Remember our first pixel maps to oldX0 in the old cache,
         // possibly out of bounds.
         // For what range of pixels can data be copied?
         copyBegin = std::min<size_t>(numPixels, std::max(0, -oldX0));
         copyEnd = std::min<size_t>(numPixels, std::max(0,
            (int)oldCache->len - oldX0
         ));
      }
      if (!(copyEnd > copyBegin))
         oldCache.reset(0);

      mWaveCache = std::make_unique<WaveCache>(numPixels, pixelsPerSecond, mRate, t0, mDirty);
      min = &mWaveCache->min[0];
      max = &mWaveCache->max[0];
      rms = &mWaveCache->rms[0];
      bl = &mWaveCache->bl[0];
      pWhere = &mWaveCache->where;

      fillWhere(*pWhere, numPixels, 0.0, correction,
         t0, mRate, samplesPerPixel);

      // The range of pixels we must fetch from the Sequence:
      p0 = (copyBegin > 0) ? 0 : copyEnd;
      p1 = (copyEnd >= numPixels) ? copyBegin : numPixels;

      // Optimization: if the old cache is good and overlaps
      // with the current one, re-use as much of the cache as
      // possible

      if (oldCache) {

         // Copy what we can from the old cache.
         const int length = copyEnd - copyBegin;
         const size_t sizeFloats = length * sizeof(float);
         const int srcIdx = (int)copyBegin + oldX0;
         memcpy(&min[copyBegin], &oldCache->min[srcIdx], sizeFloats);
         memcpy(&max[copyBegin], &oldCache->max[srcIdx], sizeFloats);
         memcpy(&rms[copyBegin], &oldCache->rms[srcIdx], sizeFloats);
         memcpy(&bl[copyBegin], &oldCache->bl[srcIdx], length * sizeof(int));
      }
   }

   if (p1 > p0) {
      // Cache was not used or did not satisfy the whole request
      std::vector<sampleCount> &where = *pWhere;

      /* handle values in the append buffer */

      auto numSamples = mSequence->GetNumSamples();
      auto a = p0;

      // Not all of the required columns might be in the sequence.
      // Some might be in the append buffer.
      for (; a < p1; ++a) {
         if (where[a + 1] > numSamples)
            break;
      }

      // Handle the columns that land in the append buffer.
      //compute the values that are outside the overlap from scratch.
      if (a < p1) {
         sampleFormat seqFormat = mSequence->GetSampleFormat();
         bool didUpdate = false;
         for(auto i = a; i < p1; i++) {
            auto left = std::max(sampleCount{ 0 },
                                 where[i] - numSamples);
            auto right = std::min(sampleCount{ mAppendBufferLen },
                                  where[i + 1] - numSamples);

            //wxCriticalSectionLocker locker(mAppendCriticalSection);

            if (right > left) {
               Floats b;
               float *pb{};
               // left is nonnegative and at most mAppendBufferLen:
               auto sLeft = left.as_size_t();
               // The difference is at most mAppendBufferLen:
               size_t len = ( right - left ).as_size_t();

               if (seqFormat == floatSample)
                  pb = &((float *)mAppendBuffer.ptr())[sLeft];
               else {
                  b.reinit(len);
                  pb = b.get();
                  SamplesToFloats(
                     mAppendBuffer.ptr() + sLeft * SAMPLE_SIZE(seqFormat),
                     seqFormat, pb, len);
               }

               float theMax, theMin, sumsq;
               {
                  const float val = pb[0];
                  theMax = theMin = val;
                  sumsq = val * val;
               }
               for(decltype(len) j = 1; j < len; j++) {
                  const float val = pb[j];
                  theMax = std::max(theMax, val);
                  theMin = std::min(theMin, val);
                  sumsq += val * val;
               }

               min[i] = theMin;
               max[i] = theMax;
               rms[i] = (float)sqrt(sumsq / len);
               bl[i] = 1; //for now just fake it.

               didUpdate=true;
            }
         }

         // Shrink the right end of the range to fetch from Sequence
         if(didUpdate)
            p1 = a;
      }

      // Done with append buffer, now fetch the rest of the cache miss
      // from the sequence
      if (p1 > p0) {
         if (!mSequence->GetWaveDisplay(&min[p0],
                                        &max[p0],
                                        &rms[p0],
                                        &bl[p0],
                                        p1-p0,
                                        &where[p0]))
         {
            return false;
         }
      }
   }

   if (!allocated) {
      // Now report the results
      display.min = min;
      display.max = max;
      display.rms = rms;
      display.bl = bl;
      display.where = &(*pWhere)[0];
   }

   return true;
}

namespace {

void ComputeSpectrogramGainFactors
   (size_t fftLen, double rate, int frequencyGain, std::vector<float> &gainFactors)
{
   if (frequencyGain > 0) {
      // Compute a frequency-dependent gain factor
      // scaled such that 1000 Hz gets a gain of 0dB

      // This is the reciprocal of the bin number of 1000 Hz:
      const double factor = ((double)rate / (double)fftLen) / 1000.0;

      auto half = fftLen / 2;
      gainFactors.reserve(half);
      // Don't take logarithm of zero!  Let bin 0 replicate the gain factor for bin 1.
      gainFactors.push_back(frequencyGain*log10(factor));
      for (decltype(half) x = 1; x < half; x++) {
         gainFactors.push_back(frequencyGain*log10(factor * x));
      }
   }
}

}

bool SpecCache::Matches
   (int dirty_, double pixelsPerSecond,
    const SpectrogramSettings &settings, double rate) const
{
   // Make a tolerant comparison of the pps values in this wise:
   // accumulated difference of times over the number of pixels is less than
   // a sample period.
   const double tstep = 1.0 / pixelsPerSecond;
   const bool ppsMatch =
      (fabs(tstep - 1.0 / pps) * len < (1.0 / rate));

   return
      ppsMatch &&
      dirty == dirty_ &&
      windowType == settings.windowType &&
      windowSize == settings.WindowSize() &&
      zeroPaddingFactor == settings.ZeroPaddingFactor() &&
      frequencyGain == settings.frequencyGain &&
      algorithm == settings.algorithm;
}

bool SpecCache::CalculateOneSpectrum
   (const SpectrogramSettings &settings,
    WaveTrackCache &waveTrackCache,
    const int xx, const sampleCount numSamples,
    double offset, double rate, double pixelsPerSecond,
    int lowerBoundX, int upperBoundX,
    const std::vector<float> &gainFactors,
    float* __restrict scratch, float* __restrict out) const
{
   bool result = false;
   const bool reassignment =
      (settings.algorithm == SpectrogramSettings::algReassignment);
   const size_t windowSizeSetting = settings.WindowSize();

   sampleCount from;

   // xx may be for a column that is out of the visible bounds, but only
   // when we are calculating reassignment contributions that may cross into
   // the visible area.

   if (xx < 0)
      from = sampleCount(
         where[0].as_double() + xx * (rate / pixelsPerSecond)
      );
   else if (xx > (int)len)
      from = sampleCount(
         where[len].as_double() + (xx - len) * (rate / pixelsPerSecond)
      );
   else
      from = where[xx];

   const bool autocorrelation =
      settings.algorithm == SpectrogramSettings::algPitchEAC;
   const size_t zeroPaddingFactorSetting = settings.ZeroPaddingFactor();
   const size_t padding = (windowSizeSetting * (zeroPaddingFactorSetting - 1)) / 2;
   const size_t fftLen = windowSizeSetting * zeroPaddingFactorSetting;
   auto nBins = settings.NBins();

   if (from < 0 || from >= numSamples) {
      if (xx >= 0 && xx < (int)len) {
         // Pixel column is out of bounds of the clip!  Should not happen.
         float *const results = &out[nBins * xx];
         std::fill(results, results + nBins, 0.0f);
      }
   }
   else {


      // We can avoid copying memory when ComputeSpectrum is used below
      bool copy = !autocorrelation || (padding > 0) || reassignment;
      float *useBuffer = 0;
      float *adj = scratch + padding;

      {
         auto myLen = windowSizeSetting;
         // Take a window of the track centered at this sample.
         from -= windowSizeSetting >> 1;
         if (from < 0) {
            // Near the start of the clip, pad left with zeroes as needed.
            // from is at least -windowSize / 2
            for (auto ii = from; ii < 0; ++ii)
               *adj++ = 0;
            myLen += from.as_long_long(); // add a negative
            from = 0;
            copy = true;
         }

         if (from + myLen >= numSamples) {
            // Near the end of the clip, pad right with zeroes as needed.
            // newlen is bounded by myLen:
            auto newlen = ( numSamples - from ).as_size_t();
            for (decltype(myLen) ii = newlen; ii < myLen; ++ii)
               adj[ii] = 0;
            myLen = newlen;
            copy = true;
         }

         if (myLen > 0) {
            useBuffer = (float*)(waveTrackCache.GetFloats(
               sampleCount(
                  floor(0.5 + from.as_double() + offset * rate)
               ),
               myLen,
               // Don't throw in this drawing operation
               false)
            );

            if (copy) {
               if (useBuffer)
                  memcpy(adj, useBuffer, myLen * sizeof(float));
               else
                  memset(adj, 0, myLen * sizeof(float));
            }
         }
      }

      if (copy || !useBuffer)
         useBuffer = scratch;

      if (autocorrelation) {
         // not reassignment, xx is surely within bounds.
         wxASSERT(xx >= 0);
         float *const results = &out[nBins * xx];
         // This function does not mutate useBuffer
         ComputeSpectrum(useBuffer, windowSizeSetting, windowSizeSetting,
            rate, results,
            autocorrelation, settings.windowType);
      }
      else if (reassignment) {
         static const double epsilon = 1e-16;
         const auto hFFT = settings.hFFT.get();

         float *const scratch2 = scratch + fftLen;
         std::copy(scratch, scratch2, scratch2);

         float *const scratch3 = scratch + 2 * fftLen;
         std::copy(scratch, scratch2, scratch3);

         {
            const float *const window = settings.window.get();
            for (size_t ii = 0; ii < fftLen; ++ii)
               scratch[ii] *= window[ii];
            RealFFTf(scratch, hFFT);
         }

         {
            const float *const dWindow = settings.dWindow.get();
            for (size_t ii = 0; ii < fftLen; ++ii)
               scratch2[ii] *= dWindow[ii];
            RealFFTf(scratch2, hFFT);
         }

         {
            const float *const tWindow = settings.tWindow.get();
            for (size_t ii = 0; ii < fftLen; ++ii)
               scratch3[ii] *= tWindow[ii];
            RealFFTf(scratch3, hFFT);
         }

         for (size_t ii = 0; ii < hFFT->Points; ++ii) {
            const int index = hFFT->BitReversed[ii];
            const float
               denomRe = scratch[index],
               denomIm = ii == 0 ? 0 : scratch[index + 1];
            const double power = denomRe * denomRe + denomIm * denomIm;
            if (power < epsilon)
               // Avoid dividing by near-zero below
               continue;

            double freqCorrection;
            {
               const double multiplier = -(fftLen / (2.0f * M_PI));
               const float
                  numRe = scratch2[index],
                  numIm = ii == 0 ? 0 : scratch2[index + 1];
               // Find complex quotient --
               // Which means, multiply numerator by conjugate of denominator,
               // then divide by norm squared of denominator --
               // Then just take its imaginary part.
               const double
                  quotIm = (-numRe * denomIm + numIm * denomRe) / power;
               // With appropriate multiplier, that becomes the correction of
               // the frequency bin.
               freqCorrection = multiplier * quotIm;
            }

            const int bin = (int)((int)ii + freqCorrection + 0.5f);
            // Must check if correction takes bin out of bounds, above or below!
            // bin is signed!
            if (bin >= 0 && bin < (int)hFFT->Points) {
               double timeCorrection;
               {
                  const float
                     numRe = scratch3[index],
                     numIm = ii == 0 ? 0 : scratch3[index + 1];
                  // Find another complex quotient --
                  // Then just take its real part.
                  // The result has sample interval as unit.
                  timeCorrection =
                     (numRe * denomRe + numIm * denomIm) / power;
               }

               int correctedX = (floor(0.5 + xx + timeCorrection * pixelsPerSecond / rate));
               if (correctedX >= lowerBoundX && correctedX < upperBoundX)
               {
                  result = true;

                  // This is non-negative, because bin and correctedX are
                  auto ind = (int)nBins * correctedX + bin;
#ifdef _OPENMP
                  // This assignment can race if index reaches into another thread's bins.
                  // The probability of a race very low, so this carries little overhead,
                  // about 5% slower vs allowing it to race.
                  #pragma omp atomic update
#endif
                  out[ind] += power;
               }
            }
         }
      }
      else {
         // not reassignment, xx is surely within bounds.
         wxASSERT(xx >= 0);
         float *const results = &out[nBins * xx];

         // Do the FFT.  Note that useBuffer is multiplied by the window,
         // and the window is initialized with leading and trailing zeroes
         // when there is padding.  Therefore we did not need to reinitialize
         // the part of useBuffer in the padding zones.

         // This function mutates useBuffer
         ComputeSpectrumUsingRealFFTf
            (useBuffer, settings.hFFT.get(), settings.window.get(), fftLen, results);
         if (!gainFactors.empty()) {
            // Apply a frequency-dependent gain factor
            for (size_t ii = 0; ii < nBins; ++ii)
               results[ii] += gainFactors[ii];
         }
      }
   }

   return result;
}

void SpecCache::Grow(size_t len_, const SpectrogramSettings& settings,
                       double pixelsPerSecond, double start_)
{
   settings.CacheWindows();

   // len columns, and so many rows, column-major.
   // Don't take column literally -- this isn't pixel data yet, it's the
   // raw data to be mapped onto the display.
   freq.resize(len_ * settings.NBins());

   // Sample counts corresponding to the columns, and to one past the end.
   where.resize(len_ + 1);

   len = len_;
   algorithm = settings.algorithm;
   pps = pixelsPerSecond;
   start = start_;
   windowType = settings.windowType;
   windowSize = settings.WindowSize();
   zeroPaddingFactor = settings.ZeroPaddingFactor();
   frequencyGain = settings.frequencyGain;
}

void SpecCache::Populate
   (const SpectrogramSettings &settings, WaveTrackCache &waveTrackCache,
    int copyBegin, int copyEnd, size_t numPixels,
    sampleCount numSamples,
    double offset, double rate, double pixelsPerSecond)
{
   const int &frequencyGainSetting = settings.frequencyGain;
   const size_t windowSizeSetting = settings.WindowSize();
   const bool autocorrelation =
      settings.algorithm == SpectrogramSettings::algPitchEAC;
   const bool reassignment =
      settings.algorithm == SpectrogramSettings::algReassignment;
   const size_t zeroPaddingFactorSetting = settings.ZeroPaddingFactor();

   // FFT length may be longer than the window of samples that affect results
   // because of zero padding done for increased frequency resolution
   const size_t fftLen = windowSizeSetting * zeroPaddingFactorSetting;
   const auto nBins = settings.NBins();

   const size_t bufferSize = fftLen;
   const size_t scratchSize = reassignment ? 3 * bufferSize : bufferSize;
   std::vector<float> scratch(scratchSize);

   std::vector<float> gainFactors;
   if (!autocorrelation)
      ComputeSpectrogramGainFactors(fftLen, rate, frequencyGainSetting, gainFactors);

   // Loop over the ranges before and after the copied portion and compute anew.
   // One of the ranges may be empty.
   for (int jj = 0; jj < 2; ++jj) {
      const int lowerBoundX = jj == 0 ? 0 : copyEnd;
      const int upperBoundX = jj == 0 ? copyBegin : numPixels;

#ifdef _OPENMP
      // Storage for mutable per-thread data.
      // private clause ensures one copy per thread
      struct ThreadLocalStorage {
         ThreadLocalStorage()  { }
         ~ThreadLocalStorage() { }

         void init(WaveTrackCache &waveTrackCache, size_t scratchSize) {
            if (!cache) {
               cache = std::make_unique<WaveTrackCache>(waveTrackCache.GetTrack());
               scratch.resize(scratchSize);
            }
         }
         std::unique_ptr<WaveTrackCache> cache;
         std::vector<float> scratch;
      } tls;

      #pragma omp parallel for private(tls)
#endif
      for (auto xx = lowerBoundX; xx < upperBoundX; ++xx)
      {
#ifdef _OPENMP
         tls.init(waveTrackCache, scratchSize);
         WaveTrackCache& cache = *tls.cache;
         float* buffer = &tls.scratch[0];
#else
         WaveTrackCache& cache = waveTrackCache;
         float* buffer = &scratch[0];
#endif
         CalculateOneSpectrum(
            settings, cache, xx, numSamples,
            offset, rate, pixelsPerSecond,
            lowerBoundX, upperBoundX,
            gainFactors, buffer, &freq[0]);
      }

      if (reassignment) {
         // Need to look beyond the edges of the range to accumulate more
         // time reassignments.
         // I'm not sure what's a good stopping criterion?
         auto xx = lowerBoundX;
         const double pixelsPerSample = pixelsPerSecond / rate;
         const int limit = std::min((int)(0.5 + fftLen * pixelsPerSample), 100);
         for (int ii = 0; ii < limit; ++ii)
         {
            const bool result =
               CalculateOneSpectrum(
                  settings, waveTrackCache, --xx, numSamples,
                  offset, rate, pixelsPerSecond,
                  lowerBoundX, upperBoundX,
                  gainFactors, &scratch[0], &freq[0]);
            if (!result)
               break;
         }

         xx = upperBoundX;
         for (int ii = 0; ii < limit; ++ii)
         {
            const bool result =
               CalculateOneSpectrum(
                  settings, waveTrackCache, xx++, numSamples,
                  offset, rate, pixelsPerSecond,
                  lowerBoundX, upperBoundX,
                  gainFactors, &scratch[0], &freq[0]);
            if (!result)
               break;
         }

         // Now Convert to dB terms.  Do this only after accumulating
         // power values, which may cross columns with the time correction.
#ifdef _OPENMP
         #pragma omp parallel for
#endif
         for (xx = lowerBoundX; xx < upperBoundX; ++xx) {
            float *const results = &freq[nBins * xx];
            for (size_t ii = 0; ii < nBins; ++ii) {
               float &power = results[ii];
               if (power <= 0)
                  power = -160.0;
               else
                  power = 10.0*log10f(power);
            }
            if (!gainFactors.empty()) {
               // Apply a frequency-dependent gain factor
               for (size_t ii = 0; ii < nBins; ++ii)
                  results[ii] += gainFactors[ii];
            }
         }
      }
   }
}

bool WaveClip::GetSpectrogram(WaveTrackCache &waveTrackCache,
                              const float *& spectrogram,
                              const sampleCount *& where,
                              size_t numPixels,
                              double t0, double pixelsPerSecond) const
{
   t0 += GetTrimLeft();

   const WaveTrack *const track = waveTrackCache.GetTrack().get();
   const SpectrogramSettings &settings = track->GetSpectrogramSettings();

   //Trim offset comparison failure forces spectrogram cache rebuild 
   //and skip copying "unchanged" data after clip border was trimmed.
   bool match =
      mSpecCache &&
      mSpecCache->leftTrim == GetTrimLeft() &&
      mSpecCache->rightTrim == GetTrimRight() &&
      mSpecCache->len > 0 &&
      mSpecCache->Matches
      (mDirty, pixelsPerSecond, settings, mRate);

   if (match &&
       mSpecCache->start == t0 &&
       mSpecCache->len >= numPixels) {
      spectrogram = &mSpecCache->freq[0];
      where = &mSpecCache->where[0];

      return false;  //hit cache completely
   }

   // Caching is not implemented for reassignment, unless for
   // a complete hit, because of the complications of time reassignment
   if (settings.algorithm == SpectrogramSettings::algReassignment)
      match = false;

   // Free the cache when it won't cause a major stutter.
   // If the window size changed, we know there is nothing to be copied
   // If we zoomed out, or resized, we can give up memory. But not too much -
   // up to 2x extra is needed at the end of the clip to prevent stutter.
   if (mSpecCache->freq.capacity() > 2.1 * mSpecCache->freq.size() ||
       mSpecCache->windowSize*mSpecCache->zeroPaddingFactor <
       settings.WindowSize()*settings.ZeroPaddingFactor())
   {
      match = false;
      mSpecCache = std::make_unique<SpecCache>();
   }

   const double tstep = 1.0 / pixelsPerSecond;
   const double samplesPerPixel = mRate * tstep;

   int oldX0 = 0;
   double correction = 0.0;

   int copyBegin = 0, copyEnd = 0;
   if (match) {
      findCorrection(mSpecCache->where, mSpecCache->len, numPixels,
         t0, mRate, samplesPerPixel,
         oldX0, correction);
      // Remember our first pixel maps to oldX0 in the old cache,
      // possibly out of bounds.
      // For what range of pixels can data be copied?
      copyBegin = std::min((int)numPixels, std::max(0, -oldX0));
      copyEnd = std::min((int)numPixels, std::max(0,
         (int)mSpecCache->len - oldX0
      ));
   }

   // Resize the cache, keep the contents unchanged.
   mSpecCache->Grow(numPixels, settings, pixelsPerSecond, t0);
   mSpecCache->leftTrim = GetTrimLeft();
   mSpecCache->rightTrim = GetTrimRight();
   auto nBins = settings.NBins();

   // Optimization: if the old cache is good and overlaps
   // with the current one, re-use as much of the cache as
   // possible
   if (copyEnd > copyBegin)
   {
      // memmove is required since dst/src overlap
      memmove(&mSpecCache->freq[nBins * copyBegin],
               &mSpecCache->freq[nBins * (copyBegin + oldX0)],
               nBins * (copyEnd - copyBegin) * sizeof(float));
   }

   // Reassignment accumulates, so it needs a zeroed buffer
   if (settings.algorithm == SpectrogramSettings::algReassignment)
   {
      // The cache could theoretically copy from the middle, resulting
      // in two regions to update. This won't happen in zoom, since
      // old cache doesn't match. It won't happen in resize, since the
      // spectrum view is pinned to left side of window.
      wxASSERT(
         (copyBegin >= 0 && copyEnd == (int)numPixels) || // copied the end
         (copyBegin == 0 && copyEnd <= (int)numPixels)    // copied the beginning
      );

      int zeroBegin = copyBegin > 0 ? 0 : copyEnd-copyBegin;
      int zeroEnd = copyBegin > 0 ? copyBegin : numPixels;

      memset(&mSpecCache->freq[nBins*zeroBegin], 0, nBins*(zeroEnd-zeroBegin)*sizeof(float));
   }

   // purposely offset the display 1/2 sample to the left (as compared
   // to waveform display) to properly center response of the FFT
   fillWhere(mSpecCache->where, numPixels, 0.5, correction,
      t0, mRate, samplesPerPixel);

   mSpecCache->Populate
      (settings, waveTrackCache, copyBegin, copyEnd, numPixels,
       GetSequenceSamplesCount(),
       GetSequenceStartTime(), mRate, pixelsPerSecond);

   mSpecCache->dirty = mDirty;
   spectrogram = &mSpecCache->freq[0];
   where = &mSpecCache->where[0];

   return true;
}

std::pair<float, float> WaveClip::GetMinMax(
   double t0, double t1, bool mayThrow) const
{
   if (t0 > t1) {
      if (mayThrow)
         THROW_INCONSISTENCY_EXCEPTION;
      return {
         0.f,  // harmless, but unused since Sequence::GetMinMax does not use these values
         0.f   // harmless, but unused since Sequence::GetMinMax does not use these values
      };
   }

   if (t0 == t1)
      return{ 0.f, 0.f };

   auto s0 = TimeToSequenceSamples(t0);
   auto s1 = TimeToSequenceSamples(t1);

   return mSequence->GetMinMax(s0, s1-s0, mayThrow);
}

float WaveClip::GetRMS(double t0, double t1, bool mayThrow) const
{
   if (t0 > t1) {
      if (mayThrow)
         THROW_INCONSISTENCY_EXCEPTION;
      return 0.f;
   }

   if (t0 == t1)
      return 0.f;

   auto s0 = TimeToSequenceSamples(t0);
   auto s1 = TimeToSequenceSamples(t1);

   return mSequence->GetRMS(s0, s1-s0, mayThrow);
}

void WaveClip::ConvertToSampleFormat(sampleFormat format,
   const std::function<void(size_t)> & progressReport)
{
   // Note:  it is not necessary to do this recursively to cutlines.
   // They get converted as needed when they are expanded.

   auto bChanged = mSequence->ConvertToSampleFormat(format, progressReport);
   if (bChanged)
      MarkChanged();
}

/*! @excsafety{No-fail} */
void WaveClip::UpdateEnvelopeTrackLen()
{
   auto len = (mSequence->GetNumSamples().as_double()) / mRate;
   if (len != mEnvelope->GetTrackLen())
      mEnvelope->SetTrackLen(len, 1.0 / GetRate());
}

/*! @excsafety{Strong} */
std::shared_ptr<SampleBlock> WaveClip::AppendNewBlock(
   samplePtr buffer, sampleFormat format, size_t len)
{
   return mSequence->AppendNewBlock( buffer, format, len );
}

/*! @excsafety{Strong} */
void WaveClip::AppendSharedBlock(const std::shared_ptr<SampleBlock> &pBlock)
{
   mSequence->AppendSharedBlock( pBlock );
}

/*! @excsafety{Partial}
 -- Some prefix (maybe none) of the buffer is appended,
and no content already flushed to disk is lost. */
bool WaveClip::Append(constSamplePtr buffer, sampleFormat format,
                      size_t len, unsigned int stride)
{
   //wxLogDebug(wxT("Append: len=%lli"), (long long) len);
   bool result = false;

   auto maxBlockSize = mSequence->GetMaxBlockSize();
   auto blockSize = mSequence->GetIdealAppendLen();
   sampleFormat seqFormat = mSequence->GetSampleFormat();

   if (!mAppendBuffer.ptr())
      mAppendBuffer.Allocate(maxBlockSize, seqFormat);

   auto cleanup = finally( [&] {
      // use No-fail-guarantee
      UpdateEnvelopeTrackLen();
      MarkChanged();
   } );

   for(;;) {
      if (mAppendBufferLen >= blockSize) {
         // flush some previously appended contents
         // use Strong-guarantee
         mSequence->Append(mAppendBuffer.ptr(), seqFormat, blockSize);
         result = true;

         // use No-fail-guarantee for rest of this "if"
         memmove(mAppendBuffer.ptr(),
                 mAppendBuffer.ptr() + blockSize * SAMPLE_SIZE(seqFormat),
                 (mAppendBufferLen - blockSize) * SAMPLE_SIZE(seqFormat));
         mAppendBufferLen -= blockSize;
         blockSize = mSequence->GetIdealAppendLen();
      }

      if (len == 0)
         break;

      // use No-fail-guarantee for rest of this "for"
      wxASSERT(mAppendBufferLen <= maxBlockSize);
      auto toCopy = std::min(len, maxBlockSize - mAppendBufferLen);

      CopySamples(buffer, format,
                  mAppendBuffer.ptr() + mAppendBufferLen * SAMPLE_SIZE(seqFormat),
                  seqFormat,
                  toCopy,
                  gHighQualityDither,
                  stride);

      mAppendBufferLen += toCopy;
      buffer += toCopy * SAMPLE_SIZE(format) * stride;
      len -= toCopy;
   }

   return result;
}

/*! @excsafety{Mixed} */
/*! @excsafety{No-fail} -- The clip will be in a flushed state. */
/*! @excsafety{Partial}
-- Some initial portion (maybe none) of the append buffer of the
clip gets appended; no previously flushed contents are lost. */
void WaveClip::Flush()
{
   //wxLogDebug(wxT("WaveClip::Flush"));
   //wxLogDebug(wxT("   mAppendBufferLen=%lli"), (long long) mAppendBufferLen);
   //wxLogDebug(wxT("   previous sample count %lli"), (long long) mSequence->GetNumSamples());

   if (mAppendBufferLen > 0) {

      auto cleanup = finally( [&] {
         // Blow away the append buffer even in case of failure.  May lose some
         // data but don't leave the track in an un-flushed state.

         // Use No-fail-guarantee of these steps.
         mAppendBufferLen = 0;
         UpdateEnvelopeTrackLen();
         MarkChanged();
      } );

      mSequence->Append(mAppendBuffer.ptr(), mSequence->GetSampleFormat(),
         mAppendBufferLen);
   }

   //wxLogDebug(wxT("now sample count %lli"), (long long) mSequence->GetNumSamples());
}

bool WaveClip::HandleXMLTag(const wxChar *tag, const wxChar **attrs)
{
   if (!wxStrcmp(tag, wxT("waveclip")))
   {
      double dblValue;
      long longValue;
      while (*attrs)
      {
         const wxChar *attr = *attrs++;
         const wxChar *value = *attrs++;

         if (!value)
            break;

         const wxString strValue = value;
         if (!wxStrcmp(attr, wxT("offset")))
         {
            if (!XMLValueChecker::IsGoodString(strValue) ||
                  !Internat::CompatibleToDouble(strValue, &dblValue))
               return false;
            SetSequenceStartTime(dblValue);
         }
         else if (!wxStrcmp(attr, wxT("trimLeft")))
         {
            if (!XMLValueChecker::IsGoodString(strValue) ||
               !Internat::CompatibleToDouble(strValue, &dblValue))
               return false;
            SetTrimLeft(dblValue);
         }
         else if (!wxStrcmp(attr, wxT("trimRight")))
         {
            if (!XMLValueChecker::IsGoodString(strValue) ||
               !Internat::CompatibleToDouble(strValue, &dblValue))
               return false;
            SetTrimRight(dblValue);
         }
         else if (!wxStrcmp(attr, wxT("name")))
         {
            if(XMLValueChecker::IsGoodLongString(strValue))
               SetName(strValue);
         }
         else if (!wxStrcmp(attr, wxT("colorindex")))
         {
            if (!XMLValueChecker::IsGoodString(strValue) ||
                  !strValue.ToLong( &longValue))
               return false;
            SetColourIndex(longValue);
         }
      }
      return true;
   }

   return false;
}

void WaveClip::HandleXMLEndTag(const wxChar *tag)
{
   if (!wxStrcmp(tag, wxT("waveclip")))
      UpdateEnvelopeTrackLen();
}

XMLTagHandler *WaveClip::HandleXMLChild(const wxChar *tag)
{
   if (!wxStrcmp(tag, wxT("sequence")))
      return mSequence.get();
   else if (!wxStrcmp(tag, wxT("envelope")))
      return mEnvelope.get();
   else if (!wxStrcmp(tag, wxT("waveclip")))
   {
      // Nested wave clips are cut lines
      mCutLines.push_back(
         std::make_unique<WaveClip>(mSequence->GetFactory(),
            mSequence->GetSampleFormat(), mRate, 0 /*colourindex*/));
      return mCutLines.back().get();
   }
   else
      return NULL;
}

void WaveClip::WriteXML(XMLWriter &xmlFile) const
// may throw
{
   xmlFile.StartTag(wxT("waveclip"));
   xmlFile.WriteAttr(wxT("offset"), mSequenceOffset, 8);
   xmlFile.WriteAttr(wxT("trimLeft"), mTrimLeft, 8);
   xmlFile.WriteAttr(wxT("trimRight"), mTrimRight, 8);
   xmlFile.WriteAttr(wxT("name"), mName);
   xmlFile.WriteAttr(wxT("colorindex"), mColourIndex );

   mSequence->WriteXML(xmlFile);
   mEnvelope->WriteXML(xmlFile);

   for (const auto &clip: mCutLines)
      clip->WriteXML(xmlFile);

   xmlFile.EndTag(wxT("waveclip"));
}

/*! @excsafety{Strong} */
void WaveClip::Paste(double t0, const WaveClip* other)
{
   const bool clipNeedsResampling = other->mRate != mRate;
   const bool clipNeedsNewFormat =
      other->mSequence->GetSampleFormat() != mSequence->GetSampleFormat();
   std::unique_ptr<WaveClip> newClip;

   t0 = std::clamp(t0, GetPlayStartTime(), GetPlayEndTime());

   //seems like edge cases cannot happen, see WaveTrack::PasteWaveTrack
   if (t0 == GetPlayStartTime())
   {
       ClearSequence(GetSequenceStartTime(), t0);
       SetTrimLeft(other->GetTrimLeft());

       auto copy = std::make_unique<WaveClip>(*other, mSequence->GetFactory(), true);
       copy->ClearSequence(copy->GetPlayEndTime(), copy->GetSequenceEndTime());
       newClip = std::move(copy);
   }
   else if (t0 == GetPlayEndTime())
   {
       ClearSequence(GetPlayEndTime(), GetSequenceEndTime());
       SetTrimRight(other->GetTrimRight());

       auto copy = std::make_unique<WaveClip>(*other, mSequence->GetFactory(), true);
       copy->ClearSequence(copy->GetSequenceStartTime(), copy->GetPlayStartTime());
       newClip = std::move(copy);
   }
   else
   {
       newClip = std::make_unique<WaveClip>(*other, mSequence->GetFactory(), true,
           other->GetPlayStartTime(), other->GetPlayEndTime());
   }

   if (clipNeedsResampling || clipNeedsNewFormat)
   {
      auto copy = std::make_unique<WaveClip>(*newClip.get(), mSequence->GetFactory(), true);
      if (clipNeedsResampling)
         // The other clip's rate is different from ours, so resample
          copy->Resample(mRate);
      if (clipNeedsNewFormat)
         // Force sample formats to match.
          copy->ConvertToSampleFormat(mSequence->GetSampleFormat());
      newClip = std::move(copy);
   }

   // Paste cut lines contained in pasted clip
   WaveClipHolders newCutlines;
   for (const auto &cutline: newClip->mCutLines)
   {
      auto cutlineCopy = std::make_unique<WaveClip>(*cutline, mSequence->GetFactory(), 
         // Recursively copy cutlines of cutlines.  They don't need
         // their offsets adjusted.
         true);
      cutlineCopy->Offset(t0 - GetSequenceStartTime());
      newCutlines.push_back(std::move(cutlineCopy));
   }

   sampleCount s0 = TimeToSequenceSamples(t0);

   // Assume Strong-guarantee from Sequence::Paste
   mSequence->Paste(s0, newClip->mSequence.get());

   // Assume No-fail-guarantee in the remaining
   MarkChanged();
   auto sampleTime = 1.0 / GetRate();
   mEnvelope->PasteEnvelope
      (s0.as_double()/mRate + GetSequenceStartTime(), newClip->mEnvelope.get(), sampleTime);
   OffsetCutLines(t0, newClip->GetPlayEndTime() - newClip->GetPlayStartTime());

   for (auto &holder : newCutlines)
      mCutLines.push_back(std::move(holder));
}

/*! @excsafety{Strong} */
void WaveClip::InsertSilence( double t, double len, double *pEnvelopeValue )
{
   if (t == GetPlayStartTime() && t > GetSequenceStartTime())
      ClearSequence(GetSequenceStartTime(), t);
   else if (t == GetPlayEndTime() && t < GetSequenceEndTime())
      ClearSequence(t, GetSequenceEndTime());

   auto s0 = TimeToSequenceSamples(t);
   auto slen = (sampleCount)floor(len * mRate + 0.5);

   // use Strong-guarantee
   GetSequence()->InsertSilence(s0, slen);

   // use No-fail-guarantee
   OffsetCutLines(t, len);

   const auto sampleTime = 1.0 / GetRate();
   auto pEnvelope = GetEnvelope();
   if ( pEnvelopeValue ) {

      // Preserve limit value at the end
      auto oldLen = pEnvelope->GetTrackLen();
      auto newLen = oldLen + len;
      pEnvelope->Cap( sampleTime );

      // Ramp across the silence to the given value
      pEnvelope->SetTrackLen( newLen, sampleTime );
      pEnvelope->InsertOrReplace
         ( pEnvelope->GetOffset() + newLen, *pEnvelopeValue );
   }
   else
      pEnvelope->InsertSpace( t, len );

   MarkChanged();
}

/*! @excsafety{Strong} */
void WaveClip::AppendSilence( double len, double envelopeValue )
{
   auto t = GetPlayEndTime();
   InsertSilence( t, len, &envelopeValue );
}

/*! @excsafety{Strong} */
void WaveClip::Clear(double t0, double t1)
{
    auto st0 = t0;
    auto st1 = t1;
    auto offset = .0;
    if (st0 <= GetPlayStartTime())
    {
        offset = (t0 - GetPlayStartTime()) + GetTrimLeft();
        st0 = GetSequenceStartTime();

        SetTrimLeft(.0);
    }
    if (st1 >= GetPlayEndTime())
    {
        st1 = GetSequenceEndTime();
        SetTrimRight(.0);
    }
    ClearSequence(st0, st1);

    if (offset != .0)
        Offset(offset);        
}

void WaveClip::ClearSequence(double t0, double t1)
{
    auto clip_t0 = std::max(t0, GetSequenceStartTime());
    auto clip_t1 = std::min(t1, GetSequenceEndTime());

    auto s0 = TimeToSequenceSamples(clip_t0);
    auto s1 = TimeToSequenceSamples(clip_t1);

    if (s0 != s1)
    {
        // use Strong-guarantee
        GetSequence()->Delete(s0, s1 - s0);

        // use No-fail-guarantee in the remaining

        // msmeyer
        //
        // Delete all cutlines that are within the given area, if any.
        //
        // Note that when cutlines are active, two functions are used:
        // Clear() and ClearAndAddCutLine(). ClearAndAddCutLine() is called
        // whenever the user directly calls a command that removes some audio, e.g.
        // "Cut" or "Clear" from the menu. This command takes care about recursive
        // preserving of cutlines within clips. Clear() is called when internal
        // operations want to remove audio. In the latter case, it is the right
        // thing to just remove all cutlines within the area.
        //

        // May DELETE as we iterate, so don't use range-for
        for (auto it = mCutLines.begin(); it != mCutLines.end();)
        {
            WaveClip* clip = it->get();
            double cutlinePosition = GetSequenceStartTime() + clip->GetSequenceStartTime();
            if (cutlinePosition >= t0 && cutlinePosition <= t1)
            {
                // This cutline is within the area, DELETE it
                it = mCutLines.erase(it);
            }
            else
            {
                if (cutlinePosition >= t1)
                {
                    clip->Offset(clip_t0 - clip_t1);
                }
                ++it;
            }
        }

        // Collapse envelope
        auto sampleTime = 1.0 / GetRate();
        GetEnvelope()->CollapseRegion(t0, t1, sampleTime);
    }


    MarkChanged();
}

/*! @excsafety{Weak}
-- This WaveClip remains destructible in case of AudacityException.
But some cutlines may be deleted */
void WaveClip::ClearAndAddCutLine(double t0, double t1)
{
   if (t0 > GetPlayEndTime() || t1 < GetPlayStartTime())
      return; // time out of bounds

   const double clip_t0 = std::max( t0, GetPlayStartTime() );
   const double clip_t1 = std::min( t1, GetPlayEndTime() );

   auto newClip = std::make_unique< WaveClip >
      (*this, mSequence->GetFactory(), true, clip_t0, clip_t1);

   newClip->SetSequenceStartTime( clip_t0 - GetSequenceStartTime() );

   // Remove cutlines from this clip that were in the selection, shift
   // left those that were after the selection
   // May DELETE as we iterate, so don't use range-for
   for (auto it = mCutLines.begin(); it != mCutLines.end();)
   {
      WaveClip* clip = it->get();
      double cutlinePosition = GetSequenceStartTime() + clip->GetSequenceStartTime();
      if (cutlinePosition >= t0 && cutlinePosition <= t1)
         it = mCutLines.erase(it);
      else
      {
         if (cutlinePosition >= t1)
         {
            clip->Offset(clip_t0 - clip_t1);
         }
         ++it;
      }
   }

   // Clear actual audio data
   auto s0 = TimeToSequenceSamples(t0);
   auto s1 = TimeToSequenceSamples(t1);

   // use Weak-guarantee
   GetSequence()->Delete(s0, s1-s0);

   // Collapse envelope
   auto sampleTime = 1.0 / GetRate();
   GetEnvelope()->CollapseRegion( t0, t1, sampleTime );
   
   MarkChanged();

   mCutLines.push_back(std::move(newClip));
}

bool WaveClip::FindCutLine(double cutLinePosition,
                           double* cutlineStart /* = NULL */,
                           double* cutlineEnd /* = NULL */) const
{
   for (const auto &cutline: mCutLines)
   {
      if (fabs(GetSequenceStartTime() + cutline->GetSequenceStartTime() - cutLinePosition) < 0.0001)
      {
         if (cutlineStart)
            *cutlineStart = GetSequenceStartTime() + cutline->GetSequenceStartTime();
         if (cutlineEnd)
            *cutlineEnd = GetSequenceStartTime() + cutline->GetSequenceEndTime();
         return true;
      }
   }

   return false;
}

/*! @excsafety{Strong} */
void WaveClip::ExpandCutLine(double cutLinePosition)
{
   auto end = mCutLines.end();
   auto it = std::find_if( mCutLines.begin(), end,
      [&](const WaveClipHolder &cutline) {
         return fabs(GetSequenceStartTime() + cutline->GetSequenceStartTime() - cutLinePosition) < 0.0001;
      } );

   if ( it != end ) {
      auto cutline = it->get();
      // assume Strong-guarantee from Paste

      // Envelope::Paste takes offset into account, WaveClip::Paste doesn't!
      // Do this to get the right result:
      cutline->mEnvelope->SetOffset(0);

      Paste(GetSequenceStartTime()+cutline->GetSequenceStartTime(), cutline);
      // Now erase the cutline,
      // but be careful to find it again, because Paste above may
      // have modified the array of cutlines (if our cutline contained
      // another cutline!), invalidating the iterator we had.
      end = mCutLines.end();
      it = std::find_if(mCutLines.begin(), end,
         [=](const WaveClipHolder &p) { return p.get() == cutline; });
      if (it != end)
         mCutLines.erase(it); // deletes cutline!
      else {
         wxASSERT(false);
      }
   }
}

bool WaveClip::RemoveCutLine(double cutLinePosition)
{
   for (auto it = mCutLines.begin(); it != mCutLines.end(); ++it)
   {
      const auto &cutline = *it;
      //std::numeric_limits<double>::epsilon() or (1.0 / static_cast<double>(mRate))? 
      if (fabs(GetSequenceStartTime() + cutline->GetSequenceStartTime() - cutLinePosition) < 0.0001)
      {
         mCutLines.erase(it); // deletes cutline!
         return true;
      }
   }

   return false;
}

/*! @excsafety{No-fail} */
void WaveClip::OffsetCutLines(double t0, double len)
{
   for (const auto &cutLine : mCutLines)
   {
      if (GetSequenceStartTime() + cutLine->GetSequenceStartTime() >= t0)
         cutLine->Offset(len);
   }
}

void WaveClip::CloseLock()
{
   GetSequence()->CloseLock();
   for (const auto &cutline: mCutLines)
      cutline->CloseLock();
}

void WaveClip::SetRate(int rate)
{
   mRate = rate;
   auto newLength = mSequence->GetNumSamples().as_double() / mRate;
   mEnvelope->RescaleTimes( newLength );
   MarkChanged();
}

/*! @excsafety{Strong} */
void WaveClip::Resample(int rate, ProgressDialog *progress)
{
   // Note:  it is not necessary to do this recursively to cutlines.
   // They get resampled as needed when they are expanded.

   if (rate == mRate)
      return; // Nothing to do

   double factor = (double)rate / (double)mRate;
   ::Resample resample(true, factor, factor); // constant rate resampling

   const size_t bufsize = 65536;
   Floats inBuffer{ bufsize };
   Floats outBuffer{ bufsize };
   sampleCount pos = 0;
   bool error = false;
   int outGenerated = 0;
   auto numSamples = mSequence->GetNumSamples();

   auto newSequence =
      std::make_unique<Sequence>(mSequence->GetFactory(), mSequence->GetSampleFormat());

   /**
    * We want to keep going as long as we have something to feed the resampler
    * with OR as long as the resampler spews out samples (which could continue
    * for a few iterations after we stop feeding it)
    */
   while (pos < numSamples || outGenerated > 0)
   {
      const auto inLen = limitSampleBufferSize( bufsize, numSamples - pos );

      bool isLast = ((pos + inLen) == numSamples);

      if (!mSequence->Get((samplePtr)inBuffer.get(), floatSample, pos, inLen, true))
      {
         error = true;
         break;
      }

      const auto results = resample.Process(factor, inBuffer.get(), inLen, isLast,
                                            outBuffer.get(), bufsize);
      outGenerated = results.second;

      pos += results.first;

      if (outGenerated < 0)
      {
         error = true;
         break;
      }

      newSequence->Append((samplePtr)outBuffer.get(), floatSample,
                          outGenerated);

      if (progress)
      {
         auto updateResult = progress->Update(
            pos.as_long_long(),
            numSamples.as_long_long()
         );
         error = (updateResult != ProgressResult::Success);
         if (error)
            throw UserException{};
      }
   }

   if (error)
      throw SimpleMessageBoxException{
         ExceptionType::Internal,
         XO("Resampling failed."),
         XO("Warning"),
         "Error:_Resampling"
      };
   else
   {
      // Use No-fail-guarantee in these steps

      // Invalidate wave display cache
      mWaveCache = std::make_unique<WaveCache>();
      // Invalidate the spectrum display cache
      mSpecCache = std::make_unique<SpecCache>();

      mSequence = std::move(newSequence);
      mRate = rate;
   }
}

// Used by commands which interact with clips using the keyboard.
// When two clips are immediately next to each other, the GetPlayEndTime()
// of the first clip and the GetPlayStartTime() of the second clip may not
// be exactly equal due to rounding errors.
bool WaveClip::SharesBoundaryWithNextClip(const WaveClip* next) const
{
   double endThis = GetRate() * GetPlayStartTime() + GetPlaySamplesCount().as_double();
   double startNext = next->GetRate() * next->GetPlayStartTime();

   // given that a double has about 15 significant digits, using a criterion
   // of half a sample should be safe in all normal usage.
   return fabs(startNext - endThis) < 0.5;
}

void WaveClip::SetName(const wxString& name)
{
   mName = name;
}

const wxString& WaveClip::GetName() const
{
   return mName;
}

sampleCount WaveClip::TimeToSamples(double time) const noexcept
{
    return sampleCount(floor(time * mRate + 0.5));
}

double WaveClip::SamplesToTime(sampleCount s) const noexcept
{
    return s.as_double() / mRate;
}

void WaveClip::SetSilence(sampleCount offset, sampleCount length)
{
    GetSequence()->SetSilence(TimeToSamples(GetTrimLeft()) + offset, length);
    MarkChanged();
}

sampleCount WaveClip::GetSequenceSamplesCount() const
{
    return mSequence->GetNumSamples();
}

double WaveClip::GetPlayStartTime() const noexcept
{
    return mSequenceOffset + mTrimLeft;
}

void WaveClip::SetPlayStartTime(double time)
{
    SetSequenceStartTime(time - mTrimLeft);
}

double WaveClip::GetPlayEndTime() const
{
    auto numSamples = mSequence->GetNumSamples();

    double maxLen = GetSequenceStartTime() + ((numSamples + mAppendBufferLen).as_double()) / mRate - mTrimRight;
    // JS: calculated value is not the length;
    // it is a maximum value and can be negative; no clipping to 0

    return maxLen;
}

sampleCount WaveClip::GetPlayStartSample() const
{
    return TimeToSamples(GetPlayStartTime());
}

sampleCount WaveClip::GetPlayEndSample() const
{
    return GetPlayStartSample() + GetPlaySamplesCount();
}

sampleCount WaveClip::GetPlaySamplesCount() const
{
    return mSequence->GetNumSamples()
       - TimeToSamples(mTrimRight) - TimeToSamples(mTrimLeft);
}

void WaveClip::SetTrimLeft(double trim)
{
    mTrimLeft = std::max(.0, trim);
}

double WaveClip::GetTrimLeft() const noexcept
{
    return mTrimLeft;
}

void WaveClip::SetTrimRight(double trim)
{
    mTrimRight = std::max(.0, trim);
}

double WaveClip::GetTrimRight() const noexcept
{
    return mTrimRight;
}

void WaveClip::TrimLeft(double deltaTime)
{
    mTrimLeft += deltaTime;
}

void WaveClip::TrimRight(double deltaTime)
{
    mTrimRight += deltaTime;
}

void WaveClip::TrimLeftTo(double to)
{
    mTrimLeft = std::clamp(to, GetSequenceStartTime(), GetPlayEndTime()) - GetSequenceStartTime();
}

void WaveClip::TrimRightTo(double to)
{
    mTrimRight = GetSequenceEndTime() - std::clamp(to, GetPlayStartTime(), GetSequenceEndTime());
}

double WaveClip::GetSequenceStartTime() const noexcept
{
    // JS: mSequenceOffset is the minimum value and it is returned; no clipping to 0
    return mSequenceOffset;
}

void WaveClip::SetSequenceStartTime(double startTime)
{
    mSequenceOffset = startTime;
    mEnvelope->SetOffset(startTime);
}

double WaveClip::GetSequenceEndTime() const
{
    auto numSamples = mSequence->GetNumSamples();

    double maxLen = GetSequenceStartTime() + (numSamples + mAppendBufferLen).as_double() / mRate;
    // JS: calculated value is not the length;
    // it is a maximum value and can be negative; no clipping to 0

    return maxLen;
}

sampleCount WaveClip::GetSequenceStartSample() const
{
    return TimeToSamples(mSequenceOffset);
}

sampleCount WaveClip::GetSequenceEndSample() const
{
    return GetSequenceStartSample() + mSequence->GetNumSamples();
}

void WaveClip::Offset(double delta) noexcept
{
    SetSequenceStartTime(GetSequenceStartTime() + delta);
}

// Bug 2288 allowed overlapping clips.
// This was a classic fencepost error.
// We are within the clip if start < t <= end.
// Note that BeforeClip and AfterClip must be consistent 
// with this definition.
bool WaveClip::WithinPlayRegion(double t) const
{
    auto ts = TimeToSamples(t);
    return ts > GetPlayStartSample() && ts < GetPlayEndSample() + mAppendBufferLen;
}

bool WaveClip::BeforePlayStartTime(double t) const
{
    auto ts = TimeToSamples(t);
    return ts <= GetPlayStartSample();
}

bool WaveClip::AfterPlayEndTime(double t) const
{
    auto ts = TimeToSamples(t);
    return ts >= GetPlayEndSample() + mAppendBufferLen;
}

sampleCount WaveClip::TimeToSequenceSamples(double t) const
{
    if (t < GetSequenceStartTime())
        return 0;
    else if (t > GetSequenceEndTime())
        return mSequence->GetNumSamples();
    return TimeToSamples(t - GetSequenceStartTime());
}

sampleCount WaveClip::ToSequenceSamples(sampleCount s) const
{
    return s - GetSequenceStartSample();
}
