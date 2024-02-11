/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  MirDsp.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "MirDsp.h"
#include "IteratorX.h"
#include "MemoryX.h"
#include "MirTypes.h"
#include "MirUtils.h"
#include "PowerSpectrumGetter.h"
#include "StftFrameProvider.h"

#include <cassert>
#include <cmath>
#include <numeric>
#include <pffft.h>

namespace MIR
{
namespace
{
constexpr float GetLog2(float x)
{
   // https://stackoverflow.com/a/28730362
   union
   {
      float val;
      int32_t x;
   } u = { x };
   auto log_2 = (float)(((u.x >> 23) & 255) - 128);
   u.x &= ~(255 << 23);
   u.x += 127 << 23;
   log_2 += ((-0.3358287811f) * u.val + 2.0f) * u.val - 0.65871759316667f;
   return log_2;
}

float GetNoveltyMeasure(
   const std::vector<float>& prevPowSpec, const std::vector<float>& powSpec)
{
   auto k = 0;
   return std::accumulate(
      powSpec.begin(), powSpec.end(), 0.f, [&](float a, float mag) {
         // Half-wave-rectified stuff
         return a + std::max(0.f, mag - prevPowSpec[k++]);
      });
}

std::vector<float> GetMovingAverage(const std::vector<float>& x, double hopRate)
{
   constexpr auto smoothingWindowDuration = 0.2;
   // An odd number.
   const int M = std::round(smoothingWindowDuration * hopRate / 4) * 2 + 1;
   const auto window = GetNormalizedHann(2 * M + 1);
   auto n = 0;
   std::vector<float> movingAverage(x.size());
   std::transform(x.begin(), x.end(), movingAverage.begin(), [&](float) {
      const auto m = IotaRange(-M, M + 1);
      const auto y =
         std::accumulate(m.begin(), m.end(), 0.f, [&](float y, int i) {
            auto k = n + i;
            while (k < 0)
               k += x.size();
            while (k >= x.size())
               k -= x.size();
            return y + x[k] * window[i + M];
         });
      ++n;
      // The moving average of the raw ODF will be subtracted from it to yield
      // the final ODF, negative results being set to 0. (This is to remove
      // noise of small ODF peaks before the method's quantization step.) The
      // larger this multiplier, the less peaks will remain. This value was
      // found by trial and error, using the benchmarking framework
      // (see TatumQuantizationFitBenchmarking.cpp)
      constexpr auto thresholdRaiser = 1.5f;
      return y * thresholdRaiser;
   });
   return movingAverage;
}
} // namespace

std::vector<float> GetNormalizedCircularAutocorr(std::vector<float> x)
{
   if (std::all_of(x.begin(), x.end(), [](float x) { return x == 0.f; }))
      return x;
   const auto N = x.size();
   assert(IsPowOfTwo(N));
   PffftSetupHolder setup{ pffft_new_setup(N, PFFFT_REAL) };
   std::vector<float> work(N);
   pffft_transform_ordered(
      setup.get(), x.data(), x.data(), work.data(), PFFFT_FORWARD);

   // Transform to a power spectrum, but preserving the layout expected by PFFFT
   // in preparation for the inverse transform.
   x[0] *= x[0];
   x[1] *= x[1];
   for (auto n = 2; n < N; n += 2)
   {
      x[n] = x[n] * x[n] + x[n + 1] * x[n + 1];
      x[n + 1] = 0.f;
   }

   pffft_transform_ordered(
      setup.get(), x.data(), x.data(), work.data(), PFFFT_BACKWARD);

   // The second half of the circular autocorrelation is the mirror of the first
   // half. We are economic and only keep the first half.
   x.erase(x.begin() + N / 2 + 1, x.end());

   const auto normalizer = 1 / x[0];
   std::transform(x.begin(), x.end(), x.begin(), [normalizer](float x) {
      return x * normalizer;
   });
   return x;
}

std::vector<float> GetOnsetDetectionFunction(
   const MirAudioReader& audio,
   const std::function<void(double)>& progressCallback,
   QuantizationFitDebugOutput* debugOutput)
{
   StftFrameProvider frameProvider { audio };
   const auto sampleRate = frameProvider.GetSampleRate();
   const auto numFrames = frameProvider.GetNumFrames();
   const auto frameSize = frameProvider.GetFftSize();
   std::vector<float> buffer(frameSize);
   std::vector<float> odf;
   odf.reserve(numFrames);
   const auto powSpecSize = frameSize / 2 + 1;
   std::vector<float> powSpec(powSpecSize);
   std::vector<float> prevPowSpec(powSpecSize);
   std::vector<float> firstPowSpec;
   std::fill(prevPowSpec.begin(), prevPowSpec.end(), 0.f);

   PowerSpectrumGetter getPowerSpectrum { frameSize };

   auto frameCounter = 0;
   while (frameProvider.GetNextFrame(buffer))
   {
      getPowerSpectrum(buffer.data(), powSpec.data());

      // Compress the frame as per section (6.5) in Müller, Meinard.
      // Fundamentals of music processing: Audio, analysis, algorithms,
      // applications. Vol. 5. Cham: Springer, 2015.
      constexpr auto gamma = 100.f;
      std::transform(
         powSpec.begin(), powSpec.end(), powSpec.begin(),
         [gamma](float x) { return GetLog2(1 + gamma * std::sqrt(x)); });

      if (firstPowSpec.empty())
         firstPowSpec = powSpec;
      else
         odf.push_back(GetNoveltyMeasure(prevPowSpec, powSpec));

      if (debugOutput)
         debugOutput->postProcessedStft.push_back(powSpec);

      std::swap(prevPowSpec, powSpec);

      if (progressCallback)
         progressCallback(1. * ++frameCounter / numFrames);
   }

   // Close the loop.
   odf.push_back(GetNoveltyMeasure(prevPowSpec, firstPowSpec));
   assert(IsPowOfTwo(odf.size()));

   const auto movingAverage =
      GetMovingAverage(odf, frameProvider.GetFrameRate());

   if (debugOutput)
   {
      debugOutput->rawOdf = odf;
      debugOutput->movingAverage = movingAverage;
   }

   // Subtract moving average from ODF.
   std::transform(
      odf.begin(), odf.end(), movingAverage.begin(), odf.begin(),
      [](float a, float b) { return std::max<float>(a - b, 0.f); });

   return odf;
}
} // namespace MIR
