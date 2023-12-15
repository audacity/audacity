#include "MirDsp.h"
#include "FFT.h"
#include "MirAudioReader.h"
#include "MirTypes.h"
#include "MirUtils.h"
#include "StftFrameProvider.h"

#include <cassert>
#include <cmath>
#include <numeric>

namespace MIR
{
namespace
{
float GetNoveltyMeasure(
   const std::vector<float>& prevPowSpec, const std::vector<float>& powSpec)
{
   auto k = 0;
   return std::accumulate(
      powSpec.begin(), powSpec.end(), 0., [&](float a, float mag) {
         // Half-wave-rectified stuff
         return a + std::max(0.f, mag - prevPowSpec[k++]);
      });
}

std::vector<float>
GetMovingAverage(const std::vector<float>& x, double sampleRate)
{
   constexpr auto smoothingWindowDuration = 0.2;
   // An odd number.
   const int M = std::round(smoothingWindowDuration * sampleRate / 4) * 2 + 1;
   const auto window = GetNormalizedHann(2 * M + 1);
   auto n = 0;
   std::vector<float> movingAverage(x.size());
   std::transform(x.begin(), x.end(), movingAverage.begin(), [&](float) {
      auto y = 0.;
      for (auto i = -M; i <= M; ++i)
      {
         auto k = n + i;
         if (k < 0)
            k += x.size();
         else if (k >= x.size())
            k -= x.size();
         y += x[k] * window[i + M];
      }
      ++n;
      constexpr auto smoothingThreshold = 1.5;
      return y * smoothingThreshold;
   });
   return movingAverage;
}
} // namespace

std::vector<float> GetNormalizedCircularAutocorr(std::vector<float> x)
{
   const auto N = x.size();
   assert(IsPowOfTwo(N));
   PowerSpectrum(N, x.data(), x.data());
   // We need the entire power spectrum for the auto-correlation, not only the
   // left-hand side.
   std::copy(x.begin() + 1, x.begin() + N / 2 - 1, x.rbegin());
   InverseRealFFT(N, x.data(), nullptr, x.data());
   // For efficiency, only keep the positive half of this symmetric signal.
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
   const auto powSpecSize = frameSize / 2 + 1;
   std::vector<float> powSpec(powSpecSize);
   std::vector<float> prevPowSpec(powSpecSize);
   std::vector<float> firstPowSpec;
   std::fill(prevPowSpec.begin(), prevPowSpec.end(), 0.f);

   auto frameCounter = 0;
   while (frameProvider.GetNextFrame(buffer))
   {
      // StftFrameProvider already applies a normalizing Hann window, no need to
      // either window it here or normalize it by frame size afterwards.
      PowerSpectrum(frameSize, buffer.data(), powSpec.data());

      // Compress the frame as per Fundamentals of Music Processing, (6.5)
      constexpr auto gamma = 100.f;
      std::transform(
         powSpec.begin(), powSpec.end(), powSpec.begin(),
         // Using `logf` on Linux fails. #ifdef it ?
         [gamma](float x) { return std::log(1 + gamma * std::sqrt(x)); });

      if (firstPowSpec.empty())
         firstPowSpec = powSpec;
      else
         odf.push_back(GetNoveltyMeasure(prevPowSpec, powSpec));

      std::swap(prevPowSpec, powSpec);

      if (debugOutput)
         debugOutput->postProcessedStft.push_back(powSpec);

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
