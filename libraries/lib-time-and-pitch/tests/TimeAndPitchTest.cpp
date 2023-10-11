/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  TimeAndPitchTest.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "StaffPad/TimeAndPitch.h"
#include "AudioContainer.h"
#include "TimeAndPitchRealSource.h"
#include "WavFileIO.h"

#include <catch2/catch.hpp>
#include <numeric>

namespace
{
constexpr auto maxBlockSize = 1024;

float GetRms(
   const std::vector<std::vector<float>>& x, int sampleRate,
   double stretchRatio)
{
   // Ignore half a second left and right.
   const int numToTrim = sampleRate * stretchRatio / 10;
   const double numSamples = x.size() * (x[0].size() - numToTrim * 2);
   return 10 * std::log10f(
                  std::accumulate(
                     x.begin(), x.end(), 0.f,
                     [numToTrim](const auto& acc, const auto& channel) {
                        return acc +
                               std::accumulate(
                                  channel.begin() + numToTrim,
                                  channel.end() - numToTrim, 0.f,
                                  [](const auto& acc, const auto& sample) {
                                     return acc + sample * sample;
                                  });
                     }) /
                  numSamples);
};

void ReadSoManySamples(
   staffpad::TimeAndPitch& sut, TimeAndPitchSource& source, int numSamples,
   int numChannels, float* const* out)
{
   AudioContainer container { maxBlockSize, numChannels };
   auto numOut = 0;
   while (numOut < numSamples)
   {
      while (sut.getNumAvailableOutputSamples() <= 0)
      {
         source.Pull(container.Get(), maxBlockSize);
         sut.feedAudio(container.Get(), maxBlockSize);
      }
      const auto retrieved = std::min({ numSamples - numOut, maxBlockSize,
                                        sut.getNumAvailableOutputSamples() });
      sut.retrieveAudio(container.Get(), retrieved);
      if (out)
         for (auto i = 0u; i < numChannels; ++i)
            std::copy(
               container.Get()[i], container.Get()[i] + retrieved,
               out[i] + numOut);
      numOut += retrieved;
   }
}
} // namespace

TEST_CASE("TimeAndPitch")
{
   SECTION("yields output with RMS equal to that of input")
   {
      const auto inputPath =
         std::string(CMAKE_SOURCE_DIR) + "/tests/samples/AudacitySpectral.wav";
      // const auto inputPath = "C:/Users/saint/Downloads/square.wav";

      std::vector<std::vector<float>> input;
      WavFileIO::Info info;
      REQUIRE(WavFileIO::Read(inputPath, input, info));
      TimeAndPitchRealSource source { input };
      staffpad::TimeAndPitch sut { info.sampleRate };
      const auto stretchRatio = GENERATE(0.5, 1.0, 2.0);
      // const auto pitchRatio = GENERATE(0.5, 1.0, 2.0);
      constexpr auto pitchRatio = 1.;
      AudioContainer container { maxBlockSize, info.numChannels };
      sut.setup(info.numChannels, maxBlockSize);
      sut.setTimeStretchAndPitchFactor(stretchRatio, pitchRatio);

      // Discard latency samples
      ReadSoManySamples(
         sut, source, sut.getLatencySamplesForStretchRatio(stretchRatio),
         info.numChannels, nullptr);

      const int outSize = info.numFrames * stretchRatio;
      std::vector<std::vector<float>> output(info.numChannels);
      std::vector<float*> outputPtrs(info.numChannels);
      for (auto i = 0u; i < info.numChannels; ++i)
      {
         output[i].resize(outSize);
         outputPtrs[i] = output[i].data();
      }

      ReadSoManySamples(
         sut, source, outSize, info.numChannels, outputPtrs.data());
      const auto inputRms = GetRms(input, info.sampleRate, 1.);
      const auto outputRms = GetRms(output, info.sampleRate, stretchRatio);

      const auto outputPath = "C:/Users/saint/Downloads/test.wav";
      REQUIRE(WavFileIO::Write(outputPath, output, info.sampleRate));

      const auto diff = outputRms - inputRms;
      REQUIRE(std::abs(diff) < 0.02f);
   }
}
