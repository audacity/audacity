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
float GetRms(const std::vector<std::vector<float>>& x)
{
   return std::sqrt(
      std::accumulate(
         x.begin(), x.end(), 0.f,
         [](const auto& acc, const auto& channel) {
            return acc + std::accumulate(
                            channel.begin(), channel.end(), 0.f,
                            [](const auto& acc, const auto& sample) {
                               return acc + sample * sample;
                            });
         }) /
      (x.size() * x[0].size()));
};
} // namespace

TEST_CASE("TimeAndPitch")
{
   SECTION("yields output with RMS equal to that of input")
   {
      const auto inputPath =
         std::string(CMAKE_SOURCE_DIR) + "/tests/samples/AudacitySpectral.wav";
      std::vector<std::vector<float>> input;
      WavFileIO::Info info;
      REQUIRE(WavFileIO::Read(inputPath, input, info));
      TimeAndPitchRealSource source { input };
      staffpad::TimeAndPitch sut { info.sampleRate };
      const auto stretchRatio = GENERATE(0.5, 1.0, 2.0);
      const auto pitchRatio = GENERATE(0.5, 1.0, 2.0);
      constexpr auto maxBlockSize = 1024;
      AudioContainer container { maxBlockSize, info.numChannels };
      sut.setup(info.numChannels, maxBlockSize);
      sut.setTimeStretchAndPitchFactor(stretchRatio, pitchRatio);
      auto toDiscard = sut.getLatencySamplesForStretchRatio(stretchRatio);
      while (toDiscard > 0)
      {
         while (sut.getNumAvailableOutputSamples() <= 0)
         {
            source.Pull(container.Get(), maxBlockSize);
            sut.feedAudio(container.Get(), maxBlockSize);
         }
         const auto discarded = std::min(toDiscard, maxBlockSize);
         sut.retrieveAudio(container.Get(), discarded);
         toDiscard -= discarded;
      }
      const int outSize = info.numFrames * stretchRatio;
      std::vector<std::vector<float>> output(info.numChannels);
      std::vector<float*> outputPtrs(info.numChannels);
      for (auto i = 0u; i < info.numChannels; ++i)
      {
         output[i].resize(outSize);
         outputPtrs[i] = output[i].data();
      }
      auto numOut = 0;
      while (numOut < outSize)
      {
         while (sut.getNumAvailableOutputSamples() <= 0)
         {
            source.Pull(container.Get(), maxBlockSize);
            sut.feedAudio(container.Get(), maxBlockSize);
         }
         const auto numSamplesToRetrieve = std::min(
            { outSize, maxBlockSize, sut.getNumAvailableOutputSamples() });
         std::vector<float*> offsetPtr(info.numChannels);
         for (auto i = 0u; i < info.numChannels; ++i)
            offsetPtr[i] = outputPtrs[i] + numOut;
         sut.retrieveAudio(offsetPtr.data(), numSamplesToRetrieve);
         numOut += numSamplesToRetrieve;
      }

      const auto inputRms = GetRms(input);
      const auto outputRms = GetRms(output);
      const auto dB = 20 * std::log10(outputRms / inputRms);
      REQUIRE(std::abs(dB) < 0.01f);
   }
}
