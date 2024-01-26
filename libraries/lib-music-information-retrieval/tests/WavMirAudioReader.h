#pragma once

#include "MirAudioReader.h"

#include <optional>
#include <string>
#include <vector>

namespace MIR
{
class WavMirAudioReader : public MirAudioReader
{
public:
   WavMirAudioReader(
      const std::string& filename, std::optional<double> timeLimit = {});

   double GetSampleRate() const override;
   long long GetNumSamples() const override;
   void
   ReadFloats(float* buffer, long long start, size_t numFrames) const override;
   std::unique_ptr<MirAudioReader> Clone() const override;

private:
   const std::shared_ptr<const std::vector<float>> mpSamples;
   const double mSampleRate = 0.;
};
} // namespace MIR
