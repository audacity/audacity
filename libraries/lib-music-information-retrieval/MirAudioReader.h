#pragma once

#include <cstddef>

namespace MIR
{
class MirAudioReader
{
public:
   virtual int GetSampleRate() const = 0;
   virtual long long GetNumSamples() const = 0;
   virtual void
   ReadFloats(float* buffer, long long where, size_t numFrames) const = 0;
   virtual ~MirAudioReader() = default;
};
} // namespace MIR
