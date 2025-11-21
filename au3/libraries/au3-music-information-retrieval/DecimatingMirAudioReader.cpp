/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  DecimatingMirAudioReader.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "DecimatingMirAudioReader.h"

#include <cmath>
#include <vector>

namespace MIR {
DecimatingMirAudioReader::DecimatingMirAudioReader(const MirAudioReader& reader)
    : mReader{reader}
    , mDecimationFactor{
                        // Input rate divided by this integer will be as close as possible to
                        // 24kHz and not greater.
                        static_cast<int>(std::ceil(reader.GetSampleRate() / 24000.))
                        }
{
}

double DecimatingMirAudioReader::GetSampleRate() const
{
    return 1. * mReader.GetSampleRate() / mDecimationFactor;
}

long long DecimatingMirAudioReader::GetNumSamples() const
{
    // Return the floor
    return mReader.GetNumSamples() / mDecimationFactor;
}

void DecimatingMirAudioReader::ReadFloats(
    float* decimated, long long decimatedStart, size_t numDecimatedFrames) const
{
    const auto numFrames = numDecimatedFrames * mDecimationFactor;
    if (mBuffer.size() < numFrames) {
        mBuffer.resize(numFrames);
    }
    const auto start = decimatedStart * mDecimationFactor;
    mReader.ReadFloats(mBuffer.data(), start, numFrames);
    for (auto i = 0; i < numDecimatedFrames; ++i) {
        decimated[i] = mBuffer[i * mDecimationFactor];
    }
}
} // namespace MIR
