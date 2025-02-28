/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  DecimatingMirAudioReader.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "MirTypes.h"

#include <vector>

namespace MIR {
/*!
 * @brief Our MIR operations do not need the full 44.1 or 48kHz resolution
 * typical of audio files. It may change in the future, if we start looking at
 * chromagrams for example, but for now even a certain amount of aliasing isn't
 * an issue. In fact, for onset detection, it may even be beneficial, since it
 * preserves a trace of the highest frequency components by folding them down
 * below the nyquist. Thus we can decimate the audio signal to a certain extent.
 * This is fast and easy to implement, meanwhile reducing dramatically the
 * amount of data and operations.
 */
class DecimatingMirAudioReader : public MirAudioReader
{
public:
    explicit DecimatingMirAudioReader(const MirAudioReader& reader);

    double GetSampleRate() const override;
    long long GetNumSamples() const override;
    void
    ReadFloats(float* buffer, long long start, size_t numFrames) const override;

private:
    const MirAudioReader& mReader;
    const int mDecimationFactor;
    mutable std::vector<float> mBuffer;
};
} // namespace MIR
