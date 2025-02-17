/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  FormantShifter.h

  A class for shifting the formants of a voice signal.

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "StaffPad/FourierTransform_pffft.h"
#include "StaffPad/SamplesFloat.h"
#include <complex>
#include <memory>

namespace staffpad::audio {
class FourierTransform;
}

class FormantShifterLoggerInterface;

class FormantShifter
{
public:
    const double cutoffQuefrency;

    FormantShifter(
        int sampleRate, double cutoffQuefrency, FormantShifterLoggerInterface& logger);

    void Reset(size_t fftSize);
    void Reset();

    /*!
     * \brief Processes `spectrum` in place, or does nothing if `Reset(fftSize)`
     * wasn't called or `Reset()` was called since.
     *
     * \details "Shifts" the frequency-domain envelope of the input signal.
     * typically used for formant preservation. Tuned to work best with voice.
     *
     * \param powerSpectrum The power of `spectrum`, i.e., `powerSpectrum[i] =
     * norm(spectrum[i])`, i.e., the square root was NOT taken.
     * \param spectrum The complex spectrum of the input signal.
     * \param factor The factor by which to scale the position of the formants
     * on the frequency axis.
     *
     * \pre `powerSpectrum` and `spectrum` are not null and have size `fftSize /
     * 2 + 1`.
     * \pre `factor > 0`.
     */
    void Process(
        const float* powerSpectrum, std::complex<float>* spectrum, double factor);

private:
    const int mSampleRate;
    FormantShifterLoggerInterface& mLogger;
    std::unique_ptr<staffpad::audio::FourierTransform> mFft;
    staffpad::SamplesComplex mEnvelope;
    staffpad::SamplesReal mCepstrum;
    std::vector<float> mEnvelopeReal;
    std::vector<float> mWeights;
};
