/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  PowerSpectrumGetter.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

struct PFFFT_Setup;

#include <vector>

namespace MIR
{
/*!
 * @brief Much faster that FFT.h's `PowerSpectrum`, at least in Short-Time
 * Fourier Transform-like situations, where many power spectra of the same size
 * are needed. Currently only power spectrum, but may be generalized to other
 * uses.
 */
class PowerSpectrumGetter
{
public:
   explicit PowerSpectrumGetter(int fftSize);
   ~PowerSpectrumGetter();

   // Please implement if needed. The dtor calls `delete` on its naked
   // pointer members, so default-copying this class would lead to crashes.
   PowerSpectrumGetter(const PowerSpectrumGetter&) = delete;

   /*!
    * @brief Returns a pointer to the data to be transformed. User must fill it
    * with `fftSize` samples before calling `Process()`.
    */
   float* GetInputPtr();

   /*!
    * @brief Computes the power spectrum of data pointed at by `GetInputPtr()`
    * into `output`.
    * @param output `fftSize / 2 + 1` samples.
    */
   void Process(float* output);

private:
   const int mFftSize;
   PFFFT_Setup* mSetup;
   float* mData;
   float* mWork;
};
} // namespace MIR
