/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  PowerSpectrumGetter.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "PowerSpectrumGetter.h"

#include <pffft.h>

namespace MIR
{
void PffftSetupDeleter::Pffft_destroy_setup(PFFFT_Setup *p)
{
   pffft_destroy_setup(p);
}

PowerSpectrumGetter::PowerSpectrumGetter(int fftSize)
    : mFftSize { fftSize }
    , mSetup { pffft_new_setup(fftSize, PFFFT_REAL) }
    , mWork(fftSize)
{
}

PowerSpectrumGetter::~PowerSpectrumGetter()
{
}

void PowerSpectrumGetter::operator()(float* buffer, float* output)
{
   pffft_transform_ordered(mSetup.get(), buffer, buffer, mWork.data(), PFFFT_FORWARD);
   output[0] = buffer[0] * buffer[0];
   for (auto i = 1; i < mFftSize / 2; ++i)
      output[i] =
         buffer[i * 2] * buffer[i * 2] + buffer[i * 2 + 1] * buffer[i * 2 + 1];
   output[mFftSize / 2] = buffer[1] * buffer[1];
}
} // namespace MIR
