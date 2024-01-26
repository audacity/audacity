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
PowerSpectrumGetter::PowerSpectrumGetter(int fftSize)
    : mFftSize { fftSize }
    , mSetup { pffft_new_setup(fftSize, PFFFT_REAL) }
    , mWork(fftSize)
{
}

PowerSpectrumGetter::PowerSpectrumGetter(PowerSpectrumGetter &&other)
   : mFftSize{ other.mFftSize }
   , mSetup{ other.mSetup }
   , mWork{ move(other.mWork) }
{
   other.mSetup = nullptr;
}

PowerSpectrumGetter::~PowerSpectrumGetter()
{
   if (mSetup)
      pffft_destroy_setup(mSetup);
}

void PowerSpectrumGetter::operator()(float* buffer, float* output)
{
   pffft_transform_ordered(mSetup, buffer, buffer, mWork.data(), PFFFT_FORWARD);
   output[0] = buffer[0] * buffer[0];
   for (auto i = 1; i < mFftSize / 2; ++i)
      output[i] =
         buffer[i * 2] * buffer[i * 2] + buffer[i * 2 + 1] * buffer[i * 2 + 1];
   output[mFftSize / 2] = buffer[1] * buffer[1];
}
} // namespace MIR
