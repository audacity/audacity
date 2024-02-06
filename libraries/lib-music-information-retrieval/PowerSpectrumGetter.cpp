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
    , mData { reinterpret_cast<float*>(
         pffft_aligned_malloc(fftSize * sizeof(float))) }
    , mWork { reinterpret_cast<float*>(
         pffft_aligned_malloc(fftSize * sizeof(float))) }
{
   std::fill(mData, mData + fftSize, 0.f);
   std::fill(mWork, mWork + fftSize, 0.f);
}

PowerSpectrumGetter::~PowerSpectrumGetter()
{
   pffft_destroy_setup(mSetup);
   pffft_aligned_free(mData);
   pffft_aligned_free(mWork);
}

float* PowerSpectrumGetter::GetInputPtr()
{
   return mData;
}

void PowerSpectrumGetter::Process(float* output)
{
   pffft_transform_ordered(mSetup, mData, mData, mWork, PFFFT_FORWARD);
   output[0] = mData[0] * mData[0];
   for (auto i = 1; i < mFftSize / 2; ++i)
      output[i] =
         mData[i * 2] * mData[i * 2] + mData[i * 2 + 1] * mData[i * 2 + 1];
   output[mFftSize / 2] = mData[1] * mData[1];
}
} // namespace MIR
