/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  PowerSpectrumGetter.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "PowerSpectrumGetter.h"

#include <cassert>
#include <pffft.h>

void PffftSetupDeleter::Pffft_destroy_setup(PFFFT_Setup *p)
{
   pffft_destroy_setup(p);
}

void *PffftAllocatorBase::Pffft_aligned_malloc(size_t nb_bytes)
{
   return pffft_aligned_malloc(nb_bytes);
}

void PffftAllocatorBase::Pffft_aligned_free(void *p)
{
   pffft_aligned_free(p);
}

PffftFloats PffftFloatVector::aligned(PffftAlignedCount c)
{
   return PffftFloats{ data() + c };
}

PffftConstFloats PffftFloatVector::aligned(PffftAlignedCount c) const
{
   return PffftConstFloats{ data() + c };
}

PffftTransformer::PffftTransformer(size_t N)
   : std::unique_ptr<PFFFT_Setup, PffftSetupDeleter>{
      pffft_new_setup(N, PFFFT_REAL)
   }
   , N{ N }
{
}

void PffftTransformer::Reset()
{
   reset();
}

void PffftTransformer::Reset(size_t N)
{
   reset(pffft_new_setup(N, PFFFT_REAL));
}

void PffftTransformer::TransformOrdered(PffftConstFloats input,
   PffftFloats output, PffftFloats work) const
{
   if (!*this) {
      assert(false);
      return;
   }
   pffft_transform_ordered(get(), input.get(), output.get(), work.get(),
      PFFFT_FORWARD);
}

void PffftTransformer::InverseTransformOrdered(PffftConstFloats input,
   PffftFloats output, PffftFloats work, bool renormalize) const
{
   if (!*this) {
      assert(false);
      return;
   }
   const auto out = output.get();
   pffft_transform_ordered(get(), input.get(), out, work.get(),
      PFFFT_BACKWARD);
   if (renormalize)
      std::transform(out, out + N, out, [N = N](float f){ return f / N; });
}

PowerSpectrumGetter::PowerSpectrumGetter(int fftSize)
    : mFftSize { fftSize }
    , mSetup { size_t(fftSize) }
    , mWork(fftSize)
{
}

PowerSpectrumGetter::~PowerSpectrumGetter()
{
}

void PowerSpectrumGetter::operator()(
   PffftFloats alignedBuffer, PffftFloats alignedOutput)
{
   mSetup.TransformOrdered(
      alignedBuffer, alignedBuffer, mWork.aligned());
   const auto buffer = alignedBuffer.get();
   const auto output = alignedOutput.get();
   output[0] = buffer[0] * buffer[0];
   for (auto i = 1; i < mFftSize / 2; ++i)
      output[i] =
         buffer[i * 2] * buffer[i * 2] + buffer[i * 2 + 1] * buffer[i * 2 + 1];
   output[mFftSize / 2] = buffer[1] * buffer[1];
}
