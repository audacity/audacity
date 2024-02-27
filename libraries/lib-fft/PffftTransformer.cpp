/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  PffftTransformer.cpp

  Matthieu Hodgkinson
  Paul Licameli

**********************************************************************/
#include "PffftTransformer.h"

#include <algorithm>
#include <cassert>
#include <cstring>
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

size_t PffftTransformer::MinSize()
{
   return pffft_min_fft_size(PFFFT_REAL);
}

static constexpr bool IsPowerOfTwo(size_t N)
{
   return (N > 0) && ((N & (N - 1)) == 0);
}

bool PffftTransformer::IsAllowedSize(size_t N)
{
   if (N < MinSize())
      return IsPowerOfTwo(N);
   while (0 == (N % 2))
      N /= 2;
   while (0 == (N % 3))
      N /= 3;
   while (0 == (N % 5))
      N /= 5;
   return (N == 1);
}

PffftAlignedCount PffftTransformer::PaddedCount(size_t fftLen)
{
   // The padding and the minimum size constraint happen to be the same
   // in the present pffft implementation, but don't mention that in the
   // header or rely on that elsewhere.
   return PffftAlignedCount{ std::max<size_t>(
      MinSize(), PffftAlignedCount{ fftLen }) };
}

PffftTransformer::PffftTransformer(size_t N)
   : std::unique_ptr<PFFFT_Setup, PffftSetupDeleter>{
      pffft_new_setup(std::max(N, MinSize()), PFFFT_REAL)
   }
   , requestedSize{ N }
{
   assert(IsAllowedSize(N));
}

void PffftTransformer::Reset()
{
   requestedSize = 0;
   reset();
}

void PffftTransformer::Reset(size_t N)
{
   assert(IsAllowedSize(N));
   this->requestedSize = N;
   reset(pffft_new_setup(std::max(N, MinSize()), PFFFT_REAL));
}

void PffftTransformer::TransformOrdered(PffftConstFloats input,
   PffftFloats output, PffftFloats work) const
{
   if (!*this) {
      assert(false);
      return;
   }
   const auto N = std::max(MinSize(), requestedSize);
   if (requestedSize < N) {
      // In frequency domain, we do an interpolation of the desired transform
      // of a small window.
      // This assumes MinSize() is a power of two
      assert(IsPowerOfTwo(MinSize()));
      // Zero pad
      auto pInput = const_cast<float*>(input.get()) + requestedSize;
      memset(pInput, '\0', (N - requestedSize) * sizeof(float));
   }
   pffft_transform_ordered(get(), input.get(), output.get(), work.get(),
      PFFFT_FORWARD);
   if (requestedSize < N) {
      // Now decimate
      auto pOutput = output.get();
      const auto factor = N / requestedSize;
      for (size_t ii = 1, nn = requestedSize; ii < nn; ++ii)
         pOutput[ii] = pOutput[ii * factor];
   }
}

void PffftTransformer::InverseTransformOrdered(PffftConstFloats input,
   PffftFloats output, PffftFloats work, bool renormalize) const
{
   assert(Size() >= MinSize());
   if (!*this) {
      assert(false);
      return;
   }
   const auto out = output.get();
   pffft_transform_ordered(get(), input.get(), out, work.get(),
      PFFFT_BACKWARD);
   if (renormalize) {
      const auto N = std::max(MinSize(), requestedSize);
      std::transform(out, out + N, out, [N](float f){ return f / N; });
   }
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
