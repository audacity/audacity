/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  PowerSpectrumGetter.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

struct PFFFT_Setup;

#include <memory>
#include <vector>
#include "pffft.h"

namespace MIR
{
struct PffftSetupDeleter {
   void operator ()(PFFFT_Setup *p){ if (p) Pffft_destroy_setup(p); }
private:
  void Pffft_destroy_setup(PFFFT_Setup *);
};
using PffftSetupHolder = std::unique_ptr<PFFFT_Setup, PffftSetupDeleter>;

struct PffftAllocatorBase {
   static void *Pffft_aligned_malloc(size_t nb_bytes);
   static void Pffft_aligned_free(void *);
};

//! Aligned memory is required by pffft, so this defines an allocator
template<typename T> struct PffftAllocator
   : std::allocator<T>, protected PffftAllocatorBase
{
   PffftAllocator() {}
   PffftAllocator(const PffftAllocator&) {}
   template<typename U> PffftAllocator(const PffftAllocator<U>&) {}

   template<typename U> struct rebind { using other = PffftAllocator<U>; };
   T* allocate(std::size_t n, const void *) {
      return allocate(n);
   }
   T* allocate(std::size_t n) {
      return static_cast<T*>(Pffft_aligned_malloc(n * sizeof(T)));
   }
   void deallocate(T *p, std::size_t n) {
      if (p) Pffft_aligned_free(p);
   }
};

//! A vector of floats guaranteeing alignment as demanded by pffft
struct PffftFloatVector : std::vector<float, PffftAllocator<float>> {
   using std::vector<float, PffftAllocator<float>>::vector;
};

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

   /*!
    * @brief Computes the power spectrum of `buffer` into `output`.
    * @param buffer Input samples of size `fftSize`. Also gets used as
    * placeholder and gets overwritten, so copy your data elsewhere if you need
    * it again afterwards.
    * @param output `fftSize / 2 + 1` samples.
    */
   void operator()(float* buffer, float* output);

private:
   const int mFftSize;
   PffftSetupHolder mSetup;
   PffftFloatVector mWork;
};
} // namespace MIR
