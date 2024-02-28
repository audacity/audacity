/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  PffftTransformer.h

  Matthieu Hodgkinson
  Paul Licameli

**********************************************************************/
#pragma once

struct PFFFT_Setup;

#include <memory>
#include <type_traits>
#include <vector>

struct FFT_API PffftSetupDeleter {
   void operator ()(PFFFT_Setup *p){ if (p) Pffft_destroy_setup(p); }
private:
  void Pffft_destroy_setup(PFFFT_Setup *);
};

struct FFT_API PffftAllocatorBase {
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

//! An unsigned number of floats that can span two addresses that are well
//! aligned for pffft
struct PffftAlignedCount
{
   // see source for pffft_aligned_malloc
   static constexpr size_t ByteAlignment = 64;
   static constexpr auto FloatAlignment = ByteAlignment / sizeof(float);

   constexpr PffftAlignedCount() = default;

   //! Construct from some number of floats, rounding up as needed
   constexpr explicit PffftAlignedCount(size_t nFloats)
      : value{
         ((nFloats + (FloatAlignment) - 1) / FloatAlignment) * FloatAlignment }
   {}

   constexpr PffftAlignedCount(const PffftAlignedCount &) = default;
   PffftAlignedCount &operator =(const PffftAlignedCount &) = default;

   //! @invariant `result: result % FloatAlignment == 0`
   constexpr operator size_t() const noexcept { return value; }

private:
   size_t value{};
};

template<typename Integral>
constexpr inline auto operator * (PffftAlignedCount x, Integral y)
   -> std::enable_if_t<
      std::is_unsigned_v<Integral> && sizeof(Integral) <= sizeof(size_t),
      PffftAlignedCount>
{
   return PffftAlignedCount(static_cast<size_t>(x) * y);
}

template<typename Integral>
constexpr inline auto operator * (Integral x, PffftAlignedCount y)
   -> std::enable_if_t<
      std::is_unsigned_v<Integral> && sizeof(Integral) <= sizeof(size_t),
      PffftAlignedCount>
{
   return PffftAlignedCount(x * static_cast<size_t>(y));
}

struct PffftFloatVector;

//! A pointer to an aligned range of floats
/*!
 It can be constructed only defaulted to null or else returned by
 PffftFloatVector::aligned
 It restricts pointer arithmetic to addition of well-aligned increments
 */
struct PffftFloats {
   constexpr PffftFloats() = default;
   constexpr PffftFloats(const PffftFloats&) = default;
   PffftFloats &operator=(const PffftFloats&) = default;

   /*!
    @invariant result is well aligned for pffft
    */
   constexpr float *get() const noexcept { return p; }

   PffftFloats operator +=(PffftAlignedCount c) {
      p += c; return *this;
   }
   friend constexpr PffftFloats operator + (PffftFloats p, PffftAlignedCount c)
   {
      return PffftFloats{ p.p + c };
   }
   friend constexpr PffftFloats operator + (PffftAlignedCount c, PffftFloats p)
   {
      return PffftFloats{ p.p + c };
   }

   explicit operator bool() const noexcept { return p != nullptr; }

private:
   constexpr explicit PffftFloats(float *p) : p{ p } {}
   friend PffftFloatVector;
   float *p{};
};

//! A read-only pointer to an aligned range of floats
/*!
 It can be constructed only defaulted to null, or from PffftFloats,
 or else returned by PffftFloatVector::aligned
 It restricts pointer arithmetic to addition of well-aligned increments.
 */
struct PffftConstFloats {
   constexpr PffftConstFloats() = default;
   constexpr PffftConstFloats(const PffftConstFloats&) = default;
   PffftConstFloats &operator=(const PffftConstFloats&) = default;
   constexpr PffftConstFloats(PffftFloats p) : PffftConstFloats{ p.get() } {}

   /*!
    @invariant result is well aligned for pffft
    */
   constexpr const float *get() const noexcept { return p; }

   PffftConstFloats operator +=(PffftAlignedCount c) {
      p += c; return *this;
   }
   friend constexpr
   PffftConstFloats operator + (PffftConstFloats p, PffftAlignedCount c)
   {
      return PffftConstFloats{ p.p + c };
   }
   friend constexpr
   PffftConstFloats operator + (PffftAlignedCount c, PffftConstFloats p)
   {
      return PffftConstFloats{ p.p + c };
   }

   explicit operator bool() const noexcept { return p != nullptr; }

private:
   explicit constexpr PffftConstFloats(const float *p) : p{ p } {}
   friend PffftFloatVector;
   const float *p{};
};

//! A vector of floats guaranteeing alignment as demanded by pffft
/*!
 It also exposes `aligned` which returns pointers to aligned sub-ranges only

 Usual hazards of pointer invalidation or out-of-bounds subscripting still
 apply.

 Size and capacity are NOT constrained to be aligned.
 */
struct FFT_API PffftFloatVector : std::vector<float, PffftAllocator<float>> {
   using std::vector<float, PffftAllocator<float>>::vector;
   //! @return `data() + c`
   PffftFloats aligned(PffftAlignedCount c = {});
   //! @return `data() + c`
   PffftConstFloats aligned(PffftAlignedCount c = {}) const;

   //! Two-argument overload views the contiguous vector as multiple aligned
   //! rows
   PffftFloats aligned(PffftAlignedCount rowSize, size_t nRow)
   { return aligned(rowSize * nRow); }

   //! Two-argument overload views the contiguous vector as multiple aligned
   //! rows
   PffftConstFloats aligned(PffftAlignedCount rowSize, size_t nRow) const
   { return aligned(rowSize * nRow); }
};

//! Encapsulates allocation of setup and calls to pffft; like a smart pointer
struct FFT_API PffftTransformer
   : private std::unique_ptr<PFFFT_Setup, PffftSetupDeleter>
{
   //! @post `!*this`
   PffftTransformer() = default;

   //! Make construction and resetting go through this non-inline call
   //! @post `*this`
   explicit PffftTransformer(size_t N);

   //! Free resources
   //! @post `!*this`
   void Reset();

   //! Reinitialize resources
   //! @post `*this`
   void Reset(size_t N);

   // Movable but not copyable
   PffftTransformer(PffftTransformer&&) = default;
   PffftTransformer & operator=(PffftTransformer&&) = default;

   using std::unique_ptr<PFFFT_Setup, PffftSetupDeleter>::operator bool;

   //! Invoke pffft_transform_ordered from time to frequency; real inputs only
   //! @pre `*this`
   void TransformOrdered(PffftConstFloats input,
      PffftFloats output, PffftFloats work) const;

   //! Invoke pffft_transform_ordered from frequency to time,
   //! then optionally rescale values by 1/Size()
   //! @pre `*this`
   void InverseTransformOrdered(PffftConstFloats input,
      PffftFloats output, PffftFloats work, bool renormalize = false) const;

   size_t Size() const { return N; };

private:
   size_t N;
};

/*!
 * @brief Much faster that FFT.h's `PowerSpectrum`, at least in Short-Time
 * Fourier Transform-like situations, where many power spectra of the same size
 * are needed. Currently only power spectrum, but may be generalized to other
 * uses.
 */
class FFT_API PowerSpectrumGetter
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
   void operator()(PffftFloats alignedBuffer, PffftFloats alignedOutput);

private:
   const int mFftSize;
   PffftTransformer mSetup;
   PffftFloatVector mWork;
};
