/**********************************************************************

  Audacity: A Digital Audio Editor

  RingBuffer.h

  Dominic Mazzoni

*******************************************************************/

#ifndef __AUDACITY_RING_BUFFER__
#define __AUDACITY_RING_BUFFER__

#include "NonInterfering.h"
#include "SampleFormat.h"
#include <atomic>

class RingBuffer final : public NonInterferingBase {
 public:
   RingBuffer(sampleFormat format, size_t size);
   ~RingBuffer();

   //
   // For the writer only:
   //

   size_t AvailForPut() const;
   //! Reader may concurrently cause a decrease of what this returns
   size_t WrittenForGet() const;
   //! Does not apply dithering
   size_t Put(constSamplePtr buffer, sampleFormat format, size_t samples,
              // optional number of trailing zeroes
              size_t padding = 0);
   //! Remove an initial segment of data that has been Put but not Flushed yet
   /*!
    @return how many were unput
    */
   size_t Unput(size_t size);
   size_t Clear(sampleFormat format, size_t samples);
   //! Get access to written but unflushed data, which is in at most two blocks
   //! Excludes the padding of the most recent Put()
   std::pair<samplePtr, size_t> GetUnflushed(unsigned iBlock);
   //! Flush after a sequence of Put (and/or Clear) calls to let consumer see
   void Flush();

   //
   // For the reader only:
   //

   size_t AvailForGet() const;
   //! Does not apply dithering
   size_t Get(samplePtr buffer, sampleFormat format, size_t samples);
   size_t Discard(size_t samples);

 private:
   size_t Filled(size_t start, size_t end) const;
   size_t Free(size_t start, size_t end) const;

   size_t mWritten{0};
   size_t mLastPadding{0};

   // Align the two atomics to avoid false sharing
   NonInterfering< std::atomic<size_t> > mStart{ 0 }, mEnd{ 0 };

   const size_t  mBufferSize;

   const sampleFormat  mFormat;
   const SampleBuffer  mBuffer;
};

#endif /*  __AUDACITY_RING_BUFFER__ */
