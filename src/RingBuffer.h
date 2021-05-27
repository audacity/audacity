/**********************************************************************

  Audacity: A Digital Audio Editor

  RingBuffer.h

  Dominic Mazzoni

*******************************************************************/

#ifndef __AUDACITY_RING_BUFFER__
#define __AUDACITY_RING_BUFFER__

#include "SampleFormat.h"
#include <atomic>

class RingBuffer {
 public:
   RingBuffer(sampleFormat format, size_t size);
   ~RingBuffer();

   //
   // For the writer only:
   //

   size_t AvailForPut();
   //! Does not apply dithering
   size_t Put(samplePtr buffer, sampleFormat format, size_t samples,
              // optional number of trailing zeroes
              size_t padding = 0);
   size_t Clear(sampleFormat format, size_t samples);

   //
   // For the reader only:
   //

   size_t AvailForGet();
   //! Does not apply dithering
   size_t Get(samplePtr buffer, sampleFormat format, size_t samples);
   size_t Discard(size_t samples);

 private:
   size_t Filled( size_t start, size_t end );
   size_t Free( size_t start, size_t end );

   enum : size_t { CacheLine = 64 };
   /*
    // We will do this in C++17 instead:
   static constexpr size_t CacheLine =
      std::hardware_destructive_interference_size;
    */

   // Align the two atomics to avoid false sharing
   alignas(CacheLine) std::atomic<size_t> mStart { 0 };
   alignas(CacheLine) std::atomic<size_t> mEnd{ 0 };

   const size_t  mBufferSize;

   sampleFormat  mFormat;
   SampleBuffer  mBuffer;
};

#endif /*  __AUDACITY_RING_BUFFER__ */
