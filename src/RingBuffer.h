/**********************************************************************

  Audacity: A Digital Audio Editor

  RingBuffer.h

  Dominic Mazzoni

*******************************************************************/

#ifndef __AUDACITY_RING_BUFFER__
#define __AUDACITY_RING_BUFFER__

#include "SampleFormat.h"
#include <atomic>

class RingBuffer final : public NonInterferingBase {
 public:
   RingBuffer(sampleFormat format, size_t size);
   ~RingBuffer();

   //
   // For the writer only:
   //

   size_t AvailForPut();
   //! Does not apply dithering
   size_t Put(constSamplePtr buffer, sampleFormat format, size_t samples,
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

   // Align the two atomics to avoid false sharing
   NonInterfering< std::atomic<size_t> > mStart { 0 }, mEnd{ 0 };

   const size_t  mBufferSize;

   sampleFormat  mFormat;
   SampleBuffer  mBuffer;
};

#endif /*  __AUDACITY_RING_BUFFER__ */
