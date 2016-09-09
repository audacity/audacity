/**********************************************************************

  Audacity: A Digital Audio Editor

  RingBuffer.h

  Dominic Mazzoni

*******************************************************************/

#ifndef __AUDACITY_RING_BUFFER__
#define __AUDACITY_RING_BUFFER__

#include "SampleFormat.h"

class RingBuffer {
 public:
   RingBuffer(sampleFormat format, size_t size);
   ~RingBuffer();

   //
   // For the writer only:
   //

   size_t AvailForPut();
   size_t Put(samplePtr buffer, sampleFormat format, size_t samples);

   //
   // For the reader only:
   //

   size_t AvailForGet();
   size_t Get(samplePtr buffer, sampleFormat format, size_t samples);
   size_t Discard(size_t samples);

 private:
   size_t Len();

   sampleFormat  mFormat;
   size_t        mStart { 0 };
   size_t        mEnd { 0 };
   size_t        mBufferSize;
   SampleBuffer  mBuffer;
};

#endif /*  __AUDACITY_RING_BUFFER__ */
