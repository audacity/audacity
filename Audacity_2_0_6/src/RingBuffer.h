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
   RingBuffer(sampleFormat format, int size);
   ~RingBuffer();

   //
   // For the writer only:
   //

   int AvailForPut();
   int Put(samplePtr buffer, sampleFormat format, int samples);

   //
   // For the reader only:
   //

   int AvailForGet();
   int Get(samplePtr buffer, sampleFormat format, int samples);
   int Discard(int samples);

 private:
   int Len();

   sampleFormat  mFormat;
   int           mStart;
   int           mEnd;
   int           mBufferSize;
   samplePtr     mBuffer;
};

#endif /*  __AUDACITY_RING_BUFFER__ */
