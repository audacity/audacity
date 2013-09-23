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
   int Put(samplePtr buffer, sampleFormat format, int samples, int stride = 1);

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

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 43b69817-059c-4f4a-87e5-b19b2bcc75fb

