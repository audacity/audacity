/**********************************************************************

  Audacity: A Digital Audio Editor

  RingBuffer.cpp

  Dominic Mazzoni

*******************************************************************//*!

\class RingBuffer
\brief Holds streamed audio samples.

  This class is thread-safe, assuming that there is only one
  thread writing, and one thread reading.  If two threads both
  need to read, or both need to write, they need to lock this
  class from outside using their own mutex.

  AvailForPut and AvailForGet may underestimate but will never
  overestimate.

*//*******************************************************************/


#include "RingBuffer.h"

RingBuffer::RingBuffer(sampleFormat format, int size)
{
   mFormat = format;
   mBufferSize = (size > 64? size: 64);
   mStart = 0;
   mEnd = 0;
   mBuffer = NewSamples(mBufferSize, mFormat);
}

RingBuffer::~RingBuffer()
{
   DeleteSamples(mBuffer);
}

int RingBuffer::Len()
{
   return (mEnd + mBufferSize - mStart) % mBufferSize;
}

//
// For the writer only:
//

int RingBuffer::AvailForPut()
{
   return (mBufferSize-4) - Len();
}

int RingBuffer::Put(samplePtr buffer, sampleFormat format,
                    int samplesToCopy)
{
   samplePtr src;
   int block;
   int copied;
   int pos;
   int len = Len();

   if (samplesToCopy > (mBufferSize-4) - len)
      samplesToCopy = (mBufferSize-4) - len;

   src = buffer;
   copied = 0;
   pos = mEnd;

   while(samplesToCopy) {
      block = samplesToCopy;
      if (block > mBufferSize - pos)
         block = mBufferSize - pos;

      CopySamples(src, format,
                  mBuffer + pos * SAMPLE_SIZE(mFormat), mFormat,
                  block);

      src += block * SAMPLE_SIZE(format);
      pos = (pos + block) % mBufferSize;
      samplesToCopy -= block;
      copied += block;
   }

   mEnd = pos;

   return copied;
}

//
// For the reader only:
//

int RingBuffer::AvailForGet()
{
   return Len();
}

int RingBuffer::Get(samplePtr buffer, sampleFormat format,
                    int samplesToCopy)
{
   samplePtr dest;
   int block;
   int copied;
   int len = Len();

   if (samplesToCopy > len)
      samplesToCopy = len;

   dest = buffer;
   copied = 0;

   while(samplesToCopy) {
      block = samplesToCopy;
      if (block > mBufferSize - mStart)
         block = mBufferSize - mStart;

      CopySamples(mBuffer + mStart * SAMPLE_SIZE(mFormat), mFormat,
                  dest, format,
                  block);

      dest += block * SAMPLE_SIZE(format);
      mStart = (mStart + block) % mBufferSize;
      samplesToCopy -= block;
      copied += block;
   }

   return copied;
}

int RingBuffer::Discard(int samplesToDiscard)
{
   int len = Len();

   if (samplesToDiscard > len)
      samplesToDiscard = len;

   mStart = (mStart + samplesToDiscard) % mBufferSize;

   return samplesToDiscard;
}
