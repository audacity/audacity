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

RingBuffer::RingBuffer(sampleFormat format, size_t size)
   : mFormat{ format }
   , mBufferSize{ std::max<size_t>(size, 64) }
   , mBuffer{ mBufferSize, mFormat }
{
}

RingBuffer::~RingBuffer()
{
}

size_t RingBuffer::Len()
{
   return (mEnd + mBufferSize - mStart) % mBufferSize;
}

//
// For the writer only:
//

size_t RingBuffer::AvailForPut()
{
   return std::max<size_t>(mBufferSize - Len(), 4) - 4;
}

size_t RingBuffer::Put(samplePtr buffer, sampleFormat format,
                    size_t samplesToCopy)
{
   samplesToCopy = std::min( samplesToCopy, AvailForPut() );
   auto src = buffer;
   size_t copied = 0;
   auto pos = mEnd;

   while(samplesToCopy) {
      auto block = std::min( samplesToCopy, mBufferSize - pos );

      CopySamples(src, format,
                  mBuffer.ptr() + pos * SAMPLE_SIZE(mFormat), mFormat,
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

size_t RingBuffer::AvailForGet()
{
   return Len();
}

size_t RingBuffer::Get(samplePtr buffer, sampleFormat format,
                       size_t samplesToCopy)
{
   samplesToCopy = std::min( samplesToCopy, Len() );
   auto dest = buffer;
   size_t copied = 0;

   while(samplesToCopy) {
      auto block = std::min( samplesToCopy, mBufferSize - mStart );

      CopySamples(mBuffer.ptr() + mStart * SAMPLE_SIZE(mFormat), mFormat,
                  dest, format,
                  block);

      dest += block * SAMPLE_SIZE(format);
      mStart = (mStart + block) % mBufferSize;
      samplesToCopy -= block;
      copied += block;
   }

   return copied;
}

size_t RingBuffer::Discard(size_t samplesToDiscard)
{
   samplesToDiscard = std::min( samplesToDiscard, Len() );

   mStart = (mStart + samplesToDiscard) % mBufferSize;

   return samplesToDiscard;
}
