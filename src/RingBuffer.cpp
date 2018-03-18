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
#include "Experimental.h"

#undef REWRITE_RING_BUFFER

// For now enabling the rewrite only for Mac

#ifdef EXPERIMENTAL_REWRITE_RING_BUFFER
#define REWRITE_RING_BUFFER
#elif defined (__APPLE__)
#define REWRITE_RING_BUFFER
#endif

namespace {
   size_t roundUp(size_t n)
   {
#ifdef REWRITE_RING_BUFFER
      // PaUtilRingBuffer requires a power of 2
      if (n == 0)
         return 1;

      size_t prev = n;
      for (size_t temp = n; temp != 0; prev = temp, temp = ((temp - 1) & temp))
         ;
      return prev == n ? prev : prev << 1;
#else
      return n;
#endif
   }
}

RingBuffer::RingBuffer(sampleFormat format, size_t size)
   : mFormat{ format }
   , mBufferSize{ roundUp( std::max<size_t>(size, 64) ) }
   , mBuffer{ mBufferSize, mFormat }
{
#ifdef REWRITE_RING_BUFFER
   PaUtil_InitializeRingBuffer(
      &mRingBuffer, SAMPLE_SIZE(format), mBufferSize, mBuffer.ptr() );
#endif
}

RingBuffer::~RingBuffer()
{
}

size_t RingBuffer::Len()
{
#ifdef REWRITE_RING_BUFFER
   return (mEnd + mBufferSize - mStart) % mBufferSize;
#else
   assert(false);
#endif
}

//
// For the writer only:
//

size_t RingBuffer::AvailForPut()
{
#ifdef REWRITE_RING_BUFFER
   return PaUtil_GetRingBufferWriteAvailable(&mRingBuffer);
#else
   return std::max<size_t>(mBufferSize - Len(), 4) - 4;
#endif
}

size_t RingBuffer::Put(samplePtr buffer, sampleFormat format,
                    size_t samplesToCopy)
{
#ifdef REWRITE_RING_BUFFER
   return PaUtil_WriteRingBuffer(&mRingBuffer, buffer, samplesToCopy);
#else
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
#endif
}

#if 0
// not used
size_t RingBuffer::Clear(sampleFormat format, size_t samplesToClear)
{
   samplesToClear = std::min( samplesToClear, AvailForPut() );
   size_t cleared = 0;
   auto pos = mEnd;

   while(samplesToClear) {
      auto block = std::min( samplesToClear, mBufferSize - pos );

      ClearSamples(mBuffer.ptr(), format, pos, block);

      pos = (pos + block) % mBufferSize;
      samplesToClear -= block;
      cleared += block;
   }

   mEnd = pos;

   return cleared;
}
#endif

//
// For the reader only:
//

size_t RingBuffer::AvailForGet()
{
#ifdef REWRITE_RING_BUFFER
   return PaUtil_GetRingBufferReadAvailable(&mRingBuffer);
#else
   return Len();
#endif
}

size_t RingBuffer::Get(samplePtr buffer, sampleFormat format,
                       size_t samplesToCopy)
{
#ifdef REWRITE_RING_BUFFER
   return PaUtil_ReadRingBuffer(&mRingBuffer, buffer, samplesToCopy);
#else
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
#endif
}

size_t RingBuffer::Discard(size_t samplesToDiscard)
{
#ifdef REWRITE_RING_BUFFER
   PaUtil_AdvanceRingBufferReadIndex(&mRingBuffer, samplesToDiscard);
#else
   samplesToDiscard = std::min( samplesToDiscard, Len() );

   mStart = (mStart + samplesToDiscard) % mBufferSize;
#endif
   return samplesToDiscard;
}
