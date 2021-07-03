/**********************************************************************

  Audacity: A Digital Audio Editor

  RingBuffer.cpp

  Dominic Mazzoni
  Paul Licameli

*******************************************************************//*!

\class RingBuffer
\brief Holds streamed audio samples.

  Assuming that there is only one thread writing, and one thread reading,
  this class implements a lock-free thread-safe bounded queue of samples
  with atomic variables that contain the first filled and free positions.
                                                                     
  If two threads both need to read, or both need to write, they need to lock
  this class from outside using their own mutex.

  AvailForPut and AvailForGet may underestimate but will never
  overestimate.

*//*******************************************************************/


#include "RingBuffer.h"
#include "Dither.h"

RingBuffer::RingBuffer(sampleFormat format, size_t size)
   : mFormat{ format }
   , mBufferSize{ std::max<size_t>(size, 64) }
   , mBuffer{ mBufferSize, mFormat }
{
}

RingBuffer::~RingBuffer()
{
}

// Calculations of free and filled space, given snapshots taken of the start
// and end values

size_t RingBuffer::Filled( size_t start, size_t end )
{
   return (end + mBufferSize - start) % mBufferSize;
}

size_t RingBuffer::Free( size_t start, size_t end )
{
   return std::max<size_t>(mBufferSize - Filled( start, end ), 4) - 4;
}

//
// For the writer only:
// Only writer writes the end, so it can read it again relaxed
// And it reads the start written by reader, with acquire order,
// so that any reading done in Get() happens-before any reuse of the space.
//

size_t RingBuffer::AvailForPut()
{
   auto start = mStart.load( std::memory_order_relaxed );
   auto end = mEnd.load( std::memory_order_relaxed );
   return Free( start, end );

   // Reader might increase the available free space after return, but will
   // never decrease it, so writer can safely assume this much at least
}

size_t RingBuffer::Put(samplePtr buffer, sampleFormat format,
                    size_t samplesToCopy, size_t padding)
{
   auto start = mStart.load( std::memory_order_acquire );
   auto end = mEnd.load( std::memory_order_relaxed );
   const auto free = Free( start, end );
   samplesToCopy = std::min( samplesToCopy, free );
   padding = std::min( padding, free - samplesToCopy );
   auto src = buffer;
   size_t copied = 0;
   auto pos = end;

   while ( samplesToCopy ) {
      auto block = std::min( samplesToCopy, mBufferSize - pos );

      CopySamples(src, format,
                  mBuffer.ptr() + pos * SAMPLE_SIZE(mFormat), mFormat,
                  block, DitherType::none);

      src += block * SAMPLE_SIZE(format);
      pos = (pos + block) % mBufferSize;
      samplesToCopy -= block;
      copied += block;
   }

   while ( padding ) {
      const auto block = std::min( padding, mBufferSize - pos );
      ClearSamples( mBuffer.ptr(), mFormat, pos, block );
      pos = (pos + block) % mBufferSize;
      padding -= block;
      copied += block;
   }

   // Atomically update the end pointer with release, so the nonatomic writes
   // just done to the buffer don't get reordered after
   mEnd.store(pos, std::memory_order_release);

   return copied;
}

size_t RingBuffer::Clear(sampleFormat format, size_t samplesToClear)
{
   auto start = mStart.load( std::memory_order_acquire );
   auto end = mEnd.load( std::memory_order_relaxed );
   samplesToClear = std::min( samplesToClear, Free( start, end ) );
   size_t cleared = 0;
   auto pos = end;

   while(samplesToClear) {
      auto block = std::min( samplesToClear, mBufferSize - pos );

      ClearSamples(mBuffer.ptr(), format, pos, block);

      pos = (pos + block) % mBufferSize;
      samplesToClear -= block;
      cleared += block;
   }

   // Atomically update the end pointer with release, so the nonatomic writes
   // just done to the buffer don't get reordered after
   mEnd.store(pos, std::memory_order_release);

   return cleared;
}

//
// For the reader only:
// Only reader writes the start, so it can read it again relaxed
// But it reads the end written by the writer, who also sends sample data
// with the changes of end; therefore that must be read with acquire order
// if we do more than merely query the size or throw samples away
//

size_t RingBuffer::AvailForGet()
{
   auto end = mEnd.load( std::memory_order_relaxed ); // get away with it here
   auto start = mStart.load( std::memory_order_relaxed );
   return Filled( start, end );

   // Writer might increase the available samples after return, but will
   // never decrease them, so reader can safely assume this much at least
}

size_t RingBuffer::Get(samplePtr buffer, sampleFormat format,
                       size_t samplesToCopy)
{
   // Must match the writer's release with acquire for well defined reads of
   // the buffer
   auto end = mEnd.load( std::memory_order_acquire );
   auto start = mStart.load( std::memory_order_relaxed );
   samplesToCopy = std::min( samplesToCopy, Filled( start, end ) );
   auto dest = buffer;
   size_t copied = 0;

   while(samplesToCopy) {
      auto block = std::min( samplesToCopy, mBufferSize - start );

      CopySamples(mBuffer.ptr() + start * SAMPLE_SIZE(mFormat), mFormat,
                  dest, format,
                  block, DitherType::none);

      dest += block * SAMPLE_SIZE(format);
      start = (start + block) % mBufferSize;
      samplesToCopy -= block;
      copied += block;
   }

   // Communicate to writer that we have consumed some data,
   // with nonrelaxed ordering
   mStart.store( start, std::memory_order_release );

   return copied;
}

size_t RingBuffer::Discard(size_t samplesToDiscard)
{
   auto end = mEnd.load( std::memory_order_relaxed ); // get away with it here
   auto start = mStart.load( std::memory_order_relaxed );
   samplesToDiscard = std::min( samplesToDiscard, Filled( start, end ) );

   // Communicate to writer that we have skipped some data, and that's all
   mStart.store((start + samplesToDiscard) % mBufferSize,
                std::memory_order_relaxed);

   return samplesToDiscard;
}
