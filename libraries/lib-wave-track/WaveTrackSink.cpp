/**********************************************************************

  Audacity: A Digital Audio Editor

  @file WaveTrackSink.cpp

  Dominic Mazzoni
  Vaughan Johnson
  Martyn Shaw

  Paul Licameli split from PerTrackEffect.cpp

**********************************************************************/
#include "WaveTrackSink.h"

#include "AudioGraphBuffers.h"
#include "TimeWarper.h"
#include "WaveTrack.h"
#include <cassert>

WaveTrackSink::WaveTrackSink(WaveTrack &left, WaveTrack *pRight,
   sampleCount start, bool isGenerator, bool isProcessor,
   sampleFormat effectiveFormat
)  : mLeft{ left }, mpRight{ pRight }
   , mGenLeft{ isGenerator ? left.EmptyCopy() : nullptr }
   , mGenRight{ pRight && isGenerator ? pRight->EmptyCopy() : nullptr }
   , mIsProcessor{ isProcessor }
   , mEffectiveFormat{ effectiveFormat }
   , mOutPos{ start }
{
}

WaveTrackSink::~WaveTrackSink() = default;

bool WaveTrackSink::AcceptsBuffers(const Buffers &buffers) const
{
   return buffers.Channels() > 0;
}

bool WaveTrackSink::Acquire(Buffers &data)
{
   if (data.BlockSize() <= data.Remaining()) {
      // post is satisfied
   }
   else
      // Output buffers have (mostly) filled
      // (less than one block remains; maybe nonzero because of samples
      // discarded for initial latency correction)
      DoConsume(data);
   return true;
}

bool WaveTrackSink::Release(const Buffers &, size_t)
{
   // May become non-trivial later
   return true;
}

void WaveTrackSink::DoConsume(Buffers &data)
{
   // Satisfy pre of GetReadPosition()
   assert(data.Channels() > 0);
   const auto inputBufferCnt = data.Position();
   if (inputBufferCnt > 0) {
      // Some data still unwritten
      if (mIsProcessor) {
         mLeft.Set(data.GetReadPosition(0),
            floatSample, mOutPos, inputBufferCnt, mEffectiveFormat);
         if (mpRight)
            mpRight->Set(data.GetReadPosition(1),
               floatSample, mOutPos, inputBufferCnt, mEffectiveFormat);
      }
      else if (mGenLeft) {
         mGenLeft->Append(data.GetReadPosition(0),
            floatSample, inputBufferCnt);
         if (mGenRight)
            mGenRight->Append(data.GetReadPosition(1),
               floatSample, inputBufferCnt);
      }
      // Satisfy post
      data.Rewind();
      // Bump to the next track position
      mOutPos += inputBufferCnt;
   }
   else {
      // Position is zero, therefore Remaining() is a positive multiple of
      // block size
   }
   // assert the post
   assert(data.BlockSize() <= data.Remaining());
}

void WaveTrackSink::Flush(Buffers &data, const double t0, const double t1)
{
   DoConsume(data);
   if (mGenLeft) {
      // Transfer the data from the temporary tracks to the actual ones
      mGenLeft->Flush();
      // mT1 gives us the NEW selection. We want to replace up to GetSel1().
      PasteTimeWarper warper{ t1, t0 + mGenLeft->GetEndTime() };
      mLeft.ClearAndPaste(t0, t1, mGenLeft.get(), true, true, &warper);
      if (mGenRight) {
         mGenRight->Flush();
         mpRight->ClearAndPaste(t0, t1, mGenRight.get(), true, true, &warper);
      }
   }
}
