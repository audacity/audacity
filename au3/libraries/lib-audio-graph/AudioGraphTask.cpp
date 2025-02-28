/**********************************************************************

  Audacity: A Digital Audio Editor

  @file AudioGraphTask.cpp

  Dominic Mazzoni
  Vaughan Johnson
  Martyn Shaw

  Paul Licameli split from PerTrackEffect.cpp

**********************************************************************/
#include "AudioGraphTask.h"
#include "AudioGraphBuffers.h"
#include "AudioGraphSink.h"
#include "AudioGraphSource.h"
#include "SampleCount.h"
#include <cassert>

AudioGraph::Task::Task(Source& source, Buffers& buffers, Sink& sink)
    : mSource{source}, mBuffers{buffers}, mSink{sink}
{
    assert(source.AcceptsBlockSize(buffers.BlockSize()));
    assert(source.AcceptsBuffers(buffers));
    assert(sink.AcceptsBuffers(buffers));
}

bool AudioGraph::Task::RunLoop()
{
    // Satisfy invariant initially
    mBuffers.Rewind();
    Status status{};
    do {
        assert(mBuffers.Remaining() >= mBuffers.BlockSize());
        status = RunOnce();
    } while (status == Status::More);
    return status == Status::Done;
}

auto AudioGraph::Task::RunOnce() -> Status
{
    const auto blockSize = mBuffers.BlockSize();
    assert(mBuffers.Remaining() >= blockSize); // pre

#ifndef NDEBUG
    // Variable used only for a loop termination sanity check
    std::optional<sampleCount> oldRemaining;
    if (mRanOnce) {
        oldRemaining.emplace(mSource.Remaining());
    }
    // else Remaining() may not be meaningful
#endif

    if (auto oCurBlockSize = mSource.Acquire(mBuffers, blockSize)) {
        const auto curBlockSize = *oCurBlockSize;
#ifndef NDEBUG
        mRanOnce = true;
        const auto remaining = mSource.Remaining();
        // Assert a post of Acquire which is part of proof of termination
        assert(!mSource.Terminates() || !oldRemaining
               || *oldRemaining == remaining);
        oldRemaining.emplace(remaining);
        // Assert another post that guarantees progress (even if not terminating)
        assert(blockSize == 0 || remaining == 0 || curBlockSize > 0);
#endif
        if (curBlockSize == 0) {
            // post (same as pre) obviously preserved
            return Status::Done;
        }

        // post of source.Acquire() satisfies pre of sink.Release()
        assert(curBlockSize <= blockSize);
        if (!mSink.Release(mBuffers, curBlockSize)) {
            return Status::Fail;
        }

        // This may break the post
        mBuffers.Advance(curBlockSize);

        // posts of source.Acquire() and source.Release()
        // give termination guarantee
        assert(mSource.Remaining() == 0 || curBlockSize > 0);
        if (!mSource.Release()) {
            return Status::Fail;
        }
#ifndef NDEBUG
        // Assert a post of Release
        assert(!mSource.Terminates()
               || mSource.Remaining() == remaining - curBlockSize);
#endif

        // Reestablish the post
        if (!mSink.Acquire(mBuffers)) {
            return Status::Fail;
        }
        assert(mBuffers.Remaining() >= blockSize);

        return Status::More;
    } else {
        return Status::Fail;
    }
}
