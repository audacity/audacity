/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  LockFreeQueue.h

  Matthieu Hodgkinson
  Moved from MeterPanel.h

**********************************************************************/
#pragma once

#include "MemoryX.h"
#include <atomic>
#include <cassert>

// Single-producer, single-consumer thread-safe queue of update messages
template<typename T>
class LockFreeQueue : public SharedNonInterfering<LockFreeQueue<T> >
{
public:
    explicit LockFreeQueue(size_t maxLen);
    ~LockFreeQueue();

    bool Put(const T& msg);
    bool Get(T& msg);

    void Clear();

private:
    // Align the two atomics to avoid false sharing
    // mStart is written only by the reader, mEnd by the writer
    NonInterfering<std::atomic<size_t> > mStart { 0 }, mEnd { 0 };

    const size_t mBufferSize;
    ArrayOf<T> mBuffer { mBufferSize };
};

template<typename T>
LockFreeQueue<T>::LockFreeQueue(size_t maxLen)
    : mBufferSize(maxLen)
{
    Clear();
}

// destructor
template<typename T> LockFreeQueue<T>::~LockFreeQueue()
{
}

template<typename T> void LockFreeQueue<T>::Clear()
{
    mStart.store(0);
    mEnd.store(0);
}

// Add a message to the end of the queue.  Return false if the
// queue was full.
template<typename T> bool LockFreeQueue<T>::Put(const T& msg)
{
    auto start = mStart.load(std::memory_order_acquire);
    auto end = mEnd.load(std::memory_order_relaxed);
    // mStart can be greater than mEnd because it is all mod mBufferSize
    assert((end + mBufferSize - start) >= 0);
    int len = (end + mBufferSize - start) % mBufferSize;

    // Never completely fill the queue, because then the
    // state is ambiguous (mStart==mEnd)
    if (len + 1 >= (int)(mBufferSize)) {
        return false;
    }

    // wxLogDebug(wxT("Put: %s"), msg.toString());

    mBuffer[end] = msg;
    mEnd.store((end + 1) % mBufferSize, std::memory_order_release);

    return true;
}

// Get the next message from the start of the queue.
// Return false if the queue was empty.
template<typename T> bool LockFreeQueue<T>::Get(T& msg)
{
    auto start = mStart.load(std::memory_order_relaxed);
    auto end = mEnd.load(std::memory_order_acquire);
    int len = (end + mBufferSize - start) % mBufferSize;

    if (len == 0) {
        return false;
    }

    msg = mBuffer[start];
    mStart.store((start + 1) % mBufferSize, std::memory_order_release);

    return true;
}
