/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file spinlock.h

  Vitaly Sverchinsky

**********************************************************************/

#pragma once

#include <atomic>
#include <thread>

/**
 * \brief Intended for locking of resources that are only
 * lightly contended and locked for very short times,
 * can be used with std::lock_guard.
 */
class spinlock
{
    std::atomic_flag flag = ATOMIC_FLAG_INIT;
public:
    ~spinlock() { }

    void lock()
    {
        for (unsigned i = 0; flag.test_and_set(std::memory_order_acquire); ++i) {
            if (i & 1) {
                std::this_thread::yield();
            }
        }
    }

    void unlock()
    {
        flag.clear(std::memory_order_release);
    }
};
