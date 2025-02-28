/*
  Copyright 2011 David Robillard <http://drobilla.net>

  Permission to use, copy, modify, and/or distribute this software for any
  purpose with or without fee is hereby granted, provided that the above
  copyright notice and this permission notice appear in all copies.

  THIS SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*/

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#ifdef HAVE_MLOCK
#    include <sys/mman.h>
#    define ZIX_MLOCK(ptr, size) mlock((ptr), (size))
#elif defined(_WIN32)
#    include <windows.h>
#    define ZIX_MLOCK(ptr, size) VirtualLock((ptr), (size))
#else
#    pragma message("warning: No memory locking, possible RT violations")
#    define ZIX_MLOCK(ptr, size)
#endif

#if defined(__APPLE__)
#    include <libkern/OSAtomic.h>
#    define ZIX_FULL_BARRIER() OSMemoryBarrier()
#elif defined(_WIN32)
#    include <windows.h>
#    define ZIX_FULL_BARRIER() MemoryBarrier()
#elif (__GNUC__ > 4) || (__GNUC__ == 4 && __GNUC_MINOR__ >= 1)
#    define ZIX_FULL_BARRIER() __sync_synchronize()
#else
#    pragma message("warning: No memory barriers, possible SMP bugs")
#    define ZIX_FULL_BARRIER()
#endif

/* No support for any systems with separate read and write barriers */
#define ZIX_READ_BARRIER() ZIX_FULL_BARRIER()
#define ZIX_WRITE_BARRIER() ZIX_FULL_BARRIER()

#include "ring.h"

struct ZixRingImpl {
    uint32_t write_head;    ///< Read index into buf
    uint32_t read_head;     ///< Write index into buf
    uint32_t size;          ///< Size (capacity) in bytes
    uint32_t size_mask;     ///< Mask for fast modulo
    char* buf;              ///< Contents
};

static inline uint32_t
next_power_of_two(uint32_t size)
{
    // http://graphics.stanford.edu/~seander/bithacks.html#RoundUpPowerOf2
    size--;
    size |= size >> 1;
    size |= size >> 2;
    size |= size >> 4;
    size |= size >> 8;
    size |= size >> 16;
    size++;
    return size;
}

ZixRing*
zix_ring_new(uint32_t size)
{
    ZixRing* ring = (ZixRing*)malloc(sizeof(ZixRing));
    ring->write_head = 0;
    ring->read_head  = 0;
    ring->size       = next_power_of_two(size);
    ring->size_mask  = ring->size - 1;
    ring->buf        = (char*)malloc(ring->size);
    return ring;
}

void
zix_ring_free(ZixRing* ring)
{
    free(ring->buf);
    free(ring);
}

void
zix_ring_mlock(ZixRing* ring)
{
    ZIX_MLOCK(ring, sizeof(ZixRing));
    ZIX_MLOCK(ring->buf, ring->size);
}

void
zix_ring_reset(ZixRing* ring)
{
    ring->write_head = 0;
    ring->read_head  = 0;
}

static inline uint32_t
read_space_internal(const ZixRing* ring, uint32_t r, uint32_t w)
{
    if (r < w) {
        return w - r;
    } else {
        return (w - r + ring->size) & ring->size_mask;
    }
}

uint32_t
zix_ring_read_space(const ZixRing* ring)
{
    return read_space_internal(ring, ring->read_head, ring->write_head);
}

static inline uint32_t
write_space_internal(const ZixRing* ring, uint32_t r, uint32_t w)
{
    if (r == w) {
        return ring->size - 1;
    } else if (r < w) {
        return ((r - w + ring->size) & ring->size_mask) - 1;
    } else {
        return (r - w) - 1;
    }
}

uint32_t
zix_ring_write_space(const ZixRing* ring)
{
    return write_space_internal(ring, ring->read_head, ring->write_head);
}

uint32_t
zix_ring_capacity(const ZixRing* ring)
{
    return ring->size - 1;
}

static inline uint32_t
peek_internal(const ZixRing* ring, uint32_t r, uint32_t w,
              uint32_t size, void* dst)
{
    if (read_space_internal(ring, r, w) < size) {
        return 0;
    }

    if (r + size < ring->size) {
        memcpy(dst, &ring->buf[r], size);
    } else {
        const uint32_t first_size = ring->size - r;
        memcpy(dst, &ring->buf[r], first_size);
        memcpy((char*)dst + first_size, &ring->buf[0], size - first_size);
    }

    return size;
}

uint32_t
zix_ring_peek(ZixRing* ring, void* dst, uint32_t size)
{
    return peek_internal(ring, ring->read_head, ring->write_head, size, dst);
}

uint32_t
zix_ring_read(ZixRing* ring, void* dst, uint32_t size)
{
    const uint32_t r = ring->read_head;
    const uint32_t w = ring->write_head;

    if (peek_internal(ring, r, w, size, dst)) {
        ZIX_READ_BARRIER();
        ring->read_head = (r + size) & ring->size_mask;
        return size;
    } else {
        return 0;
    }
}

uint32_t
zix_ring_skip(ZixRing* ring, uint32_t size)
{
    const uint32_t r = ring->read_head;
    const uint32_t w = ring->write_head;
    if (read_space_internal(ring, r, w) < size) {
        return 0;
    }

    ZIX_READ_BARRIER();
    ring->read_head = (r + size) & ring->size_mask;
    return size;
}

uint32_t
zix_ring_write(ZixRing* ring, const void* src, uint32_t size)
{
    const uint32_t r = ring->read_head;
    const uint32_t w = ring->write_head;
    if (write_space_internal(ring, r, w) < size) {
        return 0;
    }

    if (w + size <= ring->size) {
        memcpy(&ring->buf[w], src, size);
        ZIX_WRITE_BARRIER();
        ring->write_head = (w + size) & ring->size_mask;
    } else {
        const uint32_t this_size = ring->size - w;
        memcpy(&ring->buf[w], src, this_size);
        memcpy(&ring->buf[0], (const char*)src + this_size, size - this_size);
        ZIX_WRITE_BARRIER();
        ring->write_head = size - this_size;
    }

    return size;
}
