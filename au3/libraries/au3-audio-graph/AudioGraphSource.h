/**********************************************************************

  Audacity: A Digital Audio Editor

  @file AudioGraphSource.h
  @brief abstract producer of sample streams, taking Buffers as external context

  Dominic Mazzoni
  Vaughan Johnson

  Paul Licameli split from PerTrackEffect.h

**********************************************************************/

#ifndef __AUDACITY_AUDIO_GRAPH_SOURCE__
#define __AUDACITY_AUDIO_GRAPH_SOURCE__

#include <optional>

class sampleCount;

namespace AudioGraph {
class Buffers;

//! Upstream producer of sample streams, taking Buffers as external context
class AUDIO_GRAPH_API Source
{
public:
    using Buffers = AudioGraph::Buffers;

    virtual ~Source();

    virtual bool AcceptsBuffers(const Buffers& buffers) const = 0;
    virtual bool AcceptsBlockSize(size_t blockSize) const = 0;

    //! Occupy vacant space in Buffers with some data
    /*!
     May exceeed a single block of production
     Can assume same buffer is passed each time, while the caller advances it
     over the previous production, or discards it, or rotates the buffer.
     May rewind or rotate the buffer.

     @return number of positions available to read from `data` or nullopt to fail
     @pre `AcceptsBuffers(data)`
     @pre `AcceptsBlockSize(data.BlockSize())`
     @pre `bound <= data.BlockSize()`
     @pre `data.BlockSize() <= data.Remaining()`
     @post result: `!result || *result <= bound`
     @post result: `!result || *result <= data.Remaining()`
     @post result: `!result || *result <= Remaining()`
     @post `data.Remaining() > 0`
     @post result: `!result || bound == 0 || Remaining() == 0 || *result > 0`
        (progress guarantee)
     @post `!Terminates()` or
        `Remaining()` was not previously defined, or is unchanged
     */
    virtual std::optional<size_t> Acquire(Buffers& data, size_t bound) = 0;

    //! Result includes any amount Acquired and not yet Released
    /*!
     May be undefined before the first successful call to Acquire()
     @post result: `result >= 0`
     */
    virtual sampleCount Remaining() const = 0;

    //! Caller is done examining last Acquire()d positions
    /*!
     May be called only after at least one successful call to Acquire()
     @return success
     @post `!Terminates()` or
        `Remaining()` reduced by what was last returned by `Acquire()`
     */
    virtual bool Release() = 0;

    //! Needed only to make some postconditions assertable; defaults true
    virtual bool Terminates() const;
};
}
#endif
