/**********************************************************************

  Audacity: A Digital Audio Editor

  @file AudioGraphBuffers.h
  @brief Buffers that mediate data transfer between a Source and a Sink

  Paul Licameli split from PerTrackEffect.h

**********************************************************************/

#ifndef __AUDACITY_AUDIO_GRAPH_BUFFERS__
#define __AUDACITY_AUDIO_GRAPH_BUFFERS__

#include "SampleFormat.h"
#include <vector>

namespace AudioGraph {
//! Accumulates (non-interleaved) data during effect processing
/*!
 @invariant `BlockSize() > 0`
 @invariant `BufferSize() > 0`
 @invariant `BufferSize() % BlockSize() == 0`

 @invariant `mBuffers.size() == mPositions.size()`
 @invariant all `mBuffers[i].size()` are equal to `BufferSize()`
 @invariant all `(mPositions[i] - mBuffers[i].data())` are equal and in
    range [`0`, `BufferSize()`]
 */
class AUDIO_GRAPH_API Buffers
{
public:
    /*!
     @pre `blockSize > 0`
     @post `BlockSize() == blockSize`
     @post `BufferSize() == blockSize`
     @post `IsRewound()`
     */
    explicit Buffers(size_t blockSize = 512);
    /*!
     @param padding extra allocation can work around a soxr library bug

     @pre `blockSize > 0`
     @pre `nBlocks > 0`
     @post `Channels() == nChannels`
     @post `BlockSize() == blockSize`
     @post `BufferSize() == blockSize * nBlocks`
     */
    Buffers(unsigned nChannels, size_t blockSize, size_t nBlocks, size_t padding = 0);
    unsigned Channels() const { return mBuffers.size(); }
    size_t BufferSize() const { return mBufferSize; }
    size_t BlockSize() const { return mBlockSize; }
    size_t Position() const
    {
        return mBuffers.empty() ? 0
               : Positions()[0]
               - reinterpret_cast<const float*>(GetReadPosition(0));
    }

    size_t Remaining() const { return BufferSize() - Position(); }
    bool IsRewound() const { return BufferSize() == Remaining(); }
    /*!
     @param padding extra allocation can work around a soxr library bug

     @pre `blockSize > 0`
     @pre `nBlocks > 0`
     @post `Channels() == nChannels`
     @post `BlockSize() == blockSize`
     @post `BufferSize() == blockSize * nBlocks`
     */
    void Reinit(unsigned nChannels, size_t blockSize, size_t nBlocks, size_t padding = 0);
    //! Get array of positions in the buffers
    float* const* Positions() const { return mPositions.data(); }
    //! Discard some data at the (unchanging) positions
    /*!
     @param drop how many values to discard
     @param keep how many following values are defined
     @pre drop + keep <= Remaining()
     @post `Remaining()` is unchanged
     */
    void Discard(size_t drop, size_t keep);
    //! Move the positions
    /*!
     @pre count <= Remaining()
     @post `Remaining()` reduced by `count`
     */
    void Advance(size_t count);
    //! Reset positions to starts of buffers
    /*!
     @post `IsRewound()`
     */
    void Rewind();
    //! Shift all data at and after the old position to position 0
    /*!
     @return how many positions were shifted; other contents are unspecified
     @post `IsRewound()`
     */
    size_t Rotate();

    //! Get accumulated data for one channel
    /*!
     Last channel is replicated for all greater indices
     @pre `Channels() > 0`
     @pre `BufferSize() > 0`
     @post result: `result != nullptr`
     */
    constSamplePtr GetReadPosition(unsigned iChannel) const;

    //! Get writable position for one channel
    /*!
     @pre `iChannel < Channels()`
     @pre `BufferSize() > 0`
     */
    float& GetWritePosition(unsigned iChannel);

    //! Zero-fill n places in one of the buffers,
    //! starting from its position
    void ClearBuffer(unsigned iChannel, size_t n);
private:
    std::vector<std::vector<float> > mBuffers;
    std::vector<float*> mPositions;
    size_t mBufferSize{ 0 };
    size_t mBlockSize{ 0 };
};
}
#endif
