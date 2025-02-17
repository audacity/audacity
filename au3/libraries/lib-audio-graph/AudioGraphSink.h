/**********************************************************************

  Audacity: A Digital Audio Editor

  @file AudioGraphSink.h
  @brief abstract consumer of sample streams, taking Buffers as external context

  Dominic Mazzoni
  Vaughan Johnson

  Paul Licameli split from PerTrackEffect.h

**********************************************************************/
#ifndef __AUDACITY_AUDIO_GRAPH_SINK__
#define __AUDACITY_AUDIO_GRAPH_SINK__

#include <cstddef>

namespace AudioGraph {
class Buffers;

//! Downstream receiver of sample streams, taking Buffers as external context
class AUDIO_GRAPH_API Sink
{
public:
    using Buffers = AudioGraph::Buffers;

    virtual ~Sink();

    virtual bool AcceptsBuffers(const Buffers& buffers) const = 0;

    //! Guarantee empty space in Buffers before they are written
    /*!
     @return success
     @post result: `!result || data.BlockSize() <= data.Remaining()`
     */
    virtual bool Acquire(Buffers& data) = 0;
    //! Acknowledge receipt of data in Buffers, which caller may then Advance()
    /*!
     @return success
     @pre `AcceptsBuffers(data)`
     @pre `curBlockSize <= data.BlockSize()`
     */
    virtual bool Release(const Buffers& data, size_t curBlockSize) = 0;
};
}
#endif
