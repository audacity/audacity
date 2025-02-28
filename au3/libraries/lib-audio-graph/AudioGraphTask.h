/**********************************************************************

  Audacity: A Digital Audio Editor

  @file AudioGraphTask.h

  Dominic Mazzoni
  Vaughan Johnson

  Paul Licameli split from PerTrackEffect.h

**********************************************************************/

#ifndef __AUDACITY_AUDIO_GRAPH_TASK__
#define __AUDACITY_AUDIO_GRAPH_TASK__

namespace AudioGraph {
class Buffers;
class Sink;
class Source;

//! Copies from a Source to a Sink, mediated by Buffers
struct AUDIO_GRAPH_API Task {
public:
    /*!
     @pre `source.AcceptsBlockSize(buffers.BlockSize())`
     @pre `source.AcceptsBuffers(buffers)`
     @pre `sink.AcceptsBuffers(buffers)`
     */
    Task(Source& source, Buffers& buffers, Sink& sink);
    enum class Status {
        More, Done, Fail
    };
    //! Do an increment of the copy
    Status RunOnce();
    //! Do the complete copy
    /*!
     @return success
     @pre `mBuffers.Remaining() >= mBuffers.BlockSize()`
     @post result:  `result == Status::Fail ||
        mBuffers.Remaining() >= mBuffers.BlockSize()`
     */
    bool RunLoop();
private:
    Source& mSource;
    Buffers& mBuffers;
    Sink& mSink;

#ifndef NDEBUG
    bool mRanOnce{ false };
#endif
};
}
#endif
