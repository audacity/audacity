/**********************************************************************

  Audacity: A Digital Audio Editor

  @file WaveTrackSink.h
  @brief Adapter of WaveTrack to the interface AudioGraph::Sink

  Dominic Mazzoni
  Vaughan Johnson

  Paul Licameli split from PerTrackEffect.h

**********************************************************************/

#ifndef __AUDACITY_WAVE_TRACK_SINK__
#define __AUDACITY_WAVE_TRACK_SINK__

#include "AudioGraphSink.h" // to inherit
#include "SampleCount.h"
#include "SampleFormat.h"
#include <memory>

class WaveChannel;
class WaveTrack;
class TrackList;

class WAVE_TRACK_API WaveTrackSink final : public AudioGraph::Sink
{
public:
    WaveTrackSink(WaveChannel& left, WaveChannel* pRight, WaveTrack* pGenerated, sampleCount start, bool isProcessor,
                  //! This argument affects processors only, not generators
                  sampleFormat effectiveFormat);
    ~WaveTrackSink() override;

    //! Accepts buffers only if there is at least one channel
    bool AcceptsBuffers(const Buffers& buffers) const override;

    bool Acquire(Buffers& data) override;
    bool Release(const Buffers& data, size_t curBlockSize) override;

    /*!
     @copydoc DoConsume
     */
    void Flush(Buffers& data);

    //! Whether any errors have occurred in writing data
    bool IsOk() const { return mOk; }

private:
    /*!
     @pre `data.Channels() > 0`
     @post `data.BlockSize() <= data.Remaining()`
     */
    void DoConsume(Buffers& data);

    WaveChannel& mLeft;
    WaveChannel* const mpRight;
    WaveTrack* const mpGenerated;
    WaveChannel* const mGenLeft;
    WaveChannel* const mGenRight;
    const bool mIsProcessor;
    const sampleFormat mEffectiveFormat;

    sampleCount mOutPos;
    bool mOk{ true };
};
#endif
