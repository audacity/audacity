/**********************************************************************

  Audacity: A Digital Audio Editor

  PerTrackEffect.h

  Dominic Mazzoni
  Vaughan Johnson

  Paul Licameli split from Effect.h

**********************************************************************/

#ifndef __AUDACITY_PER_TRACK_EFFECT__
#define __AUDACITY_PER_TRACK_EFFECT__

#include "Effect.h" // to inherit
#include "MemoryX.h"
#include <functional>
#include <optional>

#include "SampleFormat.h"

class SampleTrack;

namespace AudioGraph {
class Buffers;

//! Upstream producer of sample streams, taking Buffers as external context
class Source {
public:
   using Buffers = AudioGraph::Buffers;

   virtual ~Source();

   //! Occupy vacant space in Buffers with some data
   /*!
    May exceeed a single block of production

    @return number of positions available to read from `data`
    @pre `Remaining() == 0 || data.Channels() > 0`
    @pre `Remaining() == 0 || data.BufferSize() > 0`
       (therefore its block size is positive too)
    @post result: `result <= data.BlockSize()`
    @post result: `result <= data.Remaining()`
    @post result: `result <= Remaining()`
    @post `data.Remaining() > 0`
    @post result: `Remaining() == 0 || result > 0` (progress guarantee)
    @post `Remaining()` is unchanged
    */
   virtual size_t Acquire(Buffers &data) = 0;

   //! Result includes any amount Acquired and not yet Released
   /*!
    @post result: `result >= 0`
    */
   virtual sampleCount Remaining() const = 0;

   //! Caller is done examining last Acquire()d positions
   /*!
    @return success
    @post `Remaining()` reduced by what was last returned by `Acquire()`
    */
   virtual bool Release() = 0;
};

//! Downstream receiver of sample streams, taking Buffers as external context
class Sink {
public:
   using Buffers = AudioGraph::Buffers;

   virtual ~Sink();
   //! Guarantee empty space in Buffers before they are written
   /*!
    @return success
    @post result: `!result || data.BlockSize() <= data.Remaining()`
    */
   virtual bool Acquire(Buffers &data) = 0;
   //! Acknowledge receipt of data in Buffers, which caller may then Advance()
   /*!
    @return success
    @pre `curBlockSize <= data.BlockSize()`
    @pre `data.Channels() > 0`
    @pre `data.BufferSize() > 0`
    */
   virtual bool Release(const Buffers &data, size_t curBlockSize) = 0;
};

}

class SampleTrackSource final : public AudioGraph::Source {
public:
   //! Type of function returning false if user cancels progress
   using Poller = std::function<bool(sampleCount blockSize)>;

   /*!
    @post `Remaining()` == len
    */
   SampleTrackSource(const SampleTrack &left, const SampleTrack *pRight,
      sampleCount start, sampleCount len, Poller pollUser);
   ~SampleTrackSource() override;

   size_t Acquire(Buffers &data) override;
   sampleCount Remaining() const override;
   //! Can test for user cancellation
   bool Release() override;
private:
   const SampleTrack &mLeft;
   const SampleTrack *const mpRight;
   const Poller mPollUser;

   sampleCount mPos{};
   sampleCount mOutputRemaining{};
   size_t mLastProduced{};
   bool mInitialized{ false };
};

class WaveTrackSink final : public AudioGraph::Sink {
public:
   WaveTrackSink(WaveTrack &left, WaveTrack *pRight,
      sampleCount start, bool isGenerator, bool isProcessor);
   ~WaveTrackSink() override;

   bool Acquire(Buffers &data) override;
   bool Release(const Buffers &data, size_t curBlockSize) override;

   /*!
    @copydoc DoConsume
    */
   void Flush(Buffers &data, double t0, double t1);

private:
   /*!
    @pre `data.Channels() > 0`
    @pre `data.BufferSize() > 0`
    @post `data.BlockSize() <= data.Remaining()`
    */
   void DoConsume(Buffers &data);

   WaveTrack &mLeft;
   WaveTrack *const mpRight;
   const std::shared_ptr<WaveTrack> mGenLeft, mGenRight;
   const bool mIsProcessor;

   sampleCount mOutPos;
};

//! Base class for Effects that treat each (mono or stereo) track independently
//! of other tracks.
/*!
   Its override of Effect::Process() uses ProcessInitialize(),
   ProcessBlock(), and ProcessFinalize() methods of its instance made by
   MakeInstance(), which must be a subclass of PerTrackEffect::Instance.
   Also uses GetLatency() to determine how many leading output samples to
   discard and how many extra samples to produce.
 */
class PerTrackEffect
   : public Effect
{
public:
   ~PerTrackEffect() override;

   class AUDACITY_DLL_API Instance : public virtual EffectInstance {
   public:
      explicit Instance(const PerTrackEffect &processor)
         : mProcessor{ processor }
      {}
      ~Instance() override;
   
      //! Uses the other virtual functions of this class
      bool Process(EffectSettings &settings) final;

      //! Called at start of destructive processing, for each (mono/stereo) track
      //! Default implementation does nothing, returns true
      virtual bool ProcessInitialize(EffectSettings &settings,
         double sampleRate, ChannelNames chanMap);

      //! Called at end of destructive processing, for each (mono/stereo) track
      //! Default implementation does nothing, returns true
      //! This may be called during stack unwinding:
      virtual bool ProcessFinalize() /* noexcept */ ;

      //! Called for destructive effect computation
      virtual size_t ProcessBlock(EffectSettings &settings,
         const float *const *inBlock, float *const *outBlock, size_t blockLen)
      = 0;

      //! Called for destructive, non-realtime effect computation
      //! Default implementation returns zero
      virtual sampleCount GetLatency(
         const EffectSettings &settings, double sampleRate) const;

   protected:
      const PerTrackEffect &mProcessor;
   };

protected:
   // These were overridables but the generality wasn't used yet
   /* virtual */ bool DoPass1() const;
   /* virtual */ bool DoPass2() const;

   // non-virtual
   bool Process(EffectInstance &instance, EffectSettings &settings) const;

   sampleCount    mSampleCnt{};

private:
   using Buffers = AudioGraph::Buffers;

   bool ProcessPass(Instance &instance, EffectSettings &settings);
   //! Type of function returning false if user cancels progress
   using Poller = std::function<bool(sampleCount blockSize)>;
   /*!
    @pre `inBuffers.BlockSize() > 0`
    @pre `len == 0 || inBuffers.Channels() > 0`
    @pre `len == 0 || inBuffers.BufferSize() > 0`
    @pre `outBuffers.Channels() > 0`
    @pre `outBuffers.BufferSize() > 0`
    @pre `outBuffers.IsRewound()`
    @pre `inBuffers.BlockSize() == outBuffers.BlockSize()`
    */
   bool ProcessTrack(Instance &instance, EffectSettings &settings,
      const Poller &pollUser, std::optional<sampleCount> genLength,
      double sampleRate, ChannelNames map,
      WaveTrack &left, WaveTrack *pRight,
      sampleCount start, sampleCount len,
      Buffers &inBuffers, Buffers &outBuffers, unsigned mNumChannels) const;
};

//! Accumulates (non-interleaved) data during effect processing
/*!
 @invariant `mBuffers.size() == mPositions.size()`
 @invariant all `mBuffers[i].size()` are equal to `BufferSize()`
 @invariant all `(mPositions[i] - mBuffers[i].data())` are equal and in
    range [`0`, `BufferSize()`]
 @invariant `BlockSize() == 0 || BufferSize() % BlockSize() == 0`
 */
class AudioGraph::Buffers {
public:
   /*!
    @post `IsRewound()`
    */
   Buffers();
   unsigned Channels() const { return mBuffers.size(); }
   size_t BufferSize() const { return mBufferSize; }
   size_t BlockSize() const { return mBlockSize; }
   size_t Position() const {
      return mBuffers.empty() ? 0
         : Positions()[0]
            - reinterpret_cast<const float*>(GetReadPosition(0));
   }
   size_t Remaining() const { return BufferSize() - Position(); }
   bool IsRewound() const { return BufferSize() == Remaining(); }
   /*!
    @post `Channels() == nChannels`
    @post `BlockSize() == blockSize`
    @post `BufferSize() == blockSize * nBlocks`
    */
   void Reinit(unsigned nChannels, size_t blockSize, size_t nBlocks);
   //! Get array of positions in the buffers
   float *const *Positions() const { return mPositions.data(); }
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
   float &GetWritePosition(unsigned iChannel);

   //! Zero-fill n places in one of the buffers,
   //! starting from its position
   void ClearBuffer(unsigned iChannel, size_t n);
private:
   std::vector<std::vector<float>> mBuffers;
   std::vector<float *> mPositions;
   size_t mBufferSize{ 0 };
   size_t mBlockSize{ 0 };
};
#endif
