/**********************************************************************

  Audacity: A Digital Audio Editor

  PerTrackEffect.h

  Dominic Mazzoni
  Vaughan Johnson

  Paul Licameli split from Effect.h

**********************************************************************/

#ifndef __AUDACITY_PER_TRACK_EFFECT__
#define __AUDACITY_PER_TRACK_EFFECT__

#include "AudioGraphSource.h" // to inherit
#include "Effect.h" // to inherit
#include "MemoryX.h"
#include <functional>

class SampleTrack;

namespace AudioGraph {

class Buffers;

//! Downstream receiver of sample streams, taking Buffers as external context
class Sink {
public:
   using Buffers = AudioGraph::Buffers;

   virtual ~Sink();

   virtual bool AcceptsBuffers(const Buffers &buffers) const = 0;

   //! Guarantee empty space in Buffers before they are written
   /*!
    @return success
    @post result: `!result || data.BlockSize() <= data.Remaining()`
    */
   virtual bool Acquire(Buffers &data) = 0;
   //! Acknowledge receipt of data in Buffers, which caller may then Advance()
   /*!
    @return success
    @pre `AcceptsBuffers(data)`
    @pre `curBlockSize <= data.BlockSize()`
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

   //! If constructed with positive length, then accepts buffers only when
   //! number of channels is positive
   bool AcceptsBuffers(const Buffers &buffers) const override;

   //! Always true
   bool AcceptsBlockSize(size_t blockSize) const override;

   std::optional<size_t> Acquire(Buffers &data, size_t bound) override;
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
   size_t mFetched{};
   bool mInitialized{ false };
};

class WaveTrackSink final : public AudioGraph::Sink {
public:
   WaveTrackSink(WaveTrack &left, WaveTrack *pRight,
      sampleCount start, bool isGenerator, bool isProcessor);
   ~WaveTrackSink() override;

   //! Accepts buffers only if there is at least one channel
   bool AcceptsBuffers(const Buffers &buffers) const override;

   bool Acquire(Buffers &data) override;
   bool Release(const Buffers &data, size_t curBlockSize) override;

   /*!
    @copydoc DoConsume
    */
   void Flush(Buffers &data, double t0, double t1);

private:
   /*!
    @pre `data.Channels() > 0`
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

   class AUDACITY_DLL_API Instance : public virtual EffectInstanceEx {
   public:
      explicit Instance(const PerTrackEffect &processor)
         : mProcessor{ processor }
      {}
      ~Instance() override;
   
      //! Uses the other virtual functions of this class
      bool Process(EffectSettings &settings) final;

      bool ProcessInitialize(EffectSettings &settings,
         double sampleRate, ChannelNames chanMap) override;

      bool ProcessFinalize() noexcept override;

      //! Default implementation returns zero
      sampleCount GetLatency(
         const EffectSettings &settings, double sampleRate) const override;

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
    Previous contents of inBuffers and outBuffers are ignored

    @pre `source.AcceptsBuffers(inBuffers)`
    @pre `source.AcceptsBlockSize(inBuffers.BlockSize())`
    @pre `sink.AcceptsBuffers(outBuffers)`
    @pre `inBuffers.BlockSize() == outBuffers.BlockSize()`
    */
   static bool ProcessTrack(Instance &instance, EffectSettings &settings,
      AudioGraph::Source &source, AudioGraph::Sink &sink,
      std::optional<sampleCount> genLength,
      double sampleRate, ChannelNames map,
      Buffers &inBuffers, Buffers &outBuffers);
};

namespace AudioGraph {

//! Decorate a source with a non-timewarping effect, which may have latency
class EffectStage final : public Source {
public:
   using Instance = EffectInstanceEx;

   //! Completes `instance.ProcessInitialize()` or throws a std::exception
   /*!
    @pre `upstream.AcceptsBlockSize(inBuffers.BlockSize())`
    @post `AcceptsBlockSize(inBuffers.BlockSize())`
    */
   EffectStage(Source &upstream, Buffers &inBuffers,
      Instance &instance, EffectSettings &settings, double sampleRate,
      std::optional<sampleCount> genLength, ChannelNames map);
   EffectStage(const EffectStage&) = delete;
   EffectStage &operator =(const EffectStage &) = delete;
   //! Finalizes the instance
   ~EffectStage() override;

   /*!
    @return true
    */
   bool AcceptsBuffers(const Buffers &buffers) const override;
   //! See postcondition of constructor
   bool AcceptsBlockSize(size_t size) const override;

   std::optional<size_t> Acquire(Buffers &data, size_t bound) override;
   sampleCount Remaining() const override;
   bool Release() override;

private:
   sampleCount DelayRemaining() const
      { return std::max<sampleCount>(0, mDelayRemaining); }

   //! Produce exactly `curBlockSize` samples in `data`
   /*!
    @pre curBlockSize <= data.BlockSize() - outBufferOffset
    @pre curBlockSize <= data.Remaining() - outBufferOffset
    @pre curBlockSize <= mInBuffers.Remaining()
    @return success
    */
   bool Process(
      const Buffers &data, size_t curBlockSize, size_t outBufferOffset) const;

   std::optional<size_t> FetchProcessAndAdvance(
      Buffers &data, size_t bound, bool doZeros, size_t outBufferOffset = 0);

   void FinishUpstream();

   Source &mUpstream;
   //! @invariant mInBuffers.BlockSize() <= mInBuffers.Remaining()
   Buffers &mInBuffers;
   Instance &mInstance;
   EffectSettings &mSettings;
   const double mSampleRate;
   const bool mIsProcessor;

   sampleCount mDelay{};
   sampleCount mDelayRemaining;
   size_t mLastProduced{};
   size_t mLastZeroes{};
   bool mLatencyDone{ false };
   bool mCleared{ false };
};

struct Task {
public:
   /*!
    @pre `source.AcceptsBlockSize(buffers.BlockSize())`
    @pre `source.AcceptsBuffers(buffers)`
    @pre `sink.AcceptsBuffers(buffers)`
    */
   Task(Source &source, Buffers &buffers, Sink &sink);
   enum class Status { More, Done, Fail };
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
   Source &mSource;
   Buffers &mBuffers;
   Sink &mSink;
};

}
#endif
