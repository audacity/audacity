/**********************************************************************

  Audacity: A Digital Audio Editor

  VSTInstance.h

  Dominic Mazzoni

  Paul Licameli split from VSTEffect.h

**********************************************************************/
#if USE_VST

#ifndef __AUDACITY_VST_INSTANCE__
#define __AUDACITY_VST_INSTANCE__

#include "PerTrackEffect.h"
#include "VSTWrapper.h"

#include <atomic>
#include <vector>

class VSTInstance;
using VSTInstanceArray = std::vector < std::unique_ptr<VSTInstance> >;

class VST_API VSTInstance final : public PerTrackEffect::Instance,
   public VSTWrapper
{
public:

   VSTInstance(const PerTrackEffect&   effect,
                     const PluginPath& path,
                     size_t            blockSize,
                     size_t            userBlockSize,
                     bool              useLatency
                    );

   ~VSTInstance() override;


   bool ProcessInitialize(EffectSettings& settings, double sampleRate,
                          ChannelNames chanMap) override;
   
   bool ProcessFinalize() noexcept override;

   size_t SetBlockSize(size_t maxBlockSize) override;
   size_t GetBlockSize() const override;

   bool RealtimeInitialize(EffectSettings& settings, double sampleRate)
      override;
   bool RealtimeAddProcessor(EffectSettings& settings, EffectOutputs *pOutputs,
      unsigned numChannels, float sampleRate) override;
   bool RealtimeFinalize(EffectSettings& settings) noexcept override;
   bool RealtimeSuspend() override;
   bool RealtimeResume() override;
   bool UsesMessages() const noexcept override;
   bool RealtimeProcessStart(MessagePackage& package) override;
   size_t RealtimeProcess(size_t group, EffectSettings& settings,
      const float* const* inbuf, float* const* outbuf, size_t numSamples)
      override;
   bool RealtimeProcessEnd(EffectSettings& settings) noexcept override;




   size_t ProcessBlock(EffectSettings& settings,
      const float* const* inBlock, float* const* outBlock, size_t blockLen) override;

   SampleCount GetLatency(const EffectSettings& settings, double sampleRate)
      const override;

   bool IsReady();

   unsigned GetAudioInCount() const override;

   unsigned GetAudioOutCount() const override;

   bool DoProcessInitialize(double sampleRate);

   void PowerOn();
   void PowerOff();

   const bool mUseLatency;

   size_t mBlockSize{ 8192 };

   std::unique_ptr<Message> MakeMessage() const override;

   std::unique_ptr<Message> MakeMessage(int id, double value) const;

   // VSTUIWrapper overrides

   void Automate(int index, float value) override;
   void NeedIdle()                       override;
   void SizeWindow(int w, int h)         override;
   void SetBufferDelay(int samples)      override;

   // The overrides above will forward calls to them to the corresponding
   // overrides in the Validator which owns the instance - this sets it.
   void SetOwningValidator(VSTUIWrapper* vi);

   bool OnePresetWasLoadedWhilePlaying();

   void DeferChunkApplication();

   bool HasGUI() const { return mGui; }

private:

   void callProcessReplacing(
      const float* const* inputs, float* const* outputs, int sampleframes);

   VSTInstanceArray mSlaves;

   bool mHasPower{ false };

   size_t mUserBlockSize{ mBlockSize };

   bool mReady{ false };

   bool mRecruited{ false };

   VSTUIWrapper* mpOwningValidator{};

   std::atomic_bool mPresetLoadedWhilePlaying{ false };

   std::mutex mDeferredChunkMutex;
   std::vector<char> mChunkToSetAtIdleTime{};

   void ApplyChunk(std::vector<char>& chunk);

   bool ChunkMustBeAppliedInMainThread() const;

   bool mIsMeldaPlugin{ false };
};

#endif

#endif // USE_VST
