/**********************************************************************

  Audacity: A Digital Audio Editor

  VSTInstance.cpp

  Dominic Mazzoni

  Paul Licameli split from VSTEffect.cpp


*//********************************************************************/
#include "VSTInstance.h"

#if USE_VST

#include <wx/time.h>

std::unique_ptr<EffectInstance::Message> VSTInstance::MakeMessage() const
{
   // The purpose here is just to allocate vectors (chunk and paramVector)
   // with sufficient size, not to get the values too
   VSTSettings settings;
   FetchSettings(settings, /* doFetch = */ false);

   VSTMessage::ParamVector paramVector;
   paramVector.resize(mAEffect->numParams, std::nullopt);

   return std::make_unique<VSTMessage>( std::move(settings.mChunk), std::move(paramVector) );
}


std::unique_ptr<EffectInstance::Message> VSTInstance::MakeMessage(int id, double value) const
{
   return std::make_unique<VSTMessage>(id, value, mAEffect->numParams);
}


unsigned VSTInstance::GetAudioInCount() const
{
   return mAudioIns;
}

unsigned VSTInstance::GetAudioOutCount() const
{
   return mAudioOuts;
}

size_t VSTInstance::SetBlockSize(size_t maxBlockSize)
{
   // Issue 3935 for IEM plug-ins, VST 2 versions:
   // It is mysterious why this further limitation of size works to
   // prevent the crashes in destructive processing, or why this is not
   // needed for non-destructive, but here it is
   // Those plugins report many channels (like 64) but most others will not
   // be affected by these lines with the default size of 8192
   // Note it may make the Block Size option of the settings dialog misleading
   auto numChannels = std::max({ 1u, GetAudioInCount(), GetAudioOutCount() });
   maxBlockSize = std::max(size_t(1),
      std::min(maxBlockSize, size_t(0x8000u / numChannels)));

   mBlockSize = std::min( maxBlockSize, mUserBlockSize );
   return mBlockSize;
}

size_t VSTInstance::GetBlockSize() const
{
   return mBlockSize;
}

auto VSTInstance::GetLatency(
   const EffectSettings& settings, double sampleRate) const -> SampleCount
{
   if (mUseLatency)
      return mBufferDelay;
   return 0;
}

bool VSTInstance::IsReady()
{
   return mReady;
}

bool VSTInstance::ProcessInitialize(
   EffectSettings& settings, double sampleRate, ChannelNames)
{
   // Issue 3942: Copy the contents of settings first.
   // settings may refer to what is in the RealtimeEffectState, but that might
   // get reassigned by EffectSettingsAccess::Set, when the validator's
   // Automate() is called-back by the plug-in during callSetParameter.
   // So this avoids a dangling reference.
   auto copiedSettings = GetSettings(settings);
   StoreSettings(copiedSettings);

   return DoProcessInitialize(sampleRate);
}

bool VSTInstance::DoProcessInitialize(double sampleRate)
{
   // Initialize time info
   memset(&mTimeInfo, 0, sizeof(mTimeInfo));
   mTimeInfo.sampleRate = sampleRate;
   mTimeInfo.nanoSeconds = wxGetUTCTimeMillis().ToDouble();
   mTimeInfo.tempo = 120.0;
   mTimeInfo.timeSigNumerator = 4;
   mTimeInfo.timeSigDenominator = 4;
   mTimeInfo.flags = kVstTempoValid | kVstNanosValid | kVstTransportPlaying;

   // Set processing parameters...power must be off for this
   callDispatcher(effSetSampleRate, 0, 0, NULL, sampleRate);
   callDispatcher(effSetBlockSize, 0, mBlockSize, NULL, 0.0);

   // Turn on the power
   PowerOn();

   // Set the initial buffer delay
   SetBufferDelay(mAEffect->initialDelay);

   mReady = true;
   return true;
}

bool VSTInstance::ProcessFinalize() noexcept
{
   return GuardedCall<bool>([&] {
      mReady = false;

      PowerOff();

      return true;
   });

}

size_t VSTInstance::ProcessBlock(EffectSettings &,
   const float *const *inBlock, float *const *outBlock, size_t blockLen)
{
   // Only call the effect if there's something to do...some do not like zero-length block
   if (blockLen)
   {
      // Go let the plugin moleste the samples
      callProcessReplacing(inBlock, outBlock, blockLen);

      // And track the position
      mTimeInfo.samplePos += (double) blockLen;
   }

   return blockLen;
}

bool VSTInstance::RealtimeInitialize(EffectSettings &settings, double sampleRate)
{
   // Temporarily disconnect from any validator, so that setting the chunk
   // does not cause Automate() callbacks (as some effects will do) that then
   // would send slider movement messages that might destroy information in
   // the settings.
   auto vr = valueRestorer(mpOwningValidator, (VSTUIWrapper*)nullptr);
   return ProcessInitialize(settings, sampleRate, {});
}

bool VSTInstance::RealtimeAddProcessor(EffectSettings &settings,
   EffectOutputs *, unsigned numChannels, float sampleRate)
{
   if (!mRecruited)
   {
      // Assign self to the first processor
      mRecruited = true;
      return true;
   }

   auto &effect = static_cast<const PerTrackEffect &>(mProcessor);
   auto slave = std::make_unique<VSTInstance>(
      const_cast<PerTrackEffect &>(effect),
      mPath, mBlockSize, mUserBlockSize, mUseLatency);

   slave->SetBlockSize(mBlockSize);

   if (!slave->ProcessInitialize(settings, sampleRate, ChannelNames()))
      return false;

   mSlaves.emplace_back(move(slave));
   return true;
}

bool VSTInstance::RealtimeFinalize(EffectSettings&) noexcept
{
return GuardedCall<bool>([&]{

   if (mpOwningValidator)
      mpOwningValidator->Flush();

   mRecruited = false;

   for (const auto &slave : mSlaves)
      slave->ProcessFinalize();
   mSlaves.clear();

   return ProcessFinalize();
});
}

bool VSTInstance::RealtimeSuspend()
{
   PowerOff();

   for (const auto &slave : mSlaves)
      slave->PowerOff();

   return true;
}

bool VSTInstance::RealtimeResume()
{
   PowerOn();

   for (const auto &slave : mSlaves)
      slave->PowerOn();

   return true;
}

bool VSTInstance::OnePresetWasLoadedWhilePlaying()
{
   return mPresetLoadedWhilePlaying.exchange(false);
}

void VSTInstance::DeferChunkApplication()
{
   std::lock_guard<std::mutex> guard(mDeferredChunkMutex);

   if (! mChunkToSetAtIdleTime.empty() )
   {    
      ApplyChunk(mChunkToSetAtIdleTime);
      mChunkToSetAtIdleTime.resize(0);
   }
}

void VSTInstance::ApplyChunk(std::vector<char>& chunk)
{
   VstPatchChunkInfo info = {
      1, mAEffect->uniqueID, mAEffect->version, mAEffect->numParams, "" };

   const auto len = chunk.size();
   const auto data = chunk.data();

   callSetChunk(true, len, data, &info);
   for (auto& slave : mSlaves)
      slave->callSetChunk(true, len, data, &info);
}

bool VSTInstance::ChunkMustBeAppliedInMainThread() const
{
   // Some plugins (e.g. Melda) can not have their chunk set in the
   // audio thread, resulting in making the whole app hang.
   // This is why we defer the setting of the chunk in the main thread.

   const bool IsAudioThread = (mMainThreadId != std::this_thread::get_id());
   
   return IsAudioThread && mIsMeldaPlugin;
}

bool VSTInstance::UsesMessages() const noexcept
{
   return true;
}

bool VSTInstance::RealtimeProcessStart(MessagePackage& package)
{
   const bool applyChunkInMainThread = ChunkMustBeAppliedInMainThread();

   if (applyChunkInMainThread)
      mDeferredChunkMutex.lock();

   if (!package.pMessage)
      return true;

   auto& message = static_cast<VSTMessage&>(*package.pMessage);

   auto &chunk = message.mChunk;

   if (!chunk.empty())
   {
      if (applyChunkInMainThread)
      {
         // Apply the chunk later
         //
         mChunkToSetAtIdleTime = chunk;
      }
      else
      {
         // Apply the chunk now
         ApplyChunk(chunk);
      }

      // Don't apply the chunk again until another message supplies a chunk
      chunk.resize(0);

      // Don't return yet.  Maybe some slider movements also accumulated after
      // the change of the chunk.

      const bool IsAudioThread = (mMainThreadId != std::this_thread::get_id());
      if (IsAudioThread)
      {
         // At the moment, the only reason why this method would be called in the audio thread,
         // is because a preset was loaded while playing

         mPresetLoadedWhilePlaying.store(true);
      }

   }


   assert(message.mParamsVec.size() == mAEffect->numParams);

   for (size_t paramID=0; paramID < mAEffect->numParams; paramID++)
   {
      if (message.mParamsVec[paramID])
      {
         float val = (float)(*message.mParamsVec[paramID]);

         // set the change on the recruited "this" instance
         callSetParameter(paramID, val);

         // set the change on any existing slaves
         for (auto& slave : mSlaves)
         {
            slave->callSetParameter(paramID, val);
         }

         // clear the used info
         message.mParamsVec[paramID] = std::nullopt;
      }
   }

   return true;
}

size_t VSTInstance::RealtimeProcess(size_t group, EffectSettings &settings,
   const float *const *inbuf, float *const *outbuf, size_t numSamples)
{
   if (!mRecruited)
   {
      // unexpected!
      return 0;
   }

   wxASSERT(numSamples <= mBlockSize);

   if (group == 0)
   {
      // use the recruited "this" instance
      return ProcessBlock(settings, inbuf, outbuf, numSamples);
   }
   else if (group <= mSlaves.size())
   {
      // use the slave which maps to the group
      return mSlaves[group - 1]->ProcessBlock(settings, inbuf, outbuf, numSamples);
   }
   else
      return 0;
}

bool VSTInstance::RealtimeProcessEnd(EffectSettings &) noexcept
{
   if ( ChunkMustBeAppliedInMainThread() )
      mDeferredChunkMutex.unlock();

   return true;
}

void VSTInstance::NeedIdle()
{
   if (mpOwningValidator)
   {
      mpOwningValidator->NeedIdle();
   }
}

void VSTInstance::PowerOn()
{
   if (!mHasPower)
   {
      // Turn the power on
      callDispatcher(effMainsChanged, 0, 1, NULL, 0.0);

      // Tell the effect we're going to start processing
      if (mVstVersion >= 2)
      {
         callDispatcher(effStartProcess, 0, 0, NULL, 0.0);
      }

      // Set state
      mHasPower = true;
   }
}

void VSTInstance::PowerOff()
{
   if (mHasPower)
   {
      // Tell the effect we're going to stop processing
      if (mVstVersion >= 2)
      {
         callDispatcher(effStopProcess, 0, 0, NULL, 0.0);
      }

      // Turn the power off
      callDispatcher(effMainsChanged, 0, 0, NULL, 0.0);

      // Set state
      mHasPower = false;
   }
}

void VSTInstance::SizeWindow(int w, int h)
{
   if (mpOwningValidator)
   {
      mpOwningValidator->SizeWindow(w, h);
   }
}

void VSTInstance::SetBufferDelay(int samples)
{
   // We do not support negative delay
   if (samples >= 0 && mUseLatency)
   {
      mBufferDelay = samples;
   }

   return;
}

void VSTInstance::callProcessReplacing(const float *const *inputs,
   float *const *outputs, int sampleframes)
{
   mAEffect->processReplacing(mAEffect,
      const_cast<float**>(inputs),
      const_cast<float**>(outputs), sampleframes);
}

void VSTInstance::Automate(int index, float value)
{
   if (mMainThreadId != std::this_thread::get_id())
      return;

   if (mpOwningValidator)
   {
      mpOwningValidator->Automate(index, value);
   }
}

VSTInstance::VSTInstance
(
   const PerTrackEffect& effect,
   const PluginPath& path,
   size_t            blockSize,
   size_t            userBlockSize,
   bool              useLatency
)

   : PerTrackEffect::Instance(effect)
   , VSTWrapper(path)
   , mUseLatency{ useLatency }
{
   // what also happens in the effect ctor
   //
   memset(&mTimeInfo, 0, sizeof(mTimeInfo));
   mTimeInfo.samplePos = 0.0;
   mTimeInfo.sampleRate = 44100.0;  // this is a bogus value, but it's only for the display
   mTimeInfo.nanoSeconds = wxGetUTCTimeMillis().ToDouble();
   mTimeInfo.tempo = 120.0;
   mTimeInfo.timeSigNumerator = 4;
   mTimeInfo.timeSigDenominator = 4;
   mTimeInfo.flags = kVstTempoValid | kVstNanosValid;

   mBlockSize = blockSize;
   mUserBlockSize = userBlockSize;

   Load();

   if (!IsReady() )
   {
      // Set some defaults since some VSTs need them...these will be reset when
      // normal or realtime processing begins
      mBlockSize = 8192;
      DoProcessInitialize(44100.0);
   }

   mIsMeldaPlugin = (mVendor == "MeldaProduction");
}

VSTInstance::~VSTInstance()
{
   PowerOff();
}

void VSTInstance::SetOwningValidator(VSTUIWrapper* vi)
{
   mpOwningValidator = vi;
}

#endif // USE_VST
