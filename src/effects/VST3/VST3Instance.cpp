#include "VST3Instance.h"

#include <public.sdk/source/vst/utility/uid.h>

#include "AudacityException.h"
#include "VST3Wrapper.h"
#include "ConfigInterface.h"
#include "VST3Utils.h"

namespace
{
   unsigned CountChannels(Steinberg::Vst::IComponent* component,
   const Steinberg::Vst::MediaTypes mediaType, 
   const Steinberg::Vst::BusDirection busDirection,
   const Steinberg::Vst::BusType busType)
   {
      using namespace Steinberg;

      unsigned channelsCount{0};

      const auto busCount = component->getBusCount(mediaType, busDirection);
      for(auto i = 0; i < busCount; ++i)
      {
         Vst::BusInfo busInfo;
         if(component->getBusInfo(mediaType, busDirection, i, busInfo) == kResultOk)
         {
            if(busInfo.busType == busType)
               channelsCount += busInfo.channelCount;
         }
      }
      return channelsCount;
   }
}

VST3Instance::VST3Instance(const PerTrackEffect& effect, VST3::Hosting::Module& module, VST3::UID effectUID)
   : Instance(effect), mEffectUID(effectUID)
{
   ReloadUserOptions();
   mWrapper = std::make_unique<VST3Wrapper>(module, mEffectUID);
}

VST3Instance::~VST3Instance() = default;

size_t VST3Instance::GetTailSize() const
{
   return Instance::GetTailSize();
}

bool VST3Instance::Init()
{
   return Instance::Init();
}

bool VST3Instance::RealtimeAddProcessor(EffectSettings& settings, unsigned, float sampleRate)
{
   return true;
}

bool VST3Instance::RealtimeFinalize(EffectSettings& settings) noexcept
{
   mWrapper->Finalize();
   return true;
}

bool VST3Instance::RealtimeInitialize(EffectSettings& settings, double sampleRate)
{
   if(mWrapper->Initialize(settings, sampleRate, Steinberg::Vst::kRealtime, mProcessingBlockSize))
   {
      mInitialDelay = mWrapper->GetLatencySamples();
      return true;
   }
   return false;
}

size_t VST3Instance::RealtimeProcess(size_t group, EffectSettings& settings, const float* const* inBuf,
   float* const* outBuf, size_t numSamples)
{
   if(group == 0)
      return mWrapper->Process(inBuf, outBuf, numSamples);
   return 0;
}

bool VST3Instance::RealtimeProcessEnd(EffectSettings& settings) noexcept
{
   return true;
}

bool VST3Instance::RealtimeProcessStart(EffectSettings& settings)
{
   mWrapper->ConsumeChanges(settings);
   return true;
}

bool VST3Instance::RealtimeResume()
{
   mWrapper->ResumeProcessing();
   return true;
}

bool VST3Instance::RealtimeSuspend()
{
   mWrapper->SuspendProcessing();
   return true;
}

sampleCount VST3Instance::GetLatency(const EffectSettings& settings, double sampleRate) const
{
   if(mUseLatency)
      return mInitialDelay;
   return { 0u };
}

bool VST3Instance::ProcessFinalize() noexcept
{
   return GuardedCall<bool>([&]
   {
      mWrapper->Finalize();
      return true;
   });
}

bool VST3Instance::ProcessInitialize(EffectSettings &settings, double sampleRate, ChannelNames chanMap)
{
   if(mWrapper->Initialize(settings, sampleRate, Steinberg::Vst::kRealtime, mProcessingBlockSize))
   {
      mInitialDelay = mWrapper->GetLatencySamples();
      return true;
   }
   return false;;
}

size_t VST3Instance::GetBlockSize() const
{
   return mProcessingBlockSize;
}

size_t VST3Instance::SetBlockSize(size_t maxBlockSize)
{
   mProcessingBlockSize = 
      static_cast<Steinberg::int32>(std::min(maxBlockSize, mUserBlockSize));
   return mProcessingBlockSize;
}

size_t VST3Instance::ProcessBlock(EffectSettings& settings, const float* const* inBlock, float* const* outBlock,
   size_t blockLen)
{
   return mWrapper->Process(inBlock, outBlock, blockLen);
}

VST3Wrapper& VST3Instance::GetWrapper()
{
   return *mWrapper;
}

unsigned VST3Instance::GetAudioInCount() const
{
   //setupProcessing should be called first
   return CountChannels(
      mWrapper->mEffectComponent,
      Steinberg::Vst::kAudio,
      Steinberg::Vst::kInput,
      Steinberg::Vst::kMain
   );
}

unsigned VST3Instance::GetAudioOutCount() const
{
   //setupProcessing should be called first
   return CountChannels(
      mWrapper->mEffectComponent,
      Steinberg::Vst::kAudio,
      Steinberg::Vst::kOutput,
      Steinberg::Vst::kMain
   );
}

void VST3Instance::ReloadUserOptions()
{
   // Reinitialize configuration settings
   int userBlockSize;
   GetConfig(mProcessor, PluginSettings::Shared, wxT("Options"),
      wxT("BufferSize"), userBlockSize, 8192);
   mUserBlockSize = std::max( 1, userBlockSize );
   GetConfig(mProcessor, PluginSettings::Shared, wxT("Options"),
      wxT("UseLatency"), mUseLatency, true);

   SetBlockSize(mUserBlockSize);
}

void VST3Instance::AssignSettings(EffectSettings& dst, EffectSettings&& src) const
{
   mWrapper->AssignSettings(dst, std::move(src));
}

