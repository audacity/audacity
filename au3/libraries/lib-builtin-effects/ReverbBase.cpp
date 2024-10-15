#include "ReverbBase.h"
#include "ShuttleAutomation.h"

ReverbBase::ReverbBase()
{
   SetLinearEffectFlag(true);
}

ReverbBase::~ReverbBase()
{
}

const EffectParameterMethods& ReverbBase::Parameters() const
{
   static CapturedParameters<
      ReverbBase, RoomSize, PreDelay, Reverberance, HfDamping, ToneLow,
      ToneHigh, WetGain, DryGain, StereoWidth, WetOnly>
      parameters;
   return parameters;
}

static const struct
{
   const TranslatableString name;
   ReverbSettings preset;
} FactoryPresets[] = {
   //                         Room  Pre            Hf       Tone Tone  Wet   Dry
   //                         Stereo Wet
   // Name                    Size, Delay, Reverb, Damping, Low, High, Gain,
   // Gain, Width, Only
   // general purpose
   /*i18n-hint: This is the name of an effect preset */
   { XO("Acoustic"), { 50, 10, 75, 100, 21, 100, -14, 0, 80, false } },
   /*i18n-hint: This is the name of an effect preset */
   { XO("Ambience"), { 100, 55, 100, 50, 53, 38, 0, -10, 100, false } },
   /*i18n-hint: This is the name of an effect preset */
   { XO("Artificial"), { 81, 99, 23, 62, 16, 19, -4, 0, 100, false } },
   /*i18n-hint: This is the name of an effect preset */
   { XO("Clean"), { 50, 10, 75, 100, 55, 100, -18, 0, 75, false } },
   /*i18n-hint: This is the name of an effect preset */
   { XO("Modern"), { 50, 10, 75, 100, 55, 100, -15, 0, 75, false } },
   // vocals
   /*i18n-hint: This is the name of an effect preset */
   { XO("Vocal I"), { 70, 20, 40, 99, 100, 50, -12, 0, 70, false } },
   /*i18n-hint: This is the name of an effect preset */
   { XO("Vocal II"), { 50, 0, 50, 99, 50, 100, -1, -1, 70, false } },
   /*i18n-hint: This is the name of an effect preset */
   { XO("Dance Vocal"), { 90, 2, 60, 77, 30, 51, -10, 0, 100, false } },
   /*i18n-hint: This is the name of an effect preset */
   { XO("Modern Vocal"), { 66, 27, 77, 8, 0, 51, -10, 0, 68, false } },
   /*i18n-hint: This is the name of an effect preset */
   { XO("Voice Tail"), { 66, 27, 100, 8, 0, 51, -6, 0, 68, false } },
   // room sizes
   /*i18n-hint: This is the name of an effect preset */
   { XO("Bathroom"), { 16, 8, 80, 0, 0, 100, -6, 0, 100, false } },
   /*i18n-hint: This is the name of an effect preset */
   { XO("Small Room Bright"), { 30, 10, 50, 50, 50, 100, -1, -1, 100, false } },
   /*i18n-hint: This is the name of an effect preset */
   { XO("Small Room Dark"), { 30, 10, 50, 50, 100, 0, -1, -1, 100, false } },
   /*i18n-hint: This is the name of an effect preset */
   { XO("Medium Room"), { 75, 10, 40, 50, 100, 70, -1, -1, 70, false } },
   /*i18n-hint: This is the name of an effect preset */
   { XO("Large Room"), { 85, 10, 40, 50, 100, 80, 0, -6, 90, false } },
   /*i18n-hint: This is the name of an effect preset */
   { XO("Church Hall"), { 90, 32, 60, 50, 100, 50, 0, -12, 100, false } },
   /*i18n-hint: This is the name of an effect preset */
   { XO("Cathedral"), { 90, 16, 90, 50, 100, 0, 0, -20, 100, false } },
   /*i18n-hint: This is the name of an effect preset */
   { XO("Big Cave"), { 100, 55, 100, 50, 53, 38, 5, -3, 100, false } },
};

//
// ReverbBase
//

const ComponentInterfaceSymbol ReverbBase::Symbol { XO("Reverb") };

ComponentInterfaceSymbol ReverbBase::GetSymbol() const
{
   return Symbol;
}

TranslatableString ReverbBase::GetDescription() const
{
   return XO("Adds ambience or a \"hall effect\"");
}

ManualPageID ReverbBase::ManualPage() const
{
   return L"Reverb";
}

// EffectDefinitionInterface implementation

EffectType ReverbBase::GetType() const
{
   return EffectTypeProcess;
}

auto ReverbBase::RealtimeSupport() const -> RealtimeSince
{
   return RealtimeSince::After_3_1;
}

static size_t BLOCK = 16384;

ReverbBase::Instance::Instance(const PerTrackEffect& effect)
    : PerTrackEffect::Instance { effect }
{
}

bool ReverbBase::Instance::ProcessInitialize(
   EffectSettings& settings, double sampleRate, ChannelNames chanMap)
{
   // For descructive processing, fix the number of channels, maybe as 1 not 2
   auto& rs = GetSettings(settings);
   mChannels = rs.mStereoWidth ? 2 : 1;

   return InstanceInit(
      settings, sampleRate, mState, chanMap, /* forceStereo = */ false);
}

bool ReverbBase::Instance::InstanceInit(
   EffectSettings& settings, double sampleRate, ReverbState& state,
   ChannelNames chanMap, bool forceStereo)
{
   auto& rs = GetSettings(settings);

   bool isStereo = false;
   state.mNumChans = 1;
   if (
      (chanMap && chanMap[0] != ChannelNameEOL &&
       chanMap[1] == ChannelNameFrontRight) ||
      forceStereo)
   {
      isStereo = true;
      state.mNumChans = 2;
   }

   state.mP = std::make_unique<Reverb_priv_ex[]>(state.mNumChans);

   for (unsigned int i = 0; i < state.mNumChans; i++)
   {
      reverb_create(
         &state.mP[i].reverb, sampleRate, rs.mWetGain, rs.mRoomSize,
         rs.mReverberance, rs.mHfDamping, rs.mPreDelay,
         rs.mStereoWidth * (isStereo ? 1 : 0), rs.mToneLow, rs.mToneHigh, BLOCK,
         state.mP[i].wet);
   }

   return true;
}

bool ReverbBase::Instance::ProcessFinalize() noexcept
{
   return true;
}

bool ReverbBase::Instance::RealtimeInitialize(
   EffectSettings& settings, double sampleRate)
{
   SetBlockSize(512);
   mSlaves.clear();

   mLastAppliedSettings = GetSettings(settings);
   mLastSampleRate = sampleRate;

   return true;
}

bool ReverbBase::Instance::RealtimeAddProcessor(
   EffectSettings& settings, EffectOutputs*, unsigned numChannels,
   float sampleRate)
{
   ReverbBase::Instance slave(mProcessor);

   // The notion of ChannelNames is unavailable here,
   // so we'll have to force the stereo init, if this is the case
   //
   InstanceInit(
      settings, sampleRate, slave.mState, /*ChannelNames=*/nullptr,
      /*forceStereo=*/(numChannels == 2));

   mSlaves.push_back(std::move(slave));
   return true;
}

bool ReverbBase::Instance::RealtimeFinalize(EffectSettings& settings) noexcept
{
   mSlaves.clear();
   return true;
}

size_t ReverbBase::Instance::RealtimeProcess(
   size_t group, EffectSettings& settings, const float* const* inbuf,
   float* const* outbuf, size_t numSamples)
{

   const auto& incomingSettings = GetSettings(settings);
   if (!(incomingSettings == mLastAppliedSettings))
   {
      const bool onlySimpleOnes =
         OnlySimpleParametersChanged(incomingSettings, mLastAppliedSettings);

      for (auto& slave : mSlaves)
      {
         for (unsigned int i = 0; i < slave.mState.mNumChans; i++)
         {
            auto& reverbCore = slave.mState.mP[i].reverb;
            const auto& is = incomingSettings;

            if (onlySimpleOnes)
            {
               reverb_set_simple_params(
                  &reverbCore, mLastSampleRate, is.mWetGain, is.mReverberance,
                  is.mHfDamping, is.mToneLow, is.mToneHigh);
            }
            else
            {
               // One of the non-simple parameters changed, so we need to
               // do a full reinit
               reverb_init(
                  &reverbCore, mLastSampleRate, is.mWetGain, is.mRoomSize,
                  is.mReverberance, is.mHfDamping, is.mPreDelay,
                  is.mStereoWidth, is.mToneLow, is.mToneHigh);
            }
         }
      }

      mLastAppliedSettings = incomingSettings;
   }

   if (group >= mSlaves.size())
      return 0;
   return InstanceProcess(
      settings, mSlaves[group].mState, inbuf, outbuf, numSamples);
}

bool ReverbBase::Instance::RealtimeSuspend()
{
   for (auto& slave : mSlaves)
   {
      for (unsigned int i = 0; i < slave.mState.mNumChans; i++)
      {
         reverb_clear(&(slave.mState.mP[i].reverb));
      }
   }

   return true;
}

unsigned ReverbBase::Instance::GetAudioOutCount() const
{
   return mChannels;
}

unsigned ReverbBase::Instance::GetAudioInCount() const
{
   return mChannels;
}

size_t ReverbBase::Instance::ProcessBlock(
   EffectSettings& settings, const float* const* inBlock,
   float* const* outBlock, size_t blockLen)
{
   return InstanceProcess(settings, mState, inBlock, outBlock, blockLen);
}

size_t ReverbBase::Instance::InstanceProcess(
   EffectSettings& settings, ReverbState& state, const float* const* inBlock,
   float* const* outBlock, size_t blockLen)
{
   auto& rs = GetSettings(settings);

   const float* ichans[2] = { NULL, NULL };
   float* ochans[2] = { NULL, NULL };

   for (unsigned int c = 0; c < state.mNumChans; c++)
   {
      ichans[c] = inBlock[c];
      ochans[c] = outBlock[c];
   }

   float const dryMult = rs.mWetOnly ? 0 : dB_to_linear(rs.mDryGain);

   auto remaining = blockLen;

   while (remaining)
   {
      auto len = std::min(remaining, decltype(remaining)(BLOCK));
      for (unsigned int c = 0; c < state.mNumChans; c++)
      {
         // Write the input samples to the reverb fifo.  Returned value is the
         // address of the fifo buffer which contains a copy of the input
         // samples.
         state.mP[c].dry =
            (float*)fifo_write(&state.mP[c].reverb.input_fifo, len, ichans[c]);
         reverb_process(&state.mP[c].reverb, len);
      }

      if (state.mNumChans == 2)
      {
         for (decltype(len) i = 0; i < len; i++)
         {
            for (int w = 0; w < 2; w++)
            {
               ochans[w][i] =
                  dryMult * state.mP[w].dry[i] +
                  0.5 * (state.mP[0].wet[w][i] + state.mP[1].wet[w][i]);
            }
         }
      }
      else
      {
         for (decltype(len) i = 0; i < len; i++)
         {
            ochans[0][i] = dryMult * state.mP[0].dry[i] + state.mP[0].wet[0][i];
         }
      }

      remaining -= len;

      for (unsigned int c = 0; c < state.mNumChans; c++)
      {
         ichans[c] += len;
         ochans[c] += len;
      }
   }

   return blockLen;
}

RegistryPaths ReverbBase::GetFactoryPresets() const
{
   RegistryPaths names;

   for (size_t i = 0; i < WXSIZEOF(FactoryPresets); i++)
   {
      names.push_back(FactoryPresets[i].name.Translation());
   }

   return names;
}

OptionalMessage
ReverbBase::LoadFactoryPreset(int id, EffectSettings& settings) const
{
   if (id < 0 || id >= (int)WXSIZEOF(FactoryPresets))
   {
      return {};
   }

   ReverbBase::GetSettings(settings) = FactoryPresets[id].preset;

   return { nullptr };
}

bool operator==(const ReverbSettings& a, const ReverbSettings& b)
{
   // With C++20, all of this can be replaced by =default
   return (a.mRoomSize == b.mRoomSize) && (a.mPreDelay == b.mPreDelay) &&
          (a.mReverberance == b.mReverberance) &&
          (a.mHfDamping == b.mHfDamping) && (a.mToneLow == b.mToneLow) &&
          (a.mToneHigh == b.mToneHigh) && (a.mWetGain == b.mWetGain) &&
          (a.mDryGain == b.mDryGain) && (a.mStereoWidth == b.mStereoWidth) &&
          (a.mWetOnly == b.mWetOnly);
}

bool OnlySimpleParametersChanged(
   const ReverbSettings& a, const ReverbSettings& b)
{
   // A "simple" reverb parameter is one that when changed, does not require the
   // reverb allpass/comb filters to be reset. This distinction enables us to
   // code things so that the user can keep hearing the processed sound while
   // they tweak one of the simple parameters.

   const bool oneSimpleParameterChanged =

      (a.mReverberance != b.mReverberance) || (a.mHfDamping != b.mHfDamping) ||
      (a.mToneLow != b.mToneLow) || (a.mToneHigh != b.mToneHigh) ||
      (a.mWetGain != b.mWetGain);

   const bool allNonSimpleParametersStayedTheSame =

      (a.mRoomSize == b.mRoomSize) && (a.mPreDelay == b.mPreDelay) &&
      (a.mStereoWidth == b.mStereoWidth);

   return oneSimpleParameterChanged && allNonSimpleParametersStayedTheSame;
}
