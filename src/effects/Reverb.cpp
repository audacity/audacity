/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2013 Audacity Team.
   License: GPL v2 or later.  See License.txt.

   Reverb.cpp
   Rob Sykes, Vaughan Johnson

******************************************************************//**

\class EffectReverb
\brief A reverberation effect

*//*******************************************************************/
#include "Reverb.h"
#include "EffectEditor.h"
#include "LoadEffects.h"

#include <wx/arrstr.h>
#include <wx/checkbox.h>
#include <wx/slider.h>
#include <wx/spinctrl.h>

#include "Prefs.h"
#include "ShuttleGui.h"
#include "valnum.h"

#include "Reverb_libSoX.h"


const EffectParameterMethods& EffectReverb::Parameters() const
{
   static CapturedParameters<EffectReverb,
      RoomSize, PreDelay, Reverberance, HfDamping, ToneLow, ToneHigh,
      WetGain, DryGain, StereoWidth, WetOnly
   > parameters;
   return parameters;
}

static const struct
{
   const TranslatableString name;
   EffectReverbSettings preset;
}
FactoryPresets[] =
{
   //                         Room  Pre            Hf       Tone Tone  Wet   Dry   Stereo Wet
   // Name                    Size, Delay, Reverb, Damping, Low, High, Gain, Gain, Width, Only
   { XO("Vocal I" ),          { 70,   20,    40,     99,      100, 50,   -12,  0,    70,    false } },
   { XO("Vocal II"),          { 50,   0,     50,     99,      50,  100,  -1,   -1,   70,    false } },
   { XO("Bathroom"),          { 16,   8,     80,     0,       0,   100,  -6,   0,    100,   false } },
   { XO("Small Room Bright"), { 30,   10,    50,     50,      50,  100,  -1,   -1,   100,   false } },
   { XO("Small Room Dark"),   { 30,   10,    50,     50,      100, 0,    -1,   -1,   100,   false } },
   { XO("Medium Room"),       { 75,   10,    40,     50,      100, 70,   -1,   -1,   70,    false } },
   { XO("Large Room"),        { 85,   10,    40,     50,      100, 80,    0,   -6,   90,    false } },
   { XO("Church Hall"),       { 90,   32,    60,     50,      100, 50,    0,   -12,  100,   false } },
   { XO("Cathedral"),         { 90,   16,    90,     50,      100, 0,     0,   -20,  100,   false } },
};

struct Reverb_priv_t
{
   reverb_t reverb;
   float *dry;
   float *wet[2];
};

struct Reverb_priv_ex : Reverb_priv_t
{
   Reverb_priv_ex() : Reverb_priv_t{} {}
   ~Reverb_priv_ex()
   {
      reverb_delete(&reverb);
   }
};

struct EffectReverbState
{
   unsigned                          mNumChans{};
   std::unique_ptr<Reverb_priv_ex[]> mP{};
};


//
// EffectReverb
//

const ComponentInterfaceSymbol EffectReverb::Symbol
{ XO("Reverb") };

namespace{ BuiltinEffectsModule::Registration< EffectReverb > reg; }


struct EffectReverb::Editor
   : EffectEditor
{
   Editor(const EffectUIServices& services,
      EffectSettingsAccess& access, const EffectReverbSettings& settings
   )  : EffectEditor{ services, access }
      , mSettings{ settings }
   {}
   virtual ~Editor() = default;

   bool ValidateUI() override;
   bool UpdateUI() override;

   void PopulateOrExchange(ShuttleGui& S);

   EffectReverbSettings mSettings;

   bool mProcessingEvent = false;

#define SpinSlider(n) \
   wxSpinCtrl  *m ## n ## T; \
   wxSlider    *m ## n ## S;

   SpinSlider(RoomSize)
   SpinSlider(PreDelay)
   SpinSlider(Reverberance)
   SpinSlider(HfDamping)
   SpinSlider(ToneLow)
   SpinSlider(ToneHigh)
   SpinSlider(WetGain)
   SpinSlider(DryGain)
   SpinSlider(StereoWidth)

#undef SpinSlider

   wxCheckBox* mWetOnlyC;


#define SpinSliderHandlers(n) \
   void On ## n ## Slider(wxCommandEvent & evt); \
   void On ## n ## Text(wxCommandEvent & evt);

   SpinSliderHandlers(RoomSize)
   SpinSliderHandlers(PreDelay)
   SpinSliderHandlers(Reverberance)
   SpinSliderHandlers(HfDamping)
   SpinSliderHandlers(ToneLow)
   SpinSliderHandlers(ToneHigh)
   SpinSliderHandlers(WetGain)
   SpinSliderHandlers(DryGain)
   SpinSliderHandlers(StereoWidth)

#undef SpinSliderHandlers

   void OnCheckbox(wxCommandEvent &evt);

};


bool EffectReverb::Editor::ValidateUI()
{
   auto& rs = mSettings;

   rs.mRoomSize     = mRoomSizeS->GetValue();
   rs.mPreDelay     = mPreDelayS->GetValue();
   rs.mReverberance = mReverberanceS->GetValue();
   rs.mHfDamping    = mHfDampingS->GetValue();
   rs.mToneLow      = mToneLowS->GetValue();
   rs.mToneHigh     = mToneHighS->GetValue();
   rs.mWetGain      = mWetGainS->GetValue();
   rs.mDryGain      = mDryGainS->GetValue();
   rs.mStereoWidth  = mStereoWidthS->GetValue();
   rs.mWetOnly      = mWetOnlyC->GetValue();

   mAccess.ModifySettings
   (
      [this](EffectSettings& settings)
      {
         // pass back the modified settings to the MessageBuffer

         EffectReverb::GetSettings(settings) = mSettings;
         return nullptr;
      }
   );

   return true;
}


struct EffectReverb::Instance
   : public PerTrackEffect::Instance
   , public EffectInstanceWithBlockSize
{
   explicit Instance(const PerTrackEffect& effect)
      : PerTrackEffect::Instance{ effect }
   {}

   bool ProcessInitialize(EffectSettings &settings, double sampleRate,
      ChannelNames chanMap) override;

   size_t ProcessBlock(EffectSettings& settings,
      const float* const* inBlock, float* const* outBlock, size_t blockLen)  override;

   bool ProcessFinalize(void) noexcept override;

   // Realtime section

   bool RealtimeInitialize(EffectSettings& settings, double sampleRate) override
   {
      SetBlockSize(512);
      mSlaves.clear();

      mLastAppliedSettings = GetSettings(settings);
      mLastSampleRate = sampleRate;

      return true;
   }

   bool RealtimeAddProcessor(EffectSettings& settings, EffectOutputs *,
      unsigned numChannels, float sampleRate) override
   {
      EffectReverb::Instance slave(mProcessor);

      // The notion of ChannelNames is unavailable here,
      // so we'll have to force the stereo init, if this is the case
      //
      InstanceInit(settings, sampleRate,
         slave.mState, /*ChannelNames=*/nullptr, /*forceStereo=*/(numChannels == 2));

      mSlaves.push_back( std::move(slave) );
      return true;
   }

   bool RealtimeFinalize(EffectSettings& settings) noexcept override
   {
      mSlaves.clear();
      return true;
   }

   size_t RealtimeProcess(size_t group, EffectSettings& settings,
      const float* const* inbuf, float* const* outbuf, size_t numSamples) override
   {

      const auto& incomingSettings = GetSettings(settings);
      if ( !(incomingSettings == mLastAppliedSettings) )
      {
         const bool onlySimpleOnes = OnlySimpleParametersChanged(incomingSettings, mLastAppliedSettings);

         for (auto& slave : mSlaves)
         {
            for (unsigned int i = 0; i < slave.mState.mNumChans; i++)
            {
               auto& reverbCore = slave.mState.mP[i].reverb;
               const auto& is = incomingSettings;

               if (onlySimpleOnes)
               {
                  reverb_set_simple_params(&reverbCore, mLastSampleRate,
                                           is.mWetGain, is.mReverberance, is.mHfDamping, is.mToneLow, is.mToneHigh);
               }
               else
               {
                  // One of the non-simple parameters changed, so we need to do a full reinit
                  reverb_init(&reverbCore, mLastSampleRate,
                              is.mWetGain, is.mRoomSize, is.mReverberance, is.mHfDamping,
                              is.mPreDelay, is.mStereoWidth, is.mToneLow, is.mToneHigh   );
               }
            }
         }         

         mLastAppliedSettings = incomingSettings;
      }


      if (group >= mSlaves.size())
         return 0;
      return InstanceProcess(settings, mSlaves[group].mState, inbuf, outbuf, numSamples);
   }


   bool RealtimeSuspend() override
   {
      for (auto& slave : mSlaves)
      {
         for (unsigned int i = 0; i < slave.mState.mNumChans; i++)
         {
            reverb_clear( &(slave.mState.mP[i].reverb) );
         }
      }

      return true;
   }


   unsigned GetAudioOutCount() const override
   {
      return mChannels;
   }

   unsigned GetAudioInCount() const override
   {
      return mChannels;
   }

   bool InstanceInit(EffectSettings& settings, double sampleRate,
      EffectReverbState& data, ChannelNames chanMap, bool forceStereo);

   size_t InstanceProcess(EffectSettings& settings, EffectReverbState& data,
      const float* const* inBlock, float* const* outBlock, size_t blockLen);

   EffectReverbState mState;
   std::vector<EffectReverb::Instance> mSlaves;

   unsigned mChannels{ 2 };

   EffectReverbSettings mLastAppliedSettings;
   double mLastSampleRate{ 0 };
};



std::shared_ptr<EffectInstance>
EffectReverb::MakeInstance() const
{
   return std::make_shared<Instance>(*this);
}


EffectReverb::EffectReverb()
{
   SetLinearEffectFlag(true);
}

EffectReverb::~EffectReverb()
{
}

// ComponentInterface implementation

ComponentInterfaceSymbol EffectReverb::GetSymbol() const
{
   return Symbol;
}

TranslatableString EffectReverb::GetDescription() const
{
   return XO("Adds ambience or a \"hall effect\"");
}

ManualPageID EffectReverb::ManualPage() const
{
   return L"Reverb";
}

// EffectDefinitionInterface implementation

EffectType EffectReverb::GetType() const
{
   return EffectTypeProcess;
}

auto EffectReverb::RealtimeSupport() const -> RealtimeSince
{
   return RealtimeSince::After_3_1;
}

static size_t BLOCK = 16384;

bool EffectReverb::Instance::ProcessInitialize(EffectSettings& settings,
   double sampleRate, ChannelNames chanMap)
{
   // For descructive processing, fix the number of channels, maybe as 1 not 2
   auto& rs = GetSettings(settings);
   mChannels = rs.mStereoWidth ? 2 : 1;

   return InstanceInit(settings,
      sampleRate, mState, chanMap, /* forceStereo = */ false);
}


bool EffectReverb::Instance::InstanceInit(EffectSettings& settings,
   double sampleRate, EffectReverbState& state,
   ChannelNames chanMap, bool forceStereo)
{
   auto& rs = GetSettings(settings);

   bool isStereo = false;
   state.mNumChans = 1;
   if (    (chanMap && chanMap[0] != ChannelNameEOL && chanMap[1] == ChannelNameFrontRight)
        || forceStereo )
   {
      isStereo = true;
      state.mNumChans = 2;
   }

   state.mP = std::make_unique<Reverb_priv_ex[]>(state.mNumChans);

   for (unsigned int i = 0; i < state.mNumChans; i++)
   {
      reverb_create(&state.mP[i].reverb,
                    sampleRate,
                    rs.mWetGain,
                    rs.mRoomSize,
                    rs.mReverberance,
                    rs.mHfDamping,
                    rs.mPreDelay,
                    rs.mStereoWidth * (isStereo ? 1 : 0),
                    rs.mToneLow,
                    rs.mToneHigh,
                    BLOCK,
                    state.mP[i].wet);
   }

   return true;
}

bool EffectReverb::Instance::ProcessFinalize() noexcept
{
   return true;
}

size_t EffectReverb::Instance::ProcessBlock(EffectSettings& settings,
   const float* const* inBlock, float* const* outBlock, size_t blockLen)
{
   return InstanceProcess(settings, mState, inBlock, outBlock, blockLen);
}

size_t EffectReverb::Instance::InstanceProcess(EffectSettings& settings, EffectReverbState& state,
   const float* const* inBlock, float* const* outBlock, size_t blockLen)
{
   auto& rs = GetSettings(settings);

   const float *ichans[2] = {NULL, NULL};
   float *ochans[2] = {NULL, NULL};

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
         // Write the input samples to the reverb fifo.  Returned value is the address of the
         // fifo buffer which contains a copy of the input samples.
         state.mP[c].dry = (float *) fifo_write(&state.mP[c].reverb.input_fifo, len, ichans[c]);
         reverb_process(&state.mP[c].reverb, len);
      }

      if (state.mNumChans == 2)
      {
         for (decltype(len) i = 0; i < len; i++)
         {
            for (int w = 0; w < 2; w++)
            {
               ochans[w][i] = dryMult *
                              state.mP[w].dry[i] +
                              0.5 *
                              (state.mP[0].wet[w][i] + state.mP[1].wet[w][i]);
            }
         }
      }
      else
      {
         for (decltype(len) i = 0; i < len; i++)
         {
            ochans[0][i] = dryMult * 
                           state.mP[0].dry[i] +
                           state.mP[0].wet[0][i];
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

RegistryPaths EffectReverb::GetFactoryPresets() const
{
   RegistryPaths names;

   for (size_t i = 0; i < WXSIZEOF(FactoryPresets); i++)
   {
      names.push_back( FactoryPresets[i].name.Translation() );
   }

   return names;
}


OptionalMessage
EffectReverb::LoadFactoryPreset(int id, EffectSettings& settings) const
{
   if (id < 0 || id >= (int) WXSIZEOF(FactoryPresets))
   {
      return {};
   }

   EffectReverb::GetSettings(settings) = FactoryPresets[id].preset;

   return { nullptr };
}

// Effect implementation
std::unique_ptr<EffectEditor> EffectReverb::MakeEditor(
   ShuttleGui& S, EffectInstance&, EffectSettingsAccess& access,
   const EffectOutputs *) const
{
   auto& settings = access.Get();
   auto& myEffSettings = GetSettings(settings);

   auto result = std::make_unique<Editor>(*this, access, myEffSettings);
   result->PopulateOrExchange(S);
   return result;
}


void EffectReverb::Editor::PopulateOrExchange(ShuttleGui & S)
{
   S.AddSpace(0, 5);

   S.StartMultiColumn(3, wxEXPAND);
   {
      S.SetStretchyCol(2);

#define SpinSlider(n, p) \
      m ## n ## T = S.AddSpinCtrl( p, n.def, n.max, n.min); \
      BindTo(*m ## n ## T, wxEVT_SPINCTRL, &Editor::On ## n ## Text);\
      \
      m ## n ## S = S.Style(wxSL_HORIZONTAL).AddSlider( {}, n.def, n.max, n.min); \
      BindTo(*m ## n ## S, wxEVT_SLIDER, &Editor::On ## n ## Slider);

      SpinSlider(RoomSize,       XXO("&Room Size (%):"))
      SpinSlider(PreDelay,       XXO("&Pre-delay (ms):"))
      SpinSlider(Reverberance,   XXO("Rever&berance (%):"))
      SpinSlider(HfDamping,      XXO("Da&mping (%):"))
      SpinSlider(ToneLow,        XXO("Tone &Low (%):"))
      SpinSlider(ToneHigh,       XXO("Tone &High (%):"))
      SpinSlider(WetGain,        XXO("Wet &Gain (dB):"))
      SpinSlider(DryGain,        XXO("Dr&y Gain (dB):"))
      SpinSlider(StereoWidth,    XXO("Stereo Wid&th (%):"))

#undef SpinSlider

   }
   S.EndMultiColumn();

   S.StartHorizontalLay(wxCENTER, false);
   {
      mWetOnlyC =
      S.AddCheckBox(XXO("Wet O&nly"), WetOnly.def);
      BindTo(*mWetOnlyC, wxEVT_CHECKBOX, &Editor::OnCheckbox);
   }
   S.EndHorizontalLay();

}

bool EffectReverb::Editor::UpdateUI()
{
   // get the settings from the MessageBuffer and write them to our local copy
   mSettings = GetSettings(mAccess.Get());

   auto& rs = mSettings;

#define SetSpinSlider(n) \
   m ## n ## S->SetValue((int) rs.m ## n); \
   m ## n ## T->SetValue(wxString::Format(wxT("%d"), (int) rs.m ## n));

   SetSpinSlider(RoomSize);
   SetSpinSlider(PreDelay);
   SetSpinSlider(Reverberance);
   SetSpinSlider(HfDamping);
   SetSpinSlider(ToneLow);
   SetSpinSlider(ToneHigh);
   SetSpinSlider(WetGain);
   SetSpinSlider(DryGain);
   SetSpinSlider(StereoWidth);

#undef SetSpinSlider

   mWetOnlyC->SetValue((int) rs.mWetOnly);

   return true;
}


#define SpinSliderHandlers(n) \
   void EffectReverb::Editor::On ## n ## Slider(wxCommandEvent & evt) \
   { \
      if (mProcessingEvent) return; \
      mProcessingEvent = true; \
      m ## n ## T->SetValue(wxString::Format(wxT("%d"), evt.GetInt())); \
      mProcessingEvent = false; \
      ValidateUI(); \
      Publish(EffectSettingChanged{}); \
   } \
   void EffectReverb::Editor::On ## n ## Text(wxCommandEvent & evt) \
   { \
      if (mProcessingEvent) return; \
      mProcessingEvent = true; \
      m ## n ## S->SetValue(std::clamp<long>(evt.GetInt(), n.min, n.max)); \
      mProcessingEvent = false; \
      ValidateUI(); \
      Publish(EffectSettingChanged{}); \
   }

SpinSliderHandlers(RoomSize)
SpinSliderHandlers(PreDelay)
SpinSliderHandlers(Reverberance)
SpinSliderHandlers(HfDamping)
SpinSliderHandlers(ToneLow)
SpinSliderHandlers(ToneHigh)
SpinSliderHandlers(WetGain)
SpinSliderHandlers(DryGain)
SpinSliderHandlers(StereoWidth)

void EffectReverb::Editor::OnCheckbox(wxCommandEvent &evt)
{
   ValidateUI();
   Publish(EffectSettingChanged{});
}

#undef SpinSliderHandlers

bool operator==(const EffectReverbSettings& a, const EffectReverbSettings& b)
{
   // With C++20, all of this can be replaced by =default
   return      (a.mRoomSize     == b.mRoomSize)
            && (a.mPreDelay     == b.mPreDelay)
            && (a.mReverberance == b.mReverberance)
            && (a.mHfDamping    == b.mHfDamping)
            && (a.mToneLow      == b.mToneLow)
            && (a.mToneHigh     == b.mToneHigh)
            && (a.mWetGain      == b.mWetGain)
            && (a.mDryGain      == b.mDryGain)
            && (a.mStereoWidth  == b.mStereoWidth)
            && (a.mWetOnly      == b.mWetOnly);           
}

bool OnlySimpleParametersChanged(const EffectReverbSettings& a, const EffectReverbSettings& b)
{
   // A "simple" reverb parameter is one that when changed, does not require the
   // reverb allpass/comb filters to be reset. This distinction enables us to
   // code things so that the user can keep hearing the processed sound while
   // they tweak one of the simple parameters.

   const bool oneSimpleParameterChanged =

               (a.mReverberance != b.mReverberance)
            || (a.mHfDamping    != b.mHfDamping)
            || (a.mToneLow      != b.mToneLow)
            || (a.mToneHigh     != b.mToneHigh)
            || (a.mWetGain      != b.mWetGain);


   const bool allNonSimpleParametersStayedTheSame =

               (a.mRoomSize     == b.mRoomSize)
            && (a.mPreDelay     == b.mPreDelay)
            && (a.mStereoWidth  == b.mStereoWidth);           

   return oneSimpleParameterChanged && allNonSimpleParametersStayedTheSame;
}
