/**********************************************************************

  Audacity: A Digital Audio Editor

  Wahwah.cpp

  Effect programming:
  Nasca Octavian Paul (Paul Nasca)

  UI programming:
  Dominic Mazzoni (with the help of wxDesigner)
  Vaughan Johnson (Preview)

*******************************************************************//**

\class EffectWahwah
\brief An Effect that adds a 'spectral glide'.

*//*******************************************************************/


#include "Wahwah.h"
#include "LoadEffects.h"

#include <math.h>

#include <wx/slider.h>

#include "ShuttleGui.h"
#include "valnum.h"

const EffectParameterMethods& EffectWahwah::Parameters() const
{
   static CapturedParameters<EffectWahwah,
      Freq, Phase, Depth, Res, FreqOfs, OutGain
   > parameters;
   return parameters;
}

// How many samples are processed before recomputing the lfo value again
#define lfoskipsamples 30

//
// EffectWahwah
//

const ComponentInterfaceSymbol EffectWahwah::Symbol
{ XO("Wahwah") };

namespace{ BuiltinEffectsModule::Registration< EffectWahwah > reg; }

struct EffectWahwah::Validator
   : EffectUIValidator
{
   Validator(EffectUIClientInterface& effect,
      EffectSettingsAccess& access, const EffectWahwahSettings& settings)
      : EffectUIValidator{ effect, access }
      , mSettings{ settings }
   {}
   virtual ~Validator() = default;

   Effect& GetEffect() const { return static_cast<Effect&>(mEffect); }

   bool ValidateUI() override;
   bool UpdateUI() override;

   void PopulateOrExchange(ShuttleGui& S);

   void OnFreqSlider(wxCommandEvent& evt);
   void OnPhaseSlider(wxCommandEvent& evt);
   void OnDepthSlider(wxCommandEvent& evt);
   void OnResonanceSlider(wxCommandEvent& evt);
   void OnFreqOffSlider(wxCommandEvent& evt);
   void OnGainSlider(wxCommandEvent& evt);

   void OnFreqText(wxCommandEvent& evt);
   void OnPhaseText(wxCommandEvent& evt);
   void OnDepthText(wxCommandEvent& evt);
   void OnResonanceText(wxCommandEvent& evt);
   void OnFreqOffText(wxCommandEvent& evt);
   void OnGainText(wxCommandEvent& evt);

   wxTextCtrl* mFreqT;
   wxTextCtrl* mPhaseT;
   wxTextCtrl* mDepthT;
   wxTextCtrl* mResT;
   wxTextCtrl* mFreqOfsT;
   wxTextCtrl* mOutGainT;

   wxSlider* mFreqS;
   wxSlider* mPhaseS;
   wxSlider* mDepthS;
   wxSlider* mResS;
   wxSlider* mFreqOfsS;
   wxSlider* mOutGainS;


   wxWeakRef<wxWindow> mUIParent;
   EffectWahwahSettings mSettings;

   void EnableApplyFromValidate()
   {
      EnableApply(mUIParent, mUIParent->Validate());
   }

   bool EnableApplyFromTransferDataToWindow()
   {
      return EnableApply(mUIParent, mUIParent->TransferDataFromWindow());
   }
};


bool EffectWahwah::Validator::ValidateUI()
{
   mAccess.ModifySettings
   (
      [this](EffectSettings& settings)
      {
         // pass back the modified settings to the MessageBuffer
         GetSettings(settings) = mSettings;
         return nullptr;
      }
   );

   return true;
}


struct EffectWahwah::Instance
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

   //bool ProcessFinalize() noexcept override;

   bool RealtimeInitialize(EffectSettings& settings, double) override;

   bool RealtimeAddProcessor(EffectSettings& settings,
      EffectOutputs *pOutputs, unsigned numChannels, float sampleRate) override;

   bool RealtimeFinalize(EffectSettings& settings) noexcept override;

   size_t RealtimeProcess(size_t group, EffectSettings& settings,
      const float* const* inbuf, float* const* outbuf, size_t numSamples)
      override;


   void InstanceInit(EffectSettings& settings, EffectWahwahState& data, float sampleRate);

   size_t InstanceProcess(EffectSettings& settings, EffectWahwahState& data,
      const float* const* inBlock, float* const* outBlock, size_t blockLen);

   unsigned GetAudioInCount() const override;
   unsigned GetAudioOutCount() const override;

   EffectWahwahState mState;
   std::vector<EffectWahwah::Instance> mSlaves;
};


std::shared_ptr<EffectInstance> EffectWahwah::MakeInstance() const
{
   return std::make_shared<Instance>(*this);
}

EffectWahwah::EffectWahwah()
{
   SetLinearEffectFlag(true);
}

EffectWahwah::~EffectWahwah()
{
}

// ComponentInterface implementation

ComponentInterfaceSymbol EffectWahwah::GetSymbol() const
{
   return Symbol;
}

TranslatableString EffectWahwah::GetDescription() const
{
   return XO("Rapid tone quality variations, like that guitar sound so popular in the 1970's");
}

ManualPageID EffectWahwah::ManualPage() const
{
   return L"Wahwah";
}

// EffectDefinitionInterface implementation

EffectType EffectWahwah::GetType() const
{
   return EffectTypeProcess;
}

auto EffectWahwah::RealtimeSupport() const -> RealtimeSince
{
   return RealtimeSince::After_3_1;
}

bool EffectWahwah::Instance::ProcessInitialize(EffectSettings & settings,
   double sampleRate, ChannelNames chanMap)
{
   InstanceInit(settings, mState, sampleRate);
   if (chanMap[0] == ChannelNameFrontRight)
      mState.phase += M_PI;
   return true;
}

size_t EffectWahwah::Instance::ProcessBlock(EffectSettings &settings,
   const float *const *inBlock, float *const *outBlock, size_t blockLen)
{
   return InstanceProcess(settings, mState, inBlock, outBlock, blockLen);
}

bool EffectWahwah::Instance::RealtimeInitialize(EffectSettings &, double)
{
   SetBlockSize(512);
   mSlaves.clear();
   return true;
}

bool EffectWahwah::Instance::RealtimeAddProcessor(
   EffectSettings &settings, EffectOutputs *, unsigned, float sampleRate)
{
   EffectWahwah::Instance slave(mProcessor);

   InstanceInit(settings, slave.mState, sampleRate);

   mSlaves.push_back(slave);

   return true;
}

bool EffectWahwah::Instance::RealtimeFinalize(EffectSettings &) noexcept
{
   mSlaves.clear();

   return true;
}

size_t EffectWahwah::Instance::RealtimeProcess(size_t group, EffectSettings &settings,
   const float *const *inbuf, float *const *outbuf, size_t numSamples)
{
   if (group >= mSlaves.size())
      return 0;
   return InstanceProcess(settings, mSlaves[group].mState, inbuf, outbuf, numSamples);
}

// Effect implementation

std::unique_ptr<EffectUIValidator> EffectWahwah::PopulateOrExchange(
   ShuttleGui & S, EffectInstance &, EffectSettingsAccess &access,
   const EffectOutputs *)
{
   auto& settings = access.Get();
   auto& myEffSettings = GetSettings(settings);
   auto result = std::make_unique<Validator>(*this, access, myEffSettings);
   result->PopulateOrExchange(S);
   return result;
}

void EffectWahwah::Validator::PopulateOrExchange(ShuttleGui & S)
{
   mUIParent = S.GetParent();
   auto& ms = mSettings;

   S.SetBorder(5);
   S.AddSpace(0, 5);

   S.StartMultiColumn(3, wxEXPAND);
   {
      S.SetStretchyCol(2);
   
      mFreqT = S
         .Validator<FloatingPointValidator<double>>(
            5, &ms.mFreq, NumValidatorStyle::ONE_TRAILING_ZERO, Freq.min, Freq.max)
         .AddTextBox(XXO("LFO Freq&uency (Hz):"), L"", 12);
      BindTo(*mFreqT, wxEVT_TEXT, &Validator::OnFreqText);

      mFreqS = S
         .Name(XO("LFO frequency in hertz"))
         .Style(wxSL_HORIZONTAL)
         .MinSize( { 100, -1 } )
         .AddSlider( {}, Freq.def * Freq.scale, Freq.max * Freq.scale, Freq.min * Freq.scale);
      BindTo(*mFreqS, wxEVT_SLIDER, &Validator::OnFreqSlider);

      mPhaseT = S
         .Validator<FloatingPointValidator<double>>(
            1, &ms.mPhase, NumValidatorStyle::DEFAULT, Phase.min, Phase.max)
         .AddTextBox(XXO("LFO Sta&rt Phase (deg.):"), L"", 12);
      BindTo(*mPhaseT, wxEVT_TEXT, &Validator::OnPhaseText);

      mPhaseS = S
         .Name(XO("LFO start phase in degrees"))
         .Style(wxSL_HORIZONTAL)
         .MinSize( { 100, -1 } )
         .AddSlider( {}, Phase.def * Phase.scale, Phase.max * Phase.scale, Phase.min * Phase.scale);
      mPhaseS->SetLineSize(10);
      BindTo(*mPhaseS, wxEVT_SLIDER, &Validator::OnPhaseSlider);

      mDepthT = S
         .Validator<IntegerValidator<int>>(
            &ms.mDepth, NumValidatorStyle::DEFAULT, Depth.min, Depth.max)
         .AddTextBox(XXO("Dept&h (%):"), L"", 12);
      BindTo(*mDepthT, wxEVT_TEXT, &Validator::OnDepthText);

      mDepthS = S
         .Name(XO("Depth in percent"))
         .Style(wxSL_HORIZONTAL)
         .MinSize( { 100, -1 } )
         .AddSlider( {}, Depth.def * Depth.scale, Depth.max * Depth.scale, Depth.min * Depth.scale);
      BindTo(*mDepthS, wxEVT_SLIDER, &Validator::OnDepthSlider);

      mResT = S
         .Validator<FloatingPointValidator<double>>(
            1, &ms.mRes, NumValidatorStyle::DEFAULT, Res.min, Res.max)
         .AddTextBox(XXO("Reso&nance:"), L"", 12);
      BindTo(*mResT, wxEVT_TEXT, &Validator::OnResonanceText);

      mResS = S
         .Name(XO("Resonance"))
         .Style(wxSL_HORIZONTAL)
         .MinSize( { 100, -1 } )
         .AddSlider( {}, Res.def * Res.scale, Res.max * Res.scale, Res.min * Res.scale);
      BindTo(*mResS, wxEVT_SLIDER, &Validator::OnResonanceSlider);

      mFreqOfsT = S
         .Validator<IntegerValidator<int>>(
            &ms.mFreqOfs, NumValidatorStyle::DEFAULT, FreqOfs.min, FreqOfs.max)
         .AddTextBox(XXO("Wah Frequency Offse&t (%):"), L"", 12);
      BindTo(*mFreqOfsT, wxEVT_TEXT, &Validator::OnFreqOffText);

      mFreqOfsS = S
         .Name(XO("Wah frequency offset in percent"))
         .Style(wxSL_HORIZONTAL)
         .MinSize( { 100, -1 } )
         .AddSlider( {}, FreqOfs.def * FreqOfs.scale, FreqOfs.max * FreqOfs.scale, FreqOfs.min * FreqOfs.scale);
      BindTo(*mFreqOfsS, wxEVT_SLIDER, &Validator::OnFreqOffSlider);

      mOutGainT = S
         .Validator<FloatingPointValidator<double>>(
            1, &ms.mOutGain, NumValidatorStyle::DEFAULT, OutGain.min, OutGain.max)
         .AddTextBox(XXO("&Output gain (dB):"), L"", 12);
      BindTo(*mOutGainT, wxEVT_TEXT, &Validator::OnGainText);

      mOutGainS = S
         .Name(XO("Output gain (dB)"))
         .Style(wxSL_HORIZONTAL)
         .MinSize( { 100, -1 } )
         .AddSlider( {}, OutGain.def * OutGain.scale, OutGain.max * OutGain.scale, OutGain.min * OutGain.scale);
      BindTo(*mOutGainS, wxEVT_SLIDER, &Validator::OnGainSlider);
   }
   S.EndMultiColumn();
}

bool EffectWahwah::Validator::UpdateUI()
{
   // get the settings from the MessageBuffer and write them to our local copy
   const auto& settings = mAccess.Get();

   mSettings = GetSettings(settings);

   auto& ms = mSettings;

   mFreqS->SetValue((int)(ms.mFreq * Freq.scale));
   mPhaseS->SetValue((int)(ms.mPhase * Phase.scale));
   mDepthS->SetValue((int)(ms.mDepth * Depth.scale));
   mResS->SetValue((int)(ms.mRes * Res.scale));
   mFreqOfsS->SetValue((int)(ms.mFreqOfs * FreqOfs.scale));
   mOutGainS->SetValue((int)(ms.mOutGain * OutGain.scale));
   
   return true;
}

// EffectWahwah implementation

void EffectWahwah::Instance::InstanceInit(EffectSettings& settings, EffectWahwahState & data, float sampleRate)
{
   auto& ms = GetSettings(settings);

   data.samplerate = sampleRate;
   data.lfoskip = ms.mFreq * 2 * M_PI / sampleRate;
   data.skipcount = 0;
   data.xn1 = 0;
   data.xn2 = 0;
   data.yn1 = 0;
   data.yn2 = 0;
   data.b0 = 0;
   data.b1 = 0;
   data.b2 = 0;
   data.a0 = 0;
   data.a1 = 0;
   data.a2 = 0;

   data.depth = ms.mDepth / 100.0;
   data.freqofs = ms.mFreqOfs / 100.0;
   data.phase = ms.mPhase * M_PI / 180.0;
   data.outgain = DB_TO_LINEAR(ms.mOutGain);
}

size_t EffectWahwah::Instance::InstanceProcess(EffectSettings& settings,
   EffectWahwahState & data,
   const float *const *inBlock, float *const *outBlock, size_t blockLen)
{
   auto& ms = GetSettings(settings);
   
   const float *ibuf = inBlock[0];
   float *obuf = outBlock[0];
   double frequency, omega, sn, cs, alpha;
   double in, out;

   data.lfoskip = ms.mFreq * 2 * M_PI / data.samplerate;
   data.depth = ms.mDepth / 100.0;
   data.freqofs = ms.mFreqOfs / 100.0;

   data.phase = ms.mPhase * M_PI / 180.0;
   data.outgain = DB_TO_LINEAR(ms.mOutGain);

   for (decltype(blockLen) i = 0; i < blockLen; i++)
   {
      in = (double) ibuf[i];

      if ((data.skipcount++) % lfoskipsamples == 0)
      {
         frequency = (1 + cos(data.skipcount * data.lfoskip + data.phase)) / 2;
         frequency = frequency * data.depth * (1 - data.freqofs) + data.freqofs;
         frequency = exp((frequency - 1) * 6);
         omega = M_PI * frequency;
         sn = sin(omega);
         cs = cos(omega);
         alpha = sn / (2 * ms.mRes);
         data.b0 = (1 - cs) / 2;
         data.b1 = 1 - cs;
         data.b2 = (1 - cs) / 2;
         data.a0 = 1 + alpha;
         data.a1 = -2 * cs;
         data.a2 = 1 - alpha;
      };
      out = (data.b0 * in + data.b1 * data.xn1 + data.b2 * data.xn2 - data.a1 * data.yn1 - data.a2 * data.yn2) / data.a0;
      data.xn2 = data.xn1;
      data.xn1 = in;
      data.yn2 = data.yn1;
      data.yn1 = out;
      out *= data.outgain;

      obuf[i] = (float) out;
   }

   return blockLen;
}

unsigned EffectWahwah::Instance::GetAudioOutCount() const
{
   return 1;
}

unsigned EffectWahwah::Instance::GetAudioInCount() const
{
   return 1;
}

void EffectWahwah::Validator::OnFreqSlider(wxCommandEvent& evt)
{
   auto& ms = mSettings;

   ms.mFreq = (double)evt.GetInt() / Freq.scale;
   mFreqT->GetValidator()->TransferToWindow();

   EnableApplyFromValidate();
   ValidateUI();
   Publish(EffectSettingChanged{});
}

void EffectWahwah::Validator::OnPhaseSlider(wxCommandEvent& evt)
{
   auto& ms = mSettings;

   int val = ((evt.GetInt() + 5) / 10) * 10; // round to nearest multiple of 10
   val = val > Phase.max * Phase.scale ? Phase.max * Phase.scale : val;
   mPhaseS->SetValue(val);
   ms.mPhase = (double)val / Phase.scale;
   mPhaseT->GetValidator()->TransferToWindow();

   EnableApplyFromValidate();
   ValidateUI();
   Publish(EffectSettingChanged{});
}

void EffectWahwah::Validator::OnDepthSlider(wxCommandEvent& evt)
{
   auto& ms = mSettings;

   ms.mDepth = evt.GetInt() / Depth.scale;
   mDepthT->GetValidator()->TransferToWindow();

   EnableApplyFromValidate();
   ValidateUI();
   Publish(EffectSettingChanged{});
}

void EffectWahwah::Validator::OnResonanceSlider(wxCommandEvent& evt)
{
   auto& ms = mSettings;

   ms.mRes = (double)evt.GetInt() / Res.scale;
   mResT->GetValidator()->TransferToWindow();

   EnableApplyFromValidate();
   ValidateUI();
   Publish(EffectSettingChanged{});
}

void EffectWahwah::Validator::OnFreqOffSlider(wxCommandEvent& evt)
{
   auto& ms = mSettings;

   ms.mFreqOfs = evt.GetInt() / FreqOfs.scale;
   mFreqOfsT->GetValidator()->TransferToWindow();

   EnableApplyFromValidate();
   ValidateUI();
   Publish(EffectSettingChanged{});
}

void EffectWahwah::Validator::OnGainSlider(wxCommandEvent& evt)
{
   auto& ms = mSettings;

   ms.mOutGain = evt.GetInt() / OutGain.scale;
   mOutGainT->GetValidator()->TransferToWindow();

   EnableApplyFromValidate();
   ValidateUI();
   Publish(EffectSettingChanged{});
}

void EffectWahwah::Validator::OnFreqText(wxCommandEvent& WXUNUSED(evt))
{
   auto& ms = mSettings;

   if (!EnableApplyFromTransferDataToWindow())
   {
      return;
   }

   mFreqS->SetValue((int)(ms.mFreq * Freq.scale));
   ValidateUI();
   Publish(EffectSettingChanged{});
}

void EffectWahwah::Validator::OnPhaseText(wxCommandEvent& WXUNUSED(evt))
{
   auto& ms = mSettings;

   if (!EnableApplyFromTransferDataToWindow())
   {
      return;
   }

   mPhaseS->SetValue((int)(ms.mPhase * Phase.scale));
   ValidateUI();
   Publish(EffectSettingChanged{});
}

void EffectWahwah::Validator::OnDepthText(wxCommandEvent& WXUNUSED(evt))
{
   auto& ms = mSettings;

   if (!EnableApplyFromTransferDataToWindow())
   {
      return;
   }

   mDepthS->SetValue((int)(ms.mDepth * Depth.scale));
   ValidateUI();
   Publish(EffectSettingChanged{});
}

void EffectWahwah::Validator::OnResonanceText(wxCommandEvent& WXUNUSED(evt))
{
   auto& ms = mSettings;

   if (!EnableApplyFromTransferDataToWindow())
   {
      return;
   }

   mResS->SetValue((int)(ms.mRes * Res.scale));
   ValidateUI();
   Publish(EffectSettingChanged{});
}

void EffectWahwah::Validator::OnFreqOffText(wxCommandEvent& WXUNUSED(evt))
{
   auto& ms = mSettings;

   if (!EnableApplyFromTransferDataToWindow())
   {
      return;
   }

   mFreqOfsS->SetValue((int)(ms.mFreqOfs * FreqOfs.scale));
   ValidateUI();
   Publish(EffectSettingChanged{});
}

void EffectWahwah::Validator::OnGainText(wxCommandEvent& WXUNUSED(evt))
{
   auto& ms = mSettings;

   if (!EnableApplyFromTransferDataToWindow())
   {
      return;
   }

   mOutGainS->SetValue((int)(ms.mOutGain * OutGain.scale));
   ValidateUI();
   Publish(EffectSettingChanged{});
}
