/**********************************************************************

  Audacity: A Digital Audio Editor

  Phaser.cpp

  Effect programming:
  Nasca Octavian Paul (Paul Nasca)

  UI programming:
  Dominic Mazzoni (with the help of wxDesigner)
  Vaughan Johnson (Preview)

*******************************************************************//**

\class EffectPhaser
\brief An Effect that changes frequencies in a time varying manner.

*//*******************************************************************/
#include "Phaser.h"
#include "EffectEditor.h"
#include "LoadEffects.h"

#include <math.h>

#include <wx/slider.h>

#include "ShuttleGui.h"
#include "valnum.h"
#include <wx/weakref.h>

const EffectParameterMethods& EffectPhaser::Parameters() const
{
   static CapturedParameters<EffectPhaser,
      Stages, DryWet, Freq, Phase, Depth, Feedback, OutGain
   > parameters{

      [](EffectPhaser &, EffectSettings &, EffectPhaserSettings &e, bool updating)
      {
         if (updating)
            e.mStages &= ~1; // must be even, but don't complain about it
         return true;
      },
   };
   return parameters;
}

//
#define phaserlfoshape 4.0

// How many samples are processed before recomputing the lfo value again
#define lfoskipsamples 20

//
// EffectPhaser
//

const ComponentInterfaceSymbol EffectPhaser::Symbol
{ XO("Phaser") };

namespace{ BuiltinEffectsModule::Registration< EffectPhaser > reg; }


struct EffectPhaser::Editor
   : EffectEditor
{
   Editor(const EffectUIServices& services,
      EffectSettingsAccess& access, const EffectPhaserSettings& settings
   )  : EffectEditor{ services, access }
      , mSettings{ settings }
   {}
   virtual ~Editor() = default;

   bool ValidateUI() override;
   bool UpdateUI() override;

   void PopulateOrExchange(ShuttleGui& S);

   wxWeakRef<wxWindow> mUIParent;
   EffectPhaserSettings mSettings;

   wxTextCtrl* mStagesT;
   wxTextCtrl* mDryWetT;
   wxTextCtrl* mFreqT;
   wxTextCtrl* mPhaseT;
   wxTextCtrl* mDepthT;
   wxTextCtrl* mFeedbackT;
   wxTextCtrl* mOutGainT;

   wxSlider* mStagesS;
   wxSlider* mDryWetS;
   wxSlider* mFreqS;
   wxSlider* mPhaseS;
   wxSlider* mDepthS;
   wxSlider* mFeedbackS;
   wxSlider* mOutGainS;

   void OnStagesSlider(wxCommandEvent& evt);
   void OnDryWetSlider(wxCommandEvent& evt);
   void OnFeedbackSlider(wxCommandEvent& evt);
   void OnDepthSlider(wxCommandEvent& evt);
   void OnPhaseSlider(wxCommandEvent& evt);
   void OnFreqSlider(wxCommandEvent& evt);
   void OnGainSlider(wxCommandEvent& evt);

   void OnStagesText(wxCommandEvent& evt);
   void OnDryWetText(wxCommandEvent& evt);
   void OnFeedbackText(wxCommandEvent& evt);
   void OnDepthText(wxCommandEvent& evt);
   void OnPhaseText(wxCommandEvent& evt);
   void OnFreqText(wxCommandEvent& evt);
   void OnGainText(wxCommandEvent& evt);
      
   
   void EnableApplyFromValidate()
   {
      EnableApply(mUIParent, mUIParent->Validate());
   }

   bool EnableApplyFromTransferDataFromWindow()
   {
      return EnableApply(mUIParent, mUIParent->TransferDataFromWindow());
   }
   
};


struct EffectPhaser::Instance
   : public PerTrackEffect::Instance
   , public EffectInstanceWithBlockSize
{
   explicit Instance(const PerTrackEffect& effect)
      : PerTrackEffect::Instance{ effect }
   {}

   bool ProcessInitialize(EffectSettings& settings,
      double          sampleRate,
      ChannelNames     chanMap) override;

   size_t ProcessBlock(EffectSettings& settings,
      const float* const* inBlock, float* const* outBlock, size_t blockLen)  override;

   bool RealtimeInitialize(EffectSettings& settings, double) override;

   bool RealtimeAddProcessor(EffectSettings& settings,
      EffectOutputs *pOutputs, unsigned numChannels, float sampleRate) override;

   bool RealtimeFinalize(EffectSettings& settings) noexcept override;

   size_t RealtimeProcess(size_t group, EffectSettings& settings,
      const float* const* inbuf, float* const* outbuf, size_t numSamples)
      override;

   unsigned GetAudioInCount() const override;
   unsigned GetAudioOutCount() const override;

   void InstanceInit(EffectSettings& settings, EffectPhaserState& data, float sampleRate);

   size_t InstanceProcess(EffectSettings& settings,
      EffectPhaserState& data,
      const float* const* inBlock,
      float* const* outBlock,
      size_t                 blockLen);

   void Coefficients(double hz, double slope, double gain, double samplerate, int type,
      double& a0, double& a1, double& a2, double& b0, double& b1, double& b2);

   float DoFilter(EffectPhaserState& data, float in);

   EffectPhaserState mState;
   std::vector<EffectPhaser::Instance> mSlaves;
};


std::shared_ptr<EffectInstance>
EffectPhaser::MakeInstance() const
{
   return std::make_shared<Instance>(*this);
}



EffectPhaser::EffectPhaser()
{
   SetLinearEffectFlag(true);
}

EffectPhaser::~EffectPhaser()
{
}

// ComponentInterface implementation

ComponentInterfaceSymbol EffectPhaser::GetSymbol() const
{
   return Symbol;
}

TranslatableString EffectPhaser::GetDescription() const
{
   return XO("Combines phase-shifted signals with the original signal");
}

ManualPageID EffectPhaser::ManualPage() const
{
   return L"Phaser";
}

// EffectDefinitionInterface implementation

EffectType EffectPhaser::GetType() const
{
   return EffectTypeProcess;
}

auto EffectPhaser::RealtimeSupport() const -> RealtimeSince
{
   return RealtimeSince::After_3_1;
}

unsigned EffectPhaser::Instance::GetAudioInCount() const
{
   return 1;
}

unsigned EffectPhaser::Instance::GetAudioOutCount() const
{
   return 1;
}

bool EffectPhaser::Instance::ProcessInitialize(
   EffectSettings& settings, double sampleRate, ChannelNames chanMap)
{
   InstanceInit(settings, mState, sampleRate);
   if (chanMap[0] == ChannelNameFrontRight)
      mState.phase += M_PI;
   return true;
}

size_t EffectPhaser::Instance::ProcessBlock(EffectSettings &settings,
   const float *const *inBlock, float *const *outBlock, size_t blockLen)
{
   return InstanceProcess(settings, mState, inBlock, outBlock, blockLen);
}

bool EffectPhaser::Instance::RealtimeInitialize(EffectSettings&, double)
{
   SetBlockSize(512);
   mSlaves.clear();
   return true;
}

bool EffectPhaser::Instance::RealtimeAddProcessor(
   EffectSettings& settings, EffectOutputs *, unsigned, float sampleRate)
{
   EffectPhaser::Instance slave(mProcessor);

   InstanceInit(settings, slave.mState, sampleRate);

   mSlaves.push_back(slave);

   return true;
}

bool EffectPhaser::Instance::RealtimeFinalize(EffectSettings &) noexcept
{
   mSlaves.clear();

   return true;
}

size_t EffectPhaser::Instance::RealtimeProcess(size_t group, EffectSettings &settings,
   const float *const *inbuf, float *const *outbuf, size_t numSamples)
{
   if (group >= mSlaves.size())
      return 0;
   return InstanceProcess(settings, mSlaves[group].mState, inbuf, outbuf, numSamples);
}

// Effect implementation
std::unique_ptr<EffectEditor> EffectPhaser::MakeEditor(
   ShuttleGui& S, EffectInstance&, EffectSettingsAccess& access,
   const EffectOutputs *) const
{
   auto& settings = access.Get();
   auto& myEffSettings = GetSettings(settings);

   auto result = std::make_unique<Editor>(*this, access, myEffSettings);
   result->PopulateOrExchange(S);
   return result;
}

void EffectPhaser::Editor::PopulateOrExchange(ShuttleGui& S)
{
   mUIParent = S.GetParent();
   auto& ms = mSettings;

   S.SetBorder(5);
   S.AddSpace(0, 5);

   S.StartMultiColumn(3, wxEXPAND);
   {
      S.SetStretchyCol(2);

      mStagesT = S
         .Validator<IntegerValidator<int>>(
            &ms.mStages, NumValidatorStyle::DEFAULT, Stages.min, Stages.max)
         .AddTextBox(XXO("&Stages:"), L"", 15);
      BindTo(*mStagesT, wxEVT_TEXT, &Editor::OnStagesText);

      mStagesS = S
         .Name(XO("Stages"))
         .Style(wxSL_HORIZONTAL)
         .MinSize( { 100, -1 } )
         .AddSlider( {}, Stages.def * Stages.scale, Stages.max * Stages.scale, Stages.min * Stages.scale);
      mStagesS->SetLineSize(2);
      BindTo(*mStagesS, wxEVT_SLIDER, &Editor::OnStagesSlider);

      mDryWetT = S
         .Validator<IntegerValidator<int>>(
            &ms.mDryWet, NumValidatorStyle::DEFAULT, DryWet.min, DryWet.max)
         .AddTextBox(XXO("&Dry/Wet:"), L"", 15);
      BindTo(*mDryWetT, wxEVT_TEXT, &Editor::OnDryWetText);

      mDryWetS = S
         .Name(XO("Dry Wet"))
         .Style(wxSL_HORIZONTAL)
         .MinSize( { 100, -1 } )
         .AddSlider( {}, DryWet.def * DryWet.scale, DryWet.max * DryWet.scale, DryWet.min * DryWet.scale);
      BindTo(*mDryWetS, wxEVT_SLIDER, &Editor::OnDryWetSlider);

      mFreqT = S
         .Validator<FloatingPointValidator<double>>(
            5, &ms.mFreq, NumValidatorStyle::ONE_TRAILING_ZERO, Freq.min, Freq.max)
         .AddTextBox(XXO("LFO Freq&uency (Hz):"), L"", 15);
      BindTo(*mFreqT, wxEVT_TEXT, &Editor::OnFreqText);

      mFreqS = S
         .Name(XO("LFO frequency in hertz"))
         .Style(wxSL_HORIZONTAL)
         .MinSize( { 100, -1 } )
         .AddSlider( {}, Freq.def * Freq.scale, Freq.max * Freq.scale, 0.0);
      BindTo(*mFreqS, wxEVT_SLIDER, &Editor::OnFreqSlider);

      mPhaseT = S
         .Validator<FloatingPointValidator<double>>(
            1, &ms.mPhase, NumValidatorStyle::DEFAULT, Phase.min, Phase.max)
         .AddTextBox(XXO("LFO Sta&rt Phase (deg.):"), L"", 15);
      BindTo(*mPhaseT, wxEVT_TEXT, &Editor::OnPhaseText);

      mPhaseS = S
         .Name(XO("LFO start phase in degrees"))
         .Style(wxSL_HORIZONTAL)
         .MinSize( { 100, -1 } )
         .AddSlider( {}, Phase.def * Phase.scale, Phase.max * Phase.scale, Phase.min * Phase.scale);
      mPhaseS->SetLineSize(10);
      BindTo(*mPhaseS, wxEVT_SLIDER, &Editor::OnPhaseSlider);

      mDepthT = S
         .Validator<IntegerValidator<int>>(
            &ms.mDepth, NumValidatorStyle::DEFAULT, Depth.min, Depth.max)
         .AddTextBox(XXO("Dept&h:"), L"", 15);
      BindTo(*mDepthT, wxEVT_TEXT, &Editor::OnDepthText);

      mDepthS = S
         .Name(XO("Depth in percent"))
         .Style(wxSL_HORIZONTAL)
         .MinSize( { 100, -1 } )
         .AddSlider( {}, Depth.def * Depth.scale, Depth.max * Depth.scale, Depth.min * Depth.scale);
      BindTo(*mDepthS, wxEVT_SLIDER, &Editor::OnDepthSlider);

      mFeedbackT = S
         .Validator<IntegerValidator<int>>(
            &ms.mFeedback, NumValidatorStyle::DEFAULT, Feedback.min, Feedback.max)
         .AddTextBox(XXO("Feedbac&k (%):"), L"", 15);
      BindTo(*mFeedbackT, wxEVT_TEXT, &Editor::OnFeedbackText);

      mFeedbackS = S
         .Name(XO("Feedback in percent"))
         .Style(wxSL_HORIZONTAL)
         .MinSize( { 100, -1 } )
         .AddSlider( {}, Feedback.def * Feedback.scale, Feedback.max * Feedback.scale, Feedback.min * Feedback.scale);
      mFeedbackS->SetLineSize(10);
      BindTo(*mFeedbackS, wxEVT_SLIDER, &Editor::OnFeedbackSlider);

      mOutGainT = S
         .Validator<FloatingPointValidator<double>>(
            1, &ms.mOutGain, NumValidatorStyle::DEFAULT, OutGain.min, OutGain.max)
         .AddTextBox(XXO("&Output gain (dB):"), L"", 12);
      BindTo(*mOutGainT, wxEVT_TEXT, &Editor::OnGainText);

      mOutGainS = S
         .Name(XO("Output gain (dB)"))
         .Style(wxSL_HORIZONTAL)
         .MinSize( { 100, -1 } )
         .AddSlider( {}, OutGain.def * OutGain.scale, OutGain.max * OutGain.scale, OutGain.min * OutGain.scale);
      BindTo(*mOutGainS, wxEVT_SLIDER, &Editor::OnGainSlider);
   }
   S.EndMultiColumn();

}

bool EffectPhaser::Editor::UpdateUI()
{
   // get the settings from the MessageBuffer and write them to our local copy
   const auto& settings = mAccess.Get();

   mSettings = GetSettings(settings);

   if (!mUIParent->TransferDataToWindow())
   {
      return false;
   }

   mStagesS->  SetValue((int) (mSettings.mStages * Stages.scale));
   mDryWetS->  SetValue((int) (mSettings.mDryWet * DryWet.scale));
   mFreqS->    SetValue((int) (mSettings.mFreq * Freq.scale));
   mPhaseS->   SetValue((int) (mSettings.mPhase * Phase.scale));
   mDepthS->   SetValue((int) (mSettings.mDepth * Depth.scale));
   mFeedbackS->SetValue((int) (mSettings.mFeedback * Feedback.scale));
   mOutGainS-> SetValue((int) (mSettings.mOutGain * OutGain.scale));

   return true;
}


bool EffectPhaser::Editor::ValidateUI()
{
   // This bit was copied from the original override of TransferDataFromWindow
   if (!mUIParent->Validate() || !mUIParent->TransferDataFromWindow())
   {
      return false;
   }

   auto& ms = mSettings;

   if (ms.mStages & 1)    // must be even
   {
      ms.mStages &= ~1;
      mStagesT->GetValidator()->TransferToWindow();
   }

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


// EffectPhaser implementation

void EffectPhaser::Instance::InstanceInit(EffectSettings& settings, EffectPhaserState & data, float sampleRate)
{
   auto& ms = GetSettings(settings);

   data.samplerate = sampleRate;

   for (int j = 0; j < ms.mStages; j++)
   {
      data.old[j] = 0;
   }

   data.skipcount = 0;
   data.gain = 0;
   data.fbout = 0;
   data.laststages = 0;
   data.outgain = 0;

   return;
}

size_t EffectPhaser::Instance::InstanceProcess(EffectSettings &settings,
   EffectPhaserState & data,
   const float *const *inBlock, float *const *outBlock, size_t blockLen)
{
   auto& ms = GetSettings(settings);

   const float *ibuf = inBlock[0];
   float *obuf = outBlock[0];

   for (int j = data.laststages; j < ms.mStages; j++)
   {
      data.old[j] = 0;
   }
   data.laststages = ms.mStages;

   data.lfoskip = ms.mFreq * 2 * M_PI / data.samplerate;
   data.phase = ms.mPhase * M_PI / 180;
   data.outgain = DB_TO_LINEAR(ms.mOutGain);

   for (decltype(blockLen) i = 0; i < blockLen; i++)
   {
      double in = ibuf[i];

      double m = in + data.fbout * ms.mFeedback / 101;  // Feedback must be less than 100% to avoid infinite gain.

      if (((data.skipcount++) % lfoskipsamples) == 0)
      {
         //compute sine between 0 and 1
         data.gain =
            (1.0 +
             cos(data.skipcount.as_double() * data.lfoskip
                 + data.phase)) / 2.0;

         // change lfo shape
         data.gain = expm1(data.gain * phaserlfoshape) / expm1(phaserlfoshape);

         // attenuate the lfo
         data.gain = 1.0 - data.gain / 255.0 * ms.mDepth;
      }

      // phasing routine
      for (int j = 0; j < ms.mStages; j++)
      {
         double tmp = data.old[j];
         data.old[j] = data.gain * tmp + m;
         m = tmp - data.gain * data.old[j];
      }
      data.fbout = m;

      obuf[i] = (float) (data.outgain * (m * ms.mDryWet + in * (255 - ms.mDryWet)) / 255);
   }

   return blockLen;
}

void EffectPhaser::Editor::OnStagesSlider(wxCommandEvent & evt)
{
   auto& ms = mSettings;

   ms.mStages = (evt.GetInt() / Stages.scale) & ~1;  // must be even;
   mStagesT->GetValidator()->TransferToWindow();
   EnableApplyFromValidate();

   ValidateUI();
   Publish(EffectSettingChanged{});
}

void EffectPhaser::Editor::OnDryWetSlider(wxCommandEvent & evt)
{
   auto& ms = mSettings;

   ms.mDryWet = evt.GetInt() / DryWet.scale;
   mDryWetT->GetValidator()->TransferToWindow();
   EnableApplyFromValidate();

   ValidateUI();
   Publish(EffectSettingChanged{});
}

void EffectPhaser::Editor::OnFreqSlider(wxCommandEvent & evt)
{
   auto& ms = mSettings;

   ms.mFreq = (double) evt.GetInt() / Freq.scale;
   if (ms.mFreq < Freq.min) ms.mFreq = Freq.min;
   mFreqT->GetValidator()->TransferToWindow();
   EnableApplyFromValidate();

   ValidateUI();
   Publish(EffectSettingChanged{});
}

void EffectPhaser::Editor::OnPhaseSlider(wxCommandEvent & evt)
{
   auto& ms = mSettings;

   int val = ((evt.GetInt() + 5) / 10) * 10; // round to nearest multiple of 10
   val = val > Phase.max * Phase.scale ? Phase.max * Phase.scale : val;
   mPhaseS->SetValue(val);
   ms.mPhase =  (double) val / Phase.scale;
   mPhaseT->GetValidator()->TransferToWindow();
   EnableApplyFromValidate();

   ValidateUI();
   Publish(EffectSettingChanged{});
}

void EffectPhaser::Editor::OnDepthSlider(wxCommandEvent & evt)
{
   auto& ms = mSettings;

   ms.mDepth = evt.GetInt() / Depth.scale;
   mDepthT->GetValidator()->TransferToWindow();
   EnableApplyFromValidate();

   ValidateUI();
   Publish(EffectSettingChanged{});
}

void EffectPhaser::Editor::OnFeedbackSlider(wxCommandEvent & evt)
{
   auto& ms = mSettings;

   int val = evt.GetInt();
   val = ((val + (val > 0 ? 5 : -5)) / 10) * 10; // round to nearest multiple of 10
   val = val > Feedback.max * Feedback.scale ? Feedback.max * Feedback.scale : val;
   mFeedbackS->SetValue(val);
   ms.mFeedback = val / Feedback.scale;
   mFeedbackT->GetValidator()->TransferToWindow();
   EnableApplyFromValidate();

   ValidateUI();
   Publish(EffectSettingChanged{});
}

void EffectPhaser::Editor::OnGainSlider(wxCommandEvent & evt)
{
   auto& ms = mSettings;

   ms.mOutGain = evt.GetInt() / OutGain.scale;
   mOutGainT->GetValidator()->TransferToWindow();
   EnableApplyFromValidate();

   ValidateUI();
   Publish(EffectSettingChanged{});
}

void EffectPhaser::Editor::OnStagesText(wxCommandEvent & WXUNUSED(evt))
{
   auto& ms = mSettings;

   if (!EnableApplyFromTransferDataFromWindow())
   {
      return;
   }

   mStagesS->SetValue((int) (ms.mStages * Stages.scale));

   ValidateUI();
   Publish(EffectSettingChanged{});
}

void EffectPhaser::Editor::OnDryWetText(wxCommandEvent & WXUNUSED(evt))
{
   auto& ms = mSettings;

   if (!EnableApplyFromTransferDataFromWindow())
   {
      return;
   }

   mDryWetS->SetValue((int) (ms.mDryWet * DryWet.scale));

   ValidateUI();
   Publish(EffectSettingChanged{});
}

void EffectPhaser::Editor::OnFreqText(wxCommandEvent & WXUNUSED(evt))
{
   auto& ms = mSettings;

   if (!EnableApplyFromTransferDataFromWindow())
   {
      return;
   }

   mFreqS->SetValue((int) (ms.mFreq * Freq.scale));

   ValidateUI();
   Publish(EffectSettingChanged{});
}

void EffectPhaser::Editor::OnPhaseText(wxCommandEvent & WXUNUSED(evt))
{
   auto& ms = mSettings;

   if (!EnableApplyFromTransferDataFromWindow())
   {
      return;
   }

   mPhaseS->SetValue((int) (ms.mPhase * Phase.scale));

   ValidateUI();
   Publish(EffectSettingChanged{});
}

void EffectPhaser::Editor::OnDepthText(wxCommandEvent & WXUNUSED(evt))
{
   auto& ms = mSettings;

   if (!EnableApplyFromTransferDataFromWindow())
   {
      return;
   }

   mDepthS->SetValue((int) (ms.mDepth * Depth.scale));

   ValidateUI();
   Publish(EffectSettingChanged{});
}

void EffectPhaser::Editor::OnFeedbackText(wxCommandEvent & WXUNUSED(evt))
{
   auto& ms = mSettings;

   if (!EnableApplyFromTransferDataFromWindow())
   {
      return;
   }

   mFeedbackS->SetValue((int) (ms.mFeedback * Feedback.scale));

   ValidateUI();
   Publish(EffectSettingChanged{});
}

void EffectPhaser::Editor::OnGainText(wxCommandEvent & WXUNUSED(evt))
{
   auto& ms = mSettings;

   if (!EnableApplyFromTransferDataFromWindow())
   {
      return;
   }

   mOutGainS->SetValue((int) (ms.mOutGain * OutGain.scale));

   ValidateUI();
   Publish(EffectSettingChanged{});
}
