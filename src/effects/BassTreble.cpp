/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2016 Audacity Team.
   License: GPL v2 or later.  See License.txt.

   BassTreble.cpp
   Steve Daulton

******************************************************************//**

\class EffectBassTreble
\brief A high shelf and low shelf filter.

*//*******************************************************************/
#include "BassTreble.h"
#include "EffectEditor.h"
#include "LoadEffects.h"

#include <math.h>
#include <algorithm>

#include <wx/checkbox.h>
#include <wx/panel.h>
#include <wx/slider.h>

#include "../ShuttleGui.h"
#include "../WaveTrack.h"
#include "../widgets/valnum.h"

const EffectParameterMethods& EffectBassTreble::Parameters() const
{
   static CapturedParameters<EffectBassTreble,
      Bass, Treble, Gain, Link
   > parameters;
   return parameters;
}

// Used to communicate the type of the filter.
enum kShelfType
{
   kBass,
   kTreble
};

const ComponentInterfaceSymbol EffectBassTreble::Symbol
{ XO("Bass and Treble") };

namespace{ BuiltinEffectsModule::Registration< EffectBassTreble > reg; }



struct EffectBassTreble::Editor
   : EffectEditor
{
   Editor(const EffectUIServices& services,
      EffectSettingsAccess& access, const EffectBassTrebleSettings& settings
   )  : EffectEditor{ services, access }
      , mSettings{ settings }
   {}
   virtual ~Editor() = default;

   bool ValidateUI() override;
   bool UpdateUI() override;

   void PopulateOrExchange(ShuttleGui& S);

   wxWeakRef<wxWindow> mUIParent{};
   EffectBassTrebleSettings mSettings;

   wxSlider* mBassS;
   wxSlider* mTrebleS;
   wxSlider* mGainS;

   wxTextCtrl* mBassT;
   wxTextCtrl* mTrebleT;
   wxTextCtrl* mGainT;

   wxCheckBox* mLinkCheckBox;

   void OnBassText(wxCommandEvent& evt);
   void OnTrebleText(wxCommandEvent& evt);
   void OnGainText(wxCommandEvent& evt);
   void OnBassSlider(wxCommandEvent& evt);
   void OnTrebleSlider(wxCommandEvent& evt);
   void OnGainSlider(wxCommandEvent& evt);
   void OnLinkCheckbox(wxCommandEvent& evt);

   // Auto-adjust gain to reduce variation in peak level
   void UpdateGain(double oldVal, int control);

   void EnableApplyFromValidate()
   {
      EnableApply(mUIParent, mUIParent->Validate());
   }

   bool EnableApplyFromTransferDataFromWindow()
   {
      return EnableApply(
         mUIParent, mUIParent->TransferDataFromWindow());
   }
};


struct EffectBassTreble::Instance
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

   bool RealtimeAddProcessor(EffectSettings& settings, EffectOutputs* pOutputs,
      unsigned numChannels, float sampleRate) override;

   bool RealtimeFinalize(EffectSettings& settings) noexcept override;

   size_t RealtimeProcess(size_t group, EffectSettings& settings,
      const float* const* inbuf, float* const* outbuf, size_t numSamples)
      override;

   unsigned GetAudioInCount() const override;
   unsigned GetAudioOutCount() const override;

   static void InstanceInit(EffectSettings& settings, EffectBassTrebleState& data, float sampleRate);

   static size_t InstanceProcess(EffectSettings&        settings,
                                 EffectBassTrebleState& data,
                                 const float* const*    inBlock,
                                 float* const*          outBlock,
                                 size_t                 blockLen);

   static void Coefficients(double hz, double slope, double gain, double samplerate, int type,
      double& a0, double& a1, double& a2, double& b0, double& b1, double& b2);

   static float DoFilter(EffectBassTrebleState& data, float in);

   EffectBassTrebleState mState;
   std::vector<EffectBassTreble::Instance> mSlaves;
};


std::shared_ptr<EffectInstance>
EffectBassTreble::MakeInstance() const
{
   return std::make_shared<Instance>(*this);
}


EffectBassTreble::EffectBassTreble()
{
   SetLinearEffectFlag(true);
}

EffectBassTreble::~EffectBassTreble()
{
}

// ComponentInterface implementation

ComponentInterfaceSymbol EffectBassTreble::GetSymbol() const
{
   return Symbol;
}

TranslatableString EffectBassTreble::GetDescription() const
{
   return XO("Simple tone control effect");
}

ManualPageID EffectBassTreble::ManualPage() const
{
   return L"Bass_and_Treble";
}

// EffectDefinitionInterface implementation

EffectType EffectBassTreble::GetType() const
{
   return EffectTypeProcess;
}

auto EffectBassTreble::RealtimeSupport() const -> RealtimeSince
{
   return RealtimeSince::After_3_1;
}

unsigned EffectBassTreble::Instance::GetAudioInCount() const
{
   return 1;
}

unsigned EffectBassTreble::Instance::GetAudioOutCount() const
{
   return 1;
}

bool EffectBassTreble::Instance::ProcessInitialize(
   EffectSettings& settings, double sampleRate, ChannelNames)
{
   InstanceInit(settings, mState, sampleRate);
   return true;
}


size_t EffectBassTreble::Instance::ProcessBlock(EffectSettings& settings,
   const float* const* inBlock, float* const* outBlock, size_t blockLen)
{
   return InstanceProcess(settings, mState, inBlock, outBlock, blockLen);
}

bool EffectBassTreble::Instance::RealtimeInitialize(EffectSettings &, double)
{
   SetBlockSize(512);
   mSlaves.clear();
   return true;
}

bool EffectBassTreble::Instance::RealtimeAddProcessor(
   EffectSettings& settings, EffectOutputs* pOutputs,
   unsigned numChannels, float sampleRate)
{
   EffectBassTreble::Instance slave(mProcessor);

   InstanceInit(settings, slave.mState, sampleRate);

   mSlaves.push_back(slave);

   return true;
}

bool EffectBassTreble::Instance::RealtimeFinalize(EffectSettings &) noexcept
{
   mSlaves.clear();

   return true;
}

size_t EffectBassTreble::Instance::RealtimeProcess(size_t group, EffectSettings &settings,
   const float *const *inbuf, float *const *outbuf, size_t numSamples)
{
   if (group >= mSlaves.size())
      return 0;
   return InstanceProcess(settings, mSlaves[group].mState, inbuf, outbuf, numSamples);
}

bool EffectBassTreble::CheckWhetherSkipEffect(const EffectSettings& settings) const
{
   auto& ms = GetSettings(settings);

   return (ms.mBass == 0.0 && ms.mTreble == 0.0 && ms.mGain == 0.0);
}


// Effect implementation

std::unique_ptr<EffectEditor> EffectBassTreble::MakeEditor(
   ShuttleGui& S, EffectInstance&, EffectSettingsAccess& access,
   const EffectOutputs *) const
{
   auto& settings = access.Get();
   auto& myEffSettings = GetSettings(settings);

   auto result = std::make_unique<Editor>(*this, access, myEffSettings);
   result->PopulateOrExchange(S);
   return result;
 }

void EffectBassTreble::Editor::PopulateOrExchange(ShuttleGui & S)
{
   mUIParent = S.GetParent();
   auto& ms = mSettings;

   S.SetBorder(5);
   S.AddSpace(0, 5);

   S.StartStatic(XO("Tone controls"));
   {
      S.StartMultiColumn(3, wxEXPAND);
      {
         S.SetStretchyCol(2);

         // Bass control
         mBassT = S
            .Name(XO("Bass (dB):"))
            .Validator<FloatingPointValidator<double>>(
               1, &ms.mBass, NumValidatorStyle::DEFAULT, Bass.min, Bass.max)
            .AddTextBox(XXO("Ba&ss (dB):"), L"", 10);
         BindTo(*mBassT, wxEVT_TEXT, &Editor::OnBassText);

         mBassS = S
            .Name(XO("Bass"))
            .Style(wxSL_HORIZONTAL)
            .AddSlider( {}, 0, Bass.max * Bass.scale, Bass.min * Bass.scale);
         BindTo(*mBassS, wxEVT_SLIDER, &Editor::OnBassSlider);

         // Treble control
         mTrebleT = S
            .Validator<FloatingPointValidator<double>>(
               1, &ms.mTreble, NumValidatorStyle::DEFAULT, Treble.min, Treble.max)
            .AddTextBox(XXO("&Treble (dB):"), L"", 10);
         BindTo(*mTrebleT, wxEVT_TEXT, &Editor::OnTrebleText);

         mTrebleS = S
            .Name(XO("Treble"))
            .Style(wxSL_HORIZONTAL)
            .AddSlider( {}, 0, Treble.max * Treble.scale, Treble.min * Treble.scale);
         BindTo(*mTrebleS, wxEVT_SLIDER, &Editor::OnTrebleSlider);
      }
      S.EndMultiColumn();
   }
   S.EndStatic();

   S.StartStatic(XO("Output"));
   {
      S.StartMultiColumn(3, wxEXPAND);
      {
         S.SetStretchyCol(2);

         // Gain control
         mGainT = S
            .Validator<FloatingPointValidator<double>>(
               1, &ms.mGain, NumValidatorStyle::DEFAULT, Gain.min, Gain.max)
            .AddTextBox(XXO("&Volume (dB):"), L"", 10);
         BindTo(*mGainT, wxEVT_TEXT, &Editor::OnGainText);

         mGainS = S
            .Name(XO("Level"))
            .Style(wxSL_HORIZONTAL)
            .AddSlider( {}, 0, Gain.max * Gain.scale, Gain.min * Gain.scale);
         BindTo(*mGainS, wxEVT_SLIDER, &Editor::OnGainSlider);
      }
      S.EndMultiColumn();

      S.StartMultiColumn(2, wxCENTER);
      {
         // Link checkbox
         mLinkCheckBox =
         S
            .AddCheckBox(XXO("&Link Volume control to Tone controls"),
               Link.def);
         BindTo(*mLinkCheckBox, wxEVT_CHECKBOX, &Editor::OnLinkCheckbox);
      }
      S.EndMultiColumn();
   }
   S.EndStatic();   
}

bool EffectBassTreble::Editor::UpdateUI()
{
   // get the settings from the MessageBuffer and write them to our local copy
   const auto& settings = mAccess.Get();

   mSettings = GetSettings(settings);

   if (! mUIParent->TransferDataToWindow())
   {
      return false;
   }

   mBassS->       SetValue((int)(mSettings.mBass * Bass.scale));
   mTrebleS->     SetValue((int)(mSettings.mTreble *Treble.scale));
   mGainS->       SetValue((int)(mSettings.mGain * Gain.scale));
   mLinkCheckBox->SetValue(mSettings.mLink);

   return true;
}



// EffectBassTreble implementation

void EffectBassTreble::Instance::InstanceInit(EffectSettings& settings, EffectBassTrebleState& data, float sampleRate)
{
   auto& ms = GetSettings(settings);
   
   data.samplerate = sampleRate;
   data.slope = 0.4f;   // same slope for both filters
   data.hzBass = 250.0f;   // could be tunable in a more advanced version
   data.hzTreble = 4000.0f;   // could be tunable in a more advanced version

   data.a0Bass = 1;
   data.a1Bass = 0;
   data.a2Bass = 0;
   data.b0Bass = 0;
   data.b1Bass = 0;
   data.b2Bass = 0;

   data.a0Treble = 1;
   data.a1Treble = 0;
   data.a2Treble = 0;
   data.b0Treble = 0;
   data.b1Treble = 0;
   data.b2Treble = 0;

   data.xn1Bass = 0;
   data.xn2Bass = 0;
   data.yn1Bass = 0;
   data.yn2Bass = 0;

   data.xn1Treble = 0;
   data.xn2Treble = 0;
   data.yn1Treble = 0;
   data.yn2Treble = 0;

   data.bass = -1;
   data.treble = -1;
   data.gain = DB_TO_LINEAR(ms.mGain);

}

size_t EffectBassTreble::Instance::InstanceProcess(EffectSettings &settings,
   EffectBassTrebleState & data,
   const float *const *inBlock, float *const *outBlock, size_t blockLen)
{
   auto& ms = GetSettings(settings);

   const float *ibuf = inBlock[0];
   float *obuf = outBlock[0];

   // Set value to ensure correct rounding
   double oldBass = DB_TO_LINEAR(ms.mBass);
   double oldTreble = DB_TO_LINEAR(ms.mTreble);

   data.gain = DB_TO_LINEAR(ms.mGain);

   // Compute coefficients of the low shelf biquand IIR filter
   if (data.bass != oldBass)
      Coefficients(data.hzBass, data.slope, ms.mBass, data.samplerate, kBass,
                  data.a0Bass, data.a1Bass, data.a2Bass,
                  data.b0Bass, data.b1Bass, data.b2Bass);

   // Compute coefficients of the high shelf biquand IIR filter
   if (data.treble != oldTreble)
      Coefficients(data.hzTreble, data.slope, ms.mTreble, data.samplerate, kTreble,
                  data.a0Treble, data.a1Treble, data.a2Treble,
                  data.b0Treble, data.b1Treble, data.b2Treble);

   for (decltype(blockLen) i = 0; i < blockLen; i++) {
      obuf[i] = DoFilter(data, ibuf[i]) * data.gain;
   }

   return blockLen;
}



// Effect implementation


void EffectBassTreble::Instance::Coefficients(double hz, double slope, double gain, double samplerate, int type,
                                   double& a0, double& a1, double& a2,
                                   double& b0, double& b1, double& b2)
{
   double w = 2 * M_PI * hz / samplerate;
   double a = exp(log(10.0) * gain / 40);
   double b = sqrt((a * a + 1) / slope - (pow((a - 1), 2)));

   if (type == kBass)
   {
      b0 = a * ((a + 1) - (a - 1) * cos(w) + b * sin(w));
      b1 = 2 * a * ((a - 1) - (a + 1) * cos(w));
      b2 = a * ((a + 1) - (a - 1) * cos(w) - b * sin(w));
      a0 = ((a + 1) + (a - 1) * cos(w) + b * sin(w));
      a1 = -2 * ((a - 1) + (a + 1) * cos(w));
      a2 = (a + 1) + (a - 1) * cos(w) - b * sin(w);
   }
   else //assumed kTreble
   {
      b0 = a * ((a + 1) + (a - 1) * cos(w) + b * sin(w));
      b1 = -2 * a * ((a - 1) + (a + 1) * cos(w));
      b2 = a * ((a + 1) + (a - 1) * cos(w) - b * sin(w));
      a0 = ((a + 1) - (a - 1) * cos(w) + b * sin(w));
      a1 = 2 * ((a - 1) - (a + 1) * cos(w));
      a2 = (a + 1) - (a - 1) * cos(w) - b * sin(w);
   }
}

float EffectBassTreble::Instance::DoFilter(EffectBassTrebleState & data, float in)
{
   // Bass filter
   float out = (data.b0Bass * in + data.b1Bass * data.xn1Bass + data.b2Bass * data.xn2Bass -
         data.a1Bass * data.yn1Bass - data.a2Bass * data.yn2Bass) / data.a0Bass;
   data.xn2Bass = data.xn1Bass;
   data.xn1Bass = in;
   data.yn2Bass = data.yn1Bass;
   data.yn1Bass = out;

   // Treble filter
   in = out;
   out = (data.b0Treble * in + data.b1Treble * data.xn1Treble + data.b2Treble * data.xn2Treble -
         data.a1Treble * data.yn1Treble - data.a2Treble * data.yn2Treble) / data.a0Treble;
   data.xn2Treble = data.xn1Treble;
   data.xn1Treble = in;
   data.yn2Treble = data.yn1Treble;
   data.yn1Treble = out;

   return out;
}


void EffectBassTreble::Editor::OnBassText(wxCommandEvent & WXUNUSED(evt))
{
   auto& ms = mSettings;

   double oldBass = ms.mBass;

   if (!EnableApplyFromTransferDataFromWindow())
   {
      return;
   }

   if (ms.mLink)
      UpdateGain(oldBass, kBass);

   mBassS->SetValue((int) (ms.mBass * Bass.scale));

   ValidateUI();
   Publish(EffectSettingChanged{});
}

void EffectBassTreble::Editor::OnTrebleText(wxCommandEvent & WXUNUSED(evt))
{
   auto& ms = mSettings;

   double oldTreble = ms.mTreble;

   if (!EnableApplyFromTransferDataFromWindow())
   {
      return;
   }

   if (ms.mLink)
      UpdateGain(oldTreble, kTreble);

   mTrebleS->SetValue((int) (ms.mTreble * Treble.scale));

   ValidateUI();
   Publish(EffectSettingChanged{});
}

void EffectBassTreble::Editor::OnGainText(wxCommandEvent & WXUNUSED(evt))
{
   auto& ms = mSettings;

   if (!EnableApplyFromTransferDataFromWindow())
   {
      return;
   }

   mGainS->SetValue((int) (ms.mGain * Gain.scale));

   ValidateUI();
   Publish(EffectSettingChanged{});
}

void EffectBassTreble::Editor::OnBassSlider(wxCommandEvent & evt)
{
   auto& ms = mSettings;

   double oldBass = ms.mBass;
   ms.mBass = (double) evt.GetInt() / Bass.scale;
   mBassT->GetValidator()->TransferToWindow();

   if (ms.mLink)
      UpdateGain(oldBass, kBass);

   EnableApplyFromValidate();

   ValidateUI();
   Publish(EffectSettingChanged{});
}

void EffectBassTreble::Editor::OnTrebleSlider(wxCommandEvent & evt)
{
   auto& ms = mSettings;

   double oldTreble = ms.mTreble;
   ms.mTreble = (double) evt.GetInt() / Treble.scale;
   mTrebleT->GetValidator()->TransferToWindow();

   if (ms.mLink)
      UpdateGain(oldTreble, kTreble);

   EnableApplyFromValidate();

   ValidateUI();
   Publish(EffectSettingChanged{});
}

void EffectBassTreble::Editor::OnGainSlider(wxCommandEvent & evt)
{
   auto& ms = mSettings;

   ms.mGain = (double) evt.GetInt() / Gain.scale;
   mGainT->GetValidator()->TransferToWindow();

   EnableApplyFromValidate();

   ValidateUI();
   Publish(EffectSettingChanged{});
}

void EffectBassTreble::Editor::OnLinkCheckbox(wxCommandEvent& /*evt*/)
{
   auto& ms = mSettings;

   ms.mLink = mLinkCheckBox->GetValue();

   ValidateUI();
   Publish(EffectSettingChanged{});
}

void EffectBassTreble::Editor::UpdateGain(double oldVal, int control)
{
   auto& ms = mSettings;

   double newVal;
   oldVal = (oldVal > 0)? oldVal / 2.0 : oldVal / 4.0;

   if (control == kBass)
      newVal = (ms.mBass > 0)? ms.mBass / 2.0 : ms.mBass / 4.0;
   else
      newVal = (ms.mTreble > 0)? ms.mTreble / 2.0 : ms.mTreble / 4.0;

   ms.mGain -= newVal - oldVal;
   ms.mGain = std::min(Gain.max, std::max(Gain.min, ms.mGain));

   mGainS->SetValue(ms.mGain);
   mGainT->GetValidator()->TransferToWindow();

}




bool EffectBassTreble::Editor::ValidateUI()
{
   // This bit was copied from the original override of the effect's TransferDataFromWindow
   if (! mUIParent->Validate() || !mUIParent->TransferDataFromWindow())
   {
      return false;
   }


   mAccess.ModifySettings
   (
      [this](EffectSettings& settings)
   {
      // pass back the modified settings to the MessageBuffer
      //
      EffectBassTreble::GetSettings(settings) = mSettings;

      return nullptr;
   }
   );

   return true;
}

