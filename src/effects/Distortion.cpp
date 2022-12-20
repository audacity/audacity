/**********************************************************************

  Audacity: A Digital Audio Editor

  Distortion.cpp

  Steve Daulton

  // TODO: Add a graph display of the waveshaper equation.
  // TODO: Allow the user to draw the graph.

******************************************************************//**

\class EffectDistortion
\brief A WaveShaper distortion effect.

*//*******************************************************************/


#include "Distortion.h"
#include "LoadEffects.h"

#include <cmath>
#include <algorithm>
//#define _USE_MATH_DEFINES

// Belt and braces
#ifndef M_PI
#define M_PI 3.1415926535897932384626433832795
#endif
#ifndef M_PI_2
#define M_PI_2 1.57079632679489661923132169163975 
#endif

#include <wx/checkbox.h>
#include <wx/choice.h>
#include <wx/intl.h>
#include <wx/valgen.h>
#include <wx/log.h>
#include <wx/slider.h>
#include <wx/stattext.h>

#include "Prefs.h"
#include "../ShuttleGui.h"
#include "../widgets/valnum.h"

const EnumValueSymbol EffectDistortion::kTableTypeStrings[nTableTypes] =
{
   { XO("Hard Clipping") },
   { XO("Soft Clipping") },
   { XO("Soft Overdrive") },
   { XO("Medium Overdrive") },
   { XO("Hard Overdrive") },
   { XO("Cubic Curve (odd harmonics)") },
   { XO("Even Harmonics") },
   { XO("Expand and Compress") },
   { XO("Leveller") },
   { XO("Rectifier Distortion") },
   { XO("Hard Limiter 1413") }
};

const EffectParameterMethods& EffectDistortion::Parameters() const
{
   static CapturedParameters<EffectDistortion,
      TableTypeIndx, DCBlock, Threshold_dB, NoiseFloor, Param1, Param2, Repeats
   > parameters{
      [](EffectDistortion &e, EffectSettings &settings, EffectDistortionSettings &p,
         bool updating) {
         if (!updating)
            e.mMaster.mThreshold = DB_TO_LINEAR(p.mThreshold_dB);
         return true;
      }
   };
   return parameters;
}

// How many samples are processed before recomputing the lookup table again
#define skipsamples 1000

static const struct
{
   const TranslatableString name;
   EffectDistortionSettings params;
}
FactoryPresets[] =
{
   //                                           Table    DCBlock  threshold   floor       Param1   Param2   Repeats
   // Defaults:                                   0       false   -6.0       -70.0(off)     50.0     50.0     1
   //
   // xgettext:no-c-format
   { XO("Hard clip -12dB, 80% make-up gain"),     { 0,        0,      -12.0,      -70.0,      0.0,     80.0,    0 } },
   // xgettext:no-c-format
   { XO("Soft clip -12dB, 80% make-up gain"),     { 1,        0,      -12.0,      -70.0,      50.0,    80.0,    0 } },
   { XO("Fuzz Box"),                              { 1,        0,      -30.0,      -70.0,      80.0,    80.0,    0 } },
   { XO("Walkie-talkie"),                         { 1,        0,      -50.0,      -70.0,      60.0,    80.0,    0 } },
   { XO("Blues drive sustain"),                   { 2,        0,       -6.0,      -70.0,      30.0,    80.0,    0 } },
   { XO("Light Crunch Overdrive"),                { 3,        0,       -6.0,      -70.0,      20.0,    80.0,    0 } },
   { XO("Heavy Overdrive"),                       { 4,        0,       -6.0,      -70.0,      90.0,    80.0,    0 } },
   { XO("3rd Harmonic (Perfect Fifth)"),          { 5,        0,       -6.0,      -70.0,     100.0,    60.0,    0 } },
   { XO("Valve Overdrive"),                       { 6,        1,       -6.0,      -70.0,      30.0,    40.0,    0 } },
   { XO("2nd Harmonic (Octave)"),                 { 6,        1,       -6.0,      -70.0,      50.0,     0.0,    0 } },
   { XO("Gated Expansion Distortion"),            { 7,        0,       -6.0,      -70.0,      30.0,    80.0,    0 } },
   { XO("Leveller, Light, -70dB noise floor"),    { 8,        0,       -6.0,      -70.0,       0.0,    50.0,    1 } },
   { XO("Leveller, Moderate, -70dB noise floor"), { 8,        0,       -6.0,      -70.0,       0.0,    50.0,    2 } },
   { XO("Leveller, Heavy, -70dB noise floor"),    { 8,        0,       -6.0,      -70.0,       0.0,    50.0,    3 } },
   { XO("Leveller, Heavier, -70dB noise floor"),  { 8,        0,       -6.0,      -70.0,       0.0,    50.0,    4 } },
   { XO("Leveller, Heaviest, -70dB noise floor"), { 8,        0,       -6.0,      -70.0,       0.0,    50.0,    5 } },
   { XO("Half-wave Rectifier"),                   { 9,        0,       -6.0,      -70.0,      50.0,    50.0,    0 } },
   { XO("Full-wave Rectifier"),                   { 9,        0,       -6.0,      -70.0,     100.0,    50.0,    0 } },
   { XO("Full-wave Rectifier (DC blocked)"),      { 9,        1,       -6.0,      -70.0,     100.0,    50.0,    0 } },
   { XO("Percussion Limiter"),                    {10,        0,      -12.0,      -70.0,     100.0,    30.0,    0 } },
};

TranslatableString defaultLabel(int index)
{
   static const TranslatableString names[] = {
      XO("Upper Threshold"),
      XO("Noise Floor"),
      XO("Parameter 1"),
      XO("Parameter 2"),
      XO("Number of repeats"),
   };

   return names[ index ];
}

//
// EffectDistortion
//

const ComponentInterfaceSymbol EffectDistortion::Symbol
{ XO("Distortion") };

namespace{ BuiltinEffectsModule::Registration< EffectDistortion > reg; }


struct EffectDistortion::Validator
   : EffectUIValidator
{
   Validator(EffectUIClientInterface& effect,
      EffectSettingsAccess& access, EffectDistortionSettings& settings)
      : EffectUIValidator{ effect, access }
      , mSettings{ settings }
   {}
   virtual ~Validator() = default;

   Effect& GetEffect() const { return static_cast<Effect&>(mEffect); }

   bool ValidateUI() override;
   bool UpdateUI() override;

   void PopulateOrExchange(ShuttleGui& S);

   // Control Handlers
   void OnTypeChoice(wxCommandEvent& evt);
   void OnDCBlockCheckbox(wxCommandEvent& evt);
   void OnThresholdText(wxCommandEvent& evt);
   void OnThresholdSlider(wxCommandEvent& evt);
   void OnNoiseFloorText(wxCommandEvent& evt);
   void OnNoiseFloorSlider(wxCommandEvent& evt);
   void OnParam1Text(wxCommandEvent& evt);
   void OnParam1Slider(wxCommandEvent& evt);
   void OnParam2Text(wxCommandEvent& evt);
   void OnParam2Slider(wxCommandEvent& evt);
   void OnRepeatsText(wxCommandEvent& evt);
   void OnRepeatsSlider(wxCommandEvent& evt);

   wxChoice* mTypeChoiceCtrl;
   wxTextCtrl* mThresholdT;
   wxTextCtrl* mNoiseFloorT;
   wxTextCtrl* mParam1T;
   wxTextCtrl* mParam2T;
   wxTextCtrl* mRepeatsT;

   wxSlider* mThresholdS;
   wxSlider* mNoiseFloorS;
   wxSlider* mParam1S;
   wxSlider* mParam2S;
   wxSlider* mRepeatsS;

   wxCheckBox* mDCBlockCheckBox;

   wxStaticText* mThresholdTxt;
   wxStaticText* mNoiseFloorTxt;
   wxStaticText* mParam1Txt;
   wxStaticText* mParam2Txt;
   wxStaticText* mRepeatsTxt;

   wxString mOldThresholdTxt;
   wxString mOldmNoiseFloorTxt;
   wxString mOldParam1Txt;
   wxString mOldParam2Txt;
   wxString mOldRepeatsTxt;

   EffectDistortionSettings& mSettings;

   EffectDistortionState& GetState()
   {
      // LATER: the state will be taken from the instance which is linked to the validator
      EffectDistortion& effect = dynamic_cast<EffectDistortion&>(mEffect);
      return effect.mMaster;
   }

   void UpdateControl(control id, bool enable, TranslatableString name);
   void UpdateUIControls();
   void UpdateControlText(wxTextCtrl* textCtrl, wxString& string, bool enabled);


   wxWindow* GetUIParent()
   {
      Effect& actualEffect = static_cast<Effect&>(mEffect);
      return actualEffect.GetUIParent();
   }

};


bool EffectDistortion::Validator::ValidateUI()
{
   {
      // This section was copied from the original
      // EffectDistortion::TransferDataFromWindow
      //
      // However, the call to ->Validate would bring up an error dialog
      // saying "Empty value"

      if ( /*!GetUIParent()->Validate() ||*/ !GetUIParent()->TransferDataFromWindow())
      {
         return false;
      }

      GetState().mThreshold = DB_TO_LINEAR(mSettings.mThreshold_dB);
   }


   mAccess.ModifySettings
   (
      [this](EffectSettings& settings)
   {
      // pass back the modified settings to the MessageBuffer

      // TODO uncomment at last step
      //EffectEcho::GetSettings(settings) = mSettings;

      return nullptr;
   }
   );

   return true;
}

EffectDistortion::EffectDistortion()
{
   Parameters().Reset(*this);
   wxASSERT(nTableTypes == WXSIZEOF(kTableTypeStrings));
   mMakeupGain = 1.0;
   mMaster.mbSavedFilterState = DCBlock.def;

   SetLinearEffectFlag(false);
}

EffectDistortion::~EffectDistortion()
{
}

// ComponentInterface implementation

ComponentInterfaceSymbol EffectDistortion::GetSymbol() const
{
   return Symbol;
}

TranslatableString EffectDistortion::GetDescription() const
{
   return XO("Waveshaping distortion effect");
}

ManualPageID EffectDistortion::ManualPage() const
{
   return L"Distortion";
}

// EffectDefinitionInterface implementation

EffectType EffectDistortion::GetType() const
{
   return EffectTypeProcess;
}

auto EffectDistortion::RealtimeSupport() const -> RealtimeSince
{
   // TODO reenable after achieving statelessness
   return RealtimeSince::Never;
}

unsigned EffectDistortion::GetAudioInCount() const
{
   return 1;
}

unsigned EffectDistortion::GetAudioOutCount() const
{
   return 1;
}

bool EffectDistortion::ProcessInitialize(
   EffectSettings &, double sampleRate, ChannelNames chanMap)
{
   InstanceInit(mMaster, sampleRate);
   return true;
}

size_t EffectDistortion::ProcessBlock(EffectSettings &settings,
   const float *const *inBlock, float *const *outBlock, size_t blockLen)
{
   return InstanceProcess(settings, mMaster, inBlock, outBlock, blockLen);
}

bool EffectDistortion::RealtimeInitialize(EffectSettings &, double)
{
   SetBlockSize(512);
   mSlaves.clear();
   return true;
}

bool EffectDistortion::RealtimeAddProcessor(
   EffectSettings &, EffectOutputs *, unsigned, float sampleRate)
{
   EffectDistortionState slave;

   InstanceInit(slave, sampleRate);

   mSlaves.push_back(slave);

   return true;
}

bool EffectDistortion::RealtimeFinalize(EffectSettings &) noexcept
{
   mSlaves.clear();

   return true;
}

size_t EffectDistortion::RealtimeProcess(size_t group, EffectSettings &settings,
   const float *const *inbuf, float *const *outbuf, size_t numSamples)
{
   if (group >= mSlaves.size())
      return 0;
   return InstanceProcess(settings, mSlaves[group], inbuf, outbuf, numSamples);
}

RegistryPaths EffectDistortion::GetFactoryPresets() const
{
   RegistryPaths names;

   for (size_t i = 0; i < WXSIZEOF(FactoryPresets); i++)
   {
      names.push_back( FactoryPresets[i].name.Translation() );
   }

   return names;
}

OptionalMessage
EffectDistortion::LoadFactoryPreset(int id, EffectSettings &) const
{
   // To do: externalize state so const_cast isn't needed
   return const_cast<EffectDistortion*>(this)->DoLoadFactoryPreset(id);
}

OptionalMessage EffectDistortion::DoLoadFactoryPreset(int id)
{
   if (id < 0 || id >= (int) WXSIZEOF(FactoryPresets))
   {
      return {};
   }

   mSettings = FactoryPresets[id].params;
   mMaster.mThreshold = DB_TO_LINEAR(mSettings.mThreshold_dB);

   return { nullptr };
}


// Effect implementation

std::unique_ptr<EffectUIValidator>
EffectDistortion::PopulateOrExchange(ShuttleGui& S, EffectInstance& instance,
   EffectSettingsAccess& access, const EffectOutputs* pOutputs)
{
   // TODO
//   auto& settings = access.Get();
//   auto& myEffSettings = GetSettings(settings);
   auto& myEffSettings = mSettings;
   auto result = std::make_unique<Validator>(*this, access, myEffSettings);
   result->PopulateOrExchange(S);
   return result;
}

// STEP TODO: add all links to OnSomething handlers
void EffectDistortion::Validator::PopulateOrExchange(ShuttleGui& S)
{
   auto& ms = mSettings;

   S.AddSpace(0, 5);
   S.StartVerticalLay();
   {
      S.StartMultiColumn(4, wxCENTER);
      {
         mTypeChoiceCtrl = S
            .MinSize( { -1, -1 } )
            .Validator<wxGenericValidator>(&ms.mTableChoiceIndx)
            .AddChoice(XXO("Distortion type:"),
               Msgids(kTableTypeStrings, nTableTypes));

         BindTo(*mTypeChoiceCtrl, wxEVT_CHOICE, &Validator::OnTypeChoice);

         mDCBlockCheckBox = S.AddCheckBox(XXO("DC blocking filter"),
                                       DCBlock.def);

         BindTo(*mDCBlockCheckBox, wxEVT_CHECKBOX, &Validator::OnDCBlockCheckbox);
      }
      S.EndMultiColumn();
      S.AddSpace(0, 10);


      S.StartStatic(XO("Threshold controls"));
      {
         S.StartMultiColumn(4, wxEXPAND);
         S.SetStretchyCol(2);
         {
            // Allow space for first Column
            S.AddSpace(250,0); S.AddSpace(0,0); S.AddSpace(0,0); S.AddSpace(0,0); 

            // Upper threshold control
            mThresholdTxt = S.AddVariableText(defaultLabel(0),
               false, wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT);
            mThresholdT = S
               .Name(defaultLabel(0))
               .Validator<FloatingPointValidator<double>>(
                  2, &ms.mThreshold_dB, NumValidatorStyle::DEFAULT,
                  Threshold_dB.min, Threshold_dB.max)
               .AddTextBox( {}, wxT(""), 10);

            BindTo(*mThresholdT, wxEVT_TEXT, &Validator::OnThresholdText);

            mThresholdS = S
               .Name(defaultLabel(0))
               .Style(wxSL_HORIZONTAL)
               .AddSlider( {}, 0,
                  DB_TO_LINEAR(Threshold_dB.max) * Threshold_dB.scale,
                  DB_TO_LINEAR(Threshold_dB.min) * Threshold_dB.scale);
            S.AddSpace(20, 0);

            BindTo(*mThresholdS, wxEVT_SLIDER, &Validator::OnThresholdSlider);
            
            // Noise floor control
            mNoiseFloorTxt = S.AddVariableText(defaultLabel(1),
               false, wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT);
            mNoiseFloorT = S
               .Name(defaultLabel(1))
               .Validator<FloatingPointValidator<double>>(
                  2, &ms.mNoiseFloor, NumValidatorStyle::DEFAULT,
                  NoiseFloor.min, NoiseFloor.max
               )
               .AddTextBox( {}, wxT(""), 10);

            BindTo(*mNoiseFloorT, wxEVT_TEXT, &Validator::OnNoiseFloorText);

            mNoiseFloorS = S
               .Name(defaultLabel(1))
               .Style(wxSL_HORIZONTAL)
               .AddSlider( {}, 0, NoiseFloor.max, NoiseFloor.min);
            S.AddSpace(20, 0);

            BindTo(*mNoiseFloorS, wxEVT_SLIDER, &Validator::OnNoiseFloorSlider);
         }
         S.EndMultiColumn();
      }
      S.EndStatic();

      S.StartStatic(XO("Parameter controls"));
      {
         S.StartMultiColumn(4, wxEXPAND);
         S.SetStretchyCol(2);
         {
            // Allow space for first Column
            S.AddSpace(250,0); S.AddSpace(0,0); S.AddSpace(0,0); S.AddSpace(0,0); 

            // Parameter1 control
            mParam1Txt = S.AddVariableText(defaultLabel(2),
               false, wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT);
            mParam1T = S
               .Name(defaultLabel(2))
               .Validator<FloatingPointValidator<double>>(
                  2, &ms.mParam1, NumValidatorStyle::DEFAULT,
                  Param1.min, Param1.max
               )
               .AddTextBox( {}, wxT(""), 10);

            BindTo(*mParam1T, wxEVT_TEXT, &Validator::OnParam1Text);

            mParam1S = S
               .Name(defaultLabel(2))
               .Style(wxSL_HORIZONTAL)
               .AddSlider( {}, 0, Param1.max, Param1.min);
            S.AddSpace(20, 0);

            BindTo(*mParam1S, wxEVT_SLIDER, &Validator::OnParam1Slider);
            
            // Parameter2 control
            mParam2Txt = S.AddVariableText(defaultLabel(3),
               false, wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT);
            mParam2T = S
               .Name(defaultLabel(3))
               .Validator<FloatingPointValidator<double>>(
                  2, &ms.mParam2, NumValidatorStyle::DEFAULT,
                  Param2.min, Param2.max
               )
               .AddTextBox( {}, wxT(""), 10);

            BindTo(*mParam2T, wxEVT_TEXT, &Validator::OnParam2Text);

            mParam2S = S
               .Name(defaultLabel(3))
               .Style(wxSL_HORIZONTAL)
               .AddSlider( {}, 0, Param2.max, Param2.min);

            BindTo(*mParam2S, wxEVT_SLIDER, &Validator::OnParam2Slider);

            S.AddSpace(20, 0);
            
            // Repeats control
            mRepeatsTxt = S.AddVariableText(defaultLabel(4),
               false, wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT);
            mRepeatsT = S
               .Name(defaultLabel(4))
               .Validator<IntegerValidator<int>>(
                  &ms.mRepeats, NumValidatorStyle::DEFAULT,
                  Repeats.min, Repeats.max
               )
               .AddTextBox( {}, wxT(""), 10);

            BindTo(*mRepeatsT, wxEVT_TEXT, &Validator::OnRepeatsText);

            mRepeatsS = S
               .Name(defaultLabel(4))
               .Style(wxSL_HORIZONTAL)
               .AddSlider( {}, Repeats.def, Repeats.max, Repeats.min);

            BindTo(*mRepeatsS, wxEVT_SLIDER, &Validator::OnRepeatsSlider);

            S.AddSpace(20, 0);
         }
         S.EndMultiColumn();
      }
      S.EndStatic();
   }
   S.EndVerticalLay();

}


bool EffectDistortion::Validator::UpdateUI()
{
   const auto& ms = mSettings;

   if (! GetUIParent()->TransferDataToWindow())
   {
      return false;
   }

   mThresholdS->     SetValue((int) (GetState().mThreshold * Threshold_dB.scale + 0.5));
   mDCBlockCheckBox->SetValue(     ms.mDCBlock);
   mNoiseFloorS->    SetValue((int)ms.mNoiseFloor + 0.5);
   mParam1S->        SetValue((int)ms.mParam1 + 0.5);
   mParam2S->        SetValue((int)ms.mParam2 + 0.5);
   mRepeatsS->       SetValue(     ms.mRepeats);

   GetState().mbSavedFilterState = ms.mDCBlock;

   UpdateUIControls();

   return true;
}


void EffectDistortion::InstanceInit(EffectDistortionState & data, float sampleRate)
{
   const auto& ms = mSettings;

   data.samplerate = sampleRate;
   data.skipcount = 0;
   data.tablechoiceindx = ms.mTableChoiceIndx;
   data.dcblock         = ms.mDCBlock;
   data.threshold       = ms.mThreshold_dB;
   data.noisefloor      = ms.mNoiseFloor;
   data.param1          = ms.mParam1;
   data.param2          = ms.mParam2;
   data.repeats         = ms.mRepeats;

   // DC block filter variables
   data.queuetotal = 0.0;

   //std::queue<float>().swap(data.queuesamples);
   while (!data.queuesamples.empty())
      data.queuesamples.pop();

   MakeTable();

   return;
}

size_t EffectDistortion::InstanceProcess(EffectSettings &settings,
   EffectDistortionState& data,
   const float *const *inBlock, float *const *outBlock, size_t blockLen)
{
   const auto& ms = mSettings;

   const float *ibuf = inBlock[0];
   float *obuf = outBlock[0];

   bool update = (ms.mTableChoiceIndx == data.tablechoiceindx &&
                  ms.mNoiseFloor == data.noisefloor &&
                  ms.mThreshold_dB == data.threshold &&
                  ms.mParam1 == data.param1 &&
                  ms.mParam2 == data.param2 &&
                  ms.mRepeats == data.repeats)? false : true;

   double p1 = ms.mParam1 / 100.0;
   double p2 = ms.mParam2 / 100.0;

   data.tablechoiceindx = ms.mTableChoiceIndx;
   data.threshold = ms.mThreshold_dB;
   data.noisefloor = ms.mNoiseFloor;
   data.param1 = ms.mParam1;
   data.repeats = ms.mRepeats;

   for (decltype(blockLen) i = 0; i < blockLen; i++) {
      if (update && ((data.skipcount++) % skipsamples == 0)) {
         MakeTable();
      }

      switch (ms.mTableChoiceIndx)
      {
      case kHardClip:
         // Param2 = make-up gain.
         obuf[i] = WaveShaper(ibuf[i]) * ((1 - p2) + (mMakeupGain * p2));
         break;
      case kSoftClip:
         // Param2 = make-up gain.
         obuf[i] = WaveShaper(ibuf[i]) * ((1 - p2) + (mMakeupGain * p2));
         break;
      case kHalfSinCurve:
         obuf[i] = WaveShaper(ibuf[i]) * p2;
         break;
      case kExpCurve:
         obuf[i] = WaveShaper(ibuf[i]) * p2;
         break;
      case kLogCurve:
         obuf[i] = WaveShaper(ibuf[i]) * p2;
         break;
      case kCubic:
         obuf[i] = WaveShaper(ibuf[i]) * p2;
         break;
      case kEvenHarmonics:
         obuf[i] = WaveShaper(ibuf[i]);
         break;
      case kSinCurve:
         obuf[i] = WaveShaper(ibuf[i]) * p2;
         break;
      case kLeveller:
         obuf[i] = WaveShaper(ibuf[i]);
         break;
      case kRectifier:
         obuf[i] = WaveShaper(ibuf[i]);
         break;
      case kHardLimiter:
         // Mix equivalent to LADSPA effect's "Wet / Residual" mix
         obuf[i] = (WaveShaper(ibuf[i]) * (p1 - p2)) + (ibuf[i] * p2);
         break;
      default:
         obuf[i] = WaveShaper(ibuf[i]);
      }
      if (ms.mDCBlock) {
         obuf[i] = DCFilter(data, obuf[i]);
      }
   }

   return blockLen;
}

void EffectDistortion::Validator::OnTypeChoice(wxCommandEvent& /*evt*/)
{
   mTypeChoiceCtrl->GetValidator()->TransferFromWindow();

   UpdateUIControls();

   ValidateUI();
}

void EffectDistortion::Validator::OnDCBlockCheckbox(wxCommandEvent& /*evt*/)
{
   auto& ms = mSettings;

   ms.mDCBlock = mDCBlockCheckBox->GetValue();

   GetState().mbSavedFilterState = ms.mDCBlock;

   ValidateUI();
}


void EffectDistortion::Validator::OnThresholdText(wxCommandEvent& /*evt*/)
{
   const auto& ms = mSettings;


   mThresholdT->GetValidator()->TransferFromWindow();
   GetState().mThreshold = DB_TO_LINEAR(ms.mThreshold_dB);
   mThresholdS->SetValue((int) (GetState().mThreshold * Threshold_dB.scale + 0.5));

   ValidateUI();
}

void EffectDistortion::Validator::OnThresholdSlider(wxCommandEvent& evt)
{
   auto& ms = mSettings;

   static const double MIN_Threshold_Linear = DB_TO_LINEAR(Threshold_dB.min);

   GetState().mThreshold = (double)evt.GetInt() / Threshold_dB.scale;
   ms.mThreshold_dB = wxMax(LINEAR_TO_DB(GetState().mThreshold), Threshold_dB.min);
   GetState().mThreshold = std::max(MIN_Threshold_Linear, GetState().mThreshold);
   mThresholdT->GetValidator()->TransferToWindow();

   ValidateUI();
}

void EffectDistortion::Validator::OnNoiseFloorText(wxCommandEvent& /*evt*/)
{
   const auto& ms = mSettings;

   mNoiseFloorT->GetValidator()->TransferFromWindow();
   mNoiseFloorS->SetValue((int) floor(ms.mNoiseFloor + 0.5));

   ValidateUI();
}

void EffectDistortion::Validator::OnNoiseFloorSlider(wxCommandEvent& evt)
{
   auto& ms = mSettings;

   ms.mNoiseFloor = (double) evt.GetInt();
   mNoiseFloorT->GetValidator()->TransferToWindow();

   ValidateUI();
}


void EffectDistortion::Validator::OnParam1Text(wxCommandEvent& /*evt*/)
{
   const auto& ms = mSettings;

   mParam1T->GetValidator()->TransferFromWindow();
   mParam1S->SetValue((int) floor(ms.mParam1 + 0.5));

   ValidateUI();
}

void EffectDistortion::Validator::OnParam1Slider(wxCommandEvent& evt)
{
   auto& ms = mSettings;

   ms.mParam1 = (double) evt.GetInt();
   mParam1T->GetValidator()->TransferToWindow();

   ValidateUI();
}

void EffectDistortion::Validator::OnParam2Text(wxCommandEvent& /*evt*/)
{
   const auto& ms = mSettings;

   mParam2T->GetValidator()->TransferFromWindow();
   mParam2S->SetValue((int) floor(ms.mParam2 + 0.5));

   ValidateUI();
}

void EffectDistortion::Validator::OnParam2Slider(wxCommandEvent& evt)
{
   auto& ms = mSettings;

   ms.mParam2 = (double) evt.GetInt();
   mParam2T->GetValidator()->TransferToWindow();

   ValidateUI();
}

void EffectDistortion::Validator::OnRepeatsText(wxCommandEvent& /*evt*/)
{
   const auto& ms = mSettings;

   mRepeatsT->GetValidator()->TransferFromWindow();
   mRepeatsS->SetValue(ms.mRepeats);

   ValidateUI();
}

void EffectDistortion::Validator::OnRepeatsSlider(wxCommandEvent& evt)
{
   auto& ms = mSettings;

   ms.mRepeats = evt.GetInt();
   mRepeatsT->GetValidator()->TransferToWindow();

   ValidateUI();
}

void EffectDistortion::Validator::UpdateUIControls()
{
   const auto& ms = mSettings;

   // set control text and names to match distortion type
   switch (ms.mTableChoiceIndx)
      {
      case kHardClip:
         UpdateControlText(mThresholdT, mOldThresholdTxt, true);
         UpdateControlText(mNoiseFloorT, mOldmNoiseFloorTxt, false);
         UpdateControlText(mParam1T, mOldParam1Txt, true);
         UpdateControlText(mParam2T, mOldParam2Txt, true);
         UpdateControlText(mRepeatsT, mOldRepeatsTxt, false);

         UpdateControl(ID_Threshold, true, XO("Clipping level"));
         UpdateControl(ID_NoiseFloor, false, defaultLabel(1));
         UpdateControl(ID_Param1, true, XO("Drive"));
         UpdateControl(ID_Param2, true, XO("Make-up Gain"));
         UpdateControl(ID_Repeats, false, defaultLabel(4));
         UpdateControl(ID_DCBlock, false, {});
         break;
      case kSoftClip:
         UpdateControlText(mThresholdT, mOldThresholdTxt, true);
         UpdateControlText(mNoiseFloorT, mOldmNoiseFloorTxt, false);
         UpdateControlText(mParam1T, mOldParam1Txt, true);
         UpdateControlText(mParam2T, mOldParam2Txt, true);
         UpdateControlText(mRepeatsT, mOldRepeatsTxt, false);

         UpdateControl(ID_Threshold, true, XO("Clipping threshold"));
         UpdateControl(ID_NoiseFloor, false, defaultLabel(1));
         UpdateControl(ID_Param1, true, XO("Hardness"));
         UpdateControl(ID_Param2, true, XO("Make-up Gain"));
         UpdateControl(ID_Repeats, false, defaultLabel(4));
         UpdateControl(ID_DCBlock, false, {});
         break;
      case kHalfSinCurve:
         UpdateControlText(mThresholdT, mOldThresholdTxt, false);
         UpdateControlText(mNoiseFloorT, mOldmNoiseFloorTxt, false);
         UpdateControlText(mParam1T, mOldParam1Txt, true);
         UpdateControlText(mParam2T, mOldParam2Txt, true);
         UpdateControlText(mRepeatsT, mOldRepeatsTxt, false);

         UpdateControl(ID_Threshold, false, defaultLabel(0));
         UpdateControl(ID_NoiseFloor, false, defaultLabel(1));
         UpdateControl(ID_Param1, true, XO("Distortion amount"));
         UpdateControl(ID_Param2, true, XO("Output level"));
         UpdateControl(ID_Repeats, false, defaultLabel(4));
         UpdateControl(ID_DCBlock, false, {});
         break;
      case kExpCurve:
         UpdateControlText(mThresholdT, mOldThresholdTxt, false);
         UpdateControlText(mNoiseFloorT, mOldmNoiseFloorTxt, false);
         UpdateControlText(mParam1T, mOldParam1Txt, true);
         UpdateControlText(mParam2T, mOldParam2Txt, true);
         UpdateControlText(mRepeatsT, mOldRepeatsTxt, false);

         UpdateControl(ID_Threshold, false, defaultLabel(0));
         UpdateControl(ID_NoiseFloor, false, defaultLabel(1));
         UpdateControl(ID_Param1, true, XO("Distortion amount"));
         UpdateControl(ID_Param2, true, XO("Output level"));
         UpdateControl(ID_Repeats, false, defaultLabel(4));
         UpdateControl(ID_DCBlock, false, {});
         break;
      case kLogCurve:
         UpdateControlText(mThresholdT, mOldThresholdTxt, false);
         UpdateControlText(mNoiseFloorT, mOldmNoiseFloorTxt, false);
         UpdateControlText(mParam1T, mOldParam1Txt, true);
         UpdateControlText(mParam2T, mOldParam2Txt, true);
         UpdateControlText(mRepeatsT, mOldRepeatsTxt, false);

         UpdateControl(ID_Threshold, false, defaultLabel(0));
         UpdateControl(ID_NoiseFloor, false, defaultLabel(1));
         UpdateControl(ID_Param1, true, XO("Distortion amount"));
         UpdateControl(ID_Param2, true, XO("Output level"));
         UpdateControl(ID_Repeats, false, defaultLabel(4));
         UpdateControl(ID_DCBlock, false, {});
         break;
      case kCubic:
         UpdateControlText(mThresholdT, mOldThresholdTxt, false);
         UpdateControlText(mNoiseFloorT, mOldmNoiseFloorTxt, false);
         UpdateControlText(mParam1T, mOldParam1Txt, true);
         UpdateControlText(mParam2T, mOldParam2Txt, true);
         UpdateControlText(mRepeatsT, mOldRepeatsTxt, true);

         UpdateControl(ID_Threshold, false, defaultLabel(0));
         UpdateControl(ID_NoiseFloor, false, defaultLabel(1));
         UpdateControl(ID_Param1, true, XO("Distortion amount"));
         UpdateControl(ID_Param2, true, XO("Output level"));
         UpdateControl(ID_Repeats, true, XO("Repeat processing"));
         UpdateControl(ID_DCBlock, false, {});
         break;
      case kEvenHarmonics:
         UpdateControlText(mThresholdT, mOldThresholdTxt, false);
         UpdateControlText(mNoiseFloorT, mOldmNoiseFloorTxt, false);
         UpdateControlText(mParam1T, mOldParam1Txt, true);
         UpdateControlText(mParam2T, mOldParam2Txt, true);
         UpdateControlText(mRepeatsT, mOldRepeatsTxt, false);

         UpdateControl(ID_Threshold, false, defaultLabel(0));
         UpdateControl(ID_NoiseFloor, false, defaultLabel(1));
         UpdateControl(ID_Param1, true, XO("Distortion amount"));
         UpdateControl(ID_Param2, true, XO("Harmonic brightness"));
         UpdateControl(ID_Repeats, false, defaultLabel(4));
         UpdateControl(ID_DCBlock, true, {});
         break;
      case kSinCurve:
         UpdateControlText(mThresholdT, mOldThresholdTxt, false);
         UpdateControlText(mNoiseFloorT, mOldmNoiseFloorTxt, false);
         UpdateControlText(mParam1T, mOldParam1Txt, true);
         UpdateControlText(mParam2T, mOldParam2Txt, true);
         UpdateControlText(mRepeatsT, mOldRepeatsTxt, false);

         UpdateControl(ID_Threshold, false, defaultLabel(0));
         UpdateControl(ID_NoiseFloor, false, defaultLabel(1));
         UpdateControl(ID_Param1, true, XO("Distortion amount"));
         UpdateControl(ID_Param2, true, XO("Output level"));
         UpdateControl(ID_Repeats, false, defaultLabel(4));
         UpdateControl(ID_DCBlock, false, {});
         break;
      case kLeveller:
         UpdateControlText(mThresholdT, mOldThresholdTxt, false);
         UpdateControlText(mNoiseFloorT, mOldmNoiseFloorTxt, true);
         UpdateControlText(mParam1T, mOldParam1Txt, true);
         UpdateControlText(mParam2T, mOldParam2Txt, false);
         UpdateControlText(mRepeatsT, mOldRepeatsTxt, true);

         UpdateControl(ID_Threshold, false, defaultLabel(0));
         UpdateControl(ID_NoiseFloor, true, defaultLabel(1));
         UpdateControl(ID_Param1, true, XO("Levelling fine adjustment"));
         UpdateControl(ID_Param2, false, defaultLabel(3));
         UpdateControl(ID_Repeats, true, XO("Degree of Levelling"));
         UpdateControl(ID_DCBlock, false, {});
         break;
      case kRectifier:
         UpdateControlText(mThresholdT, mOldThresholdTxt, false);
         UpdateControlText(mNoiseFloorT, mOldmNoiseFloorTxt, false);
         UpdateControlText(mParam1T, mOldParam1Txt, true);
         UpdateControlText(mParam2T, mOldParam2Txt, false);
         UpdateControlText(mRepeatsT, mOldRepeatsTxt, false);

         UpdateControl(ID_Threshold, false, defaultLabel(0));
         UpdateControl(ID_NoiseFloor, false, defaultLabel(1));
         UpdateControl(ID_Param1, true, XO("Distortion amount"));
         UpdateControl(ID_Param2, false, defaultLabel(3));
         UpdateControl(ID_Repeats, false, defaultLabel(4));
         UpdateControl(ID_DCBlock, true, {});
         break;
      case kHardLimiter:
         UpdateControlText(mThresholdT, mOldThresholdTxt, true);
         UpdateControlText(mNoiseFloorT, mOldmNoiseFloorTxt, false);
         UpdateControlText(mParam1T, mOldParam1Txt, true);
         UpdateControlText(mParam2T, mOldParam2Txt, true);
         UpdateControlText(mRepeatsT, mOldRepeatsTxt, false);

         UpdateControl(ID_Threshold, true, XO("dB Limit"));
         UpdateControl(ID_NoiseFloor, false, defaultLabel(1));
         UpdateControl(ID_Param1, true, XO("Wet level"));
         UpdateControl(ID_Param2, true, XO("Residual level"));
         UpdateControl(ID_Repeats, false, defaultLabel(4));
         UpdateControl(ID_DCBlock, false, {});
         break;
      default:
         UpdateControl(ID_Threshold,   true, defaultLabel(0));
         UpdateControl(ID_NoiseFloor,  true, defaultLabel(1));
         UpdateControl(ID_Param1,      true, defaultLabel(2));
         UpdateControl(ID_Param2,      true, defaultLabel(3));
         UpdateControl(ID_Repeats,     true, defaultLabel(4));
         UpdateControl(ID_DCBlock,     false, {});
   }
}


void EffectDistortion::Validator::UpdateControl(
   control id, bool enabled, TranslatableString name)
{
   auto& ms = mSettings;

   auto suffix = XO("(Not Used):");
   switch (id)
   {
      case ID_Threshold: {
         /* i18n-hint: Control range. */
         if (enabled) suffix = XO("(-100 to 0 dB):");
         name.Join( suffix, wxT(" ") );

         // Logarithmic slider is set indirectly
         GetState().mThreshold = DB_TO_LINEAR(ms.mThreshold_dB);
         mThresholdS->SetValue((int) (GetState().mThreshold * Threshold_dB.scale + 0.5));

         auto translated = name.Translation();
         mThresholdTxt->SetLabel(translated);
         mThresholdS->SetName(translated);
         mThresholdT->SetName(translated);
         mThresholdS->Enable(enabled);
         mThresholdT->Enable(enabled);
         break;
      }
      case ID_NoiseFloor: {
         /* i18n-hint: Control range. */
         if (enabled) suffix = XO("(-80 to -20 dB):");
         name.Join( suffix, wxT(" ") );

         auto translated = name.Translation();
         mNoiseFloorTxt->SetLabel(translated);
         mNoiseFloorS->SetName(translated);
         mNoiseFloorT->SetName(translated);
         mNoiseFloorS->Enable(enabled);
         mNoiseFloorT->Enable(enabled);
         break;
      }
      case ID_Param1: {
         /* i18n-hint: Control range. */
         if (enabled) suffix = XO("(0 to 100):");
         name.Join( suffix, wxT(" ") );

         auto translated = name.Translation();
         mParam1Txt->SetLabel(translated);
         mParam1S->SetName(translated);
         mParam1T->SetName(translated);
         mParam1S->Enable(enabled);
         mParam1T->Enable(enabled);
         break;
      }
      case ID_Param2: {
         /* i18n-hint: Control range. */
         if (enabled) suffix = XO("(0 to 100):");
         name.Join( suffix, wxT(" ") );

         auto translated = name.Translation();
         mParam2Txt->SetLabel(translated);
         mParam2S->SetName(translated);
         mParam2T->SetName(translated);
         mParam2S->Enable(enabled);
         mParam2T->Enable(enabled);
         break;
      }
      case ID_Repeats: {
         /* i18n-hint: Control range. */
         if (enabled) suffix = XO("(0 to 5):");
         name.Join( suffix, wxT(" ") );

         auto translated = name.Translation();
         mRepeatsTxt->SetLabel(translated);
         mRepeatsS->SetName(translated);
         mRepeatsT->SetName(translated);
         mRepeatsS->Enable(enabled);
         mRepeatsT->Enable(enabled);
         break;
      }
      case ID_DCBlock: {
         if (enabled) {
            mDCBlockCheckBox->SetValue(GetState().mbSavedFilterState);
            ms.mDCBlock = GetState().mbSavedFilterState;
         }
         else {
            mDCBlockCheckBox->SetValue(false);
            ms.mDCBlock = false;
         }

         mDCBlockCheckBox->Enable(enabled);
         break;
      }
      default:
         break;
   }
}

void EffectDistortion::Validator::UpdateControlText(wxTextCtrl* textCtrl, wxString& string, bool enabled)
{
   if (enabled) {
      if (textCtrl->GetValue().empty())
         textCtrl->SetValue(string);
      else
         string = textCtrl->GetValue();
   }
   else {
      if (!textCtrl->GetValue().empty())
         string = textCtrl->GetValue();
      textCtrl->SetValue(wxT(""));
   }
}

void EffectDistortion::MakeTable()
{
   const auto& ms = mSettings;

   switch (ms.mTableChoiceIndx)
   {
      case kHardClip:
         HardClip();
         break;
      case kSoftClip:
         SoftClip();
         break;
      case kHalfSinCurve:
         HalfSinTable();
         break;
      case kExpCurve:
         ExponentialTable();
         break;
      case kLogCurve:
         LogarithmicTable();
         break;
      case kCubic:
         CubicTable();
         break;
      case kEvenHarmonics:
         EvenHarmonicTable();
         break;
      case kSinCurve:
         SineTable();
         break;
      case kLeveller:
         Leveller();
         break;
      case kRectifier:
         Rectifier();
         break;
      case kHardLimiter:
         HardLimiter();
         break;
   }
}


//
// Preset tables for gain lookup
//

void EffectDistortion::HardClip()
{
   double lowThresh = 1 - mMaster.mThreshold;
   double highThresh = 1 + mMaster.mThreshold;

   for (int n = 0; n < TABLESIZE; n++) {
      if (n < (STEPS * lowThresh))
         mTable[n] = - mMaster.mThreshold;
      else if (n > (STEPS * highThresh))
         mTable[n] = mMaster.mThreshold;
      else
         mTable[n] = n/(double)STEPS - 1;

      mMakeupGain = 1.0 / mMaster.mThreshold;
   }
}

void EffectDistortion::SoftClip()
{
   const auto& ms = mSettings;

   double threshold = 1 + mMaster.mThreshold;
   double amount = std::pow(2.0, 7.0 * ms.mParam1 / 100.0); // range 1 to 128
   double peak = LogCurve(mMaster.mThreshold, 1.0, amount);
   mMakeupGain = 1.0 / peak;
   mTable[STEPS] = 0.0;   // origin

   // positive half of table
   for (int n = STEPS; n < TABLESIZE; n++) {
      if (n < (STEPS * threshold)) // origin to threshold
         mTable[n] = n/(float)STEPS - 1;
      else
         mTable[n] = LogCurve(mMaster.mThreshold, n/(double)STEPS - 1, amount);
   }
   CopyHalfTable();
}

float EffectDistortion::LogCurve(double threshold, float value, double ratio)
{
   return threshold + ((std::exp(ratio * (threshold - value)) - 1) / -ratio);
}

void EffectDistortion::ExponentialTable()
{
   const auto& ms = mSettings;

   double amount = std::min(0.999, DB_TO_LINEAR(-1 * ms.mParam1));   // avoid divide by zero

   for (int n = STEPS; n < TABLESIZE; n++) {
      double linVal = n/(float)STEPS;
      double scale = -1.0 / (1.0 - amount);   // unity gain at 0dB
      double curve = std::exp((linVal - 1) * std::log(amount));
      mTable[n] = scale * (curve -1);
   }
   CopyHalfTable();
}

void EffectDistortion::LogarithmicTable()
{
   const auto& ms = mSettings;

   double amount = ms.mParam1;
   double stepsize = 1.0 / STEPS;
   double linVal = 0;

   if (amount == 0){
      for (int n = STEPS; n < TABLESIZE; n++) {
      mTable[n] = linVal;
      linVal += stepsize;
      }
   }
   else {
      for (int n = STEPS; n < TABLESIZE; n++) {
         mTable[n] = std::log(1 + (amount * linVal)) / std::log(1 + amount);
         linVal += stepsize;
      }
   }
   CopyHalfTable();
}

void EffectDistortion::HalfSinTable()
{
   const auto& ms = mSettings;

   int iter = std::floor(ms.mParam1 / 20.0);
   double fractionalpart = (ms.mParam1 / 20.0) - iter;
   double stepsize = 1.0 / STEPS;
   double linVal = 0;

   for (int n = STEPS; n < TABLESIZE; n++) {
      mTable[n] = linVal;
      for (int i = 0; i < iter; i++) {
         mTable[n] = std::sin(mTable[n] * M_PI_2);
      }
      mTable[n] += ((std::sin(mTable[n] * M_PI_2) - mTable[n]) * fractionalpart);
      linVal += stepsize;
   }
   CopyHalfTable();
}

void EffectDistortion::CubicTable()
{
   const auto& ms = mSettings;

   double amount = ms.mParam1 * std::sqrt(3.0) / 100.0;
   double gain = 1.0;
   if (amount != 0.0)
      gain = 1.0 / Cubic(std::min(amount, 1.0));

   double stepsize = amount / STEPS;
   double x = -amount;
   
   if (amount == 0) {
      for (int i = 0; i < TABLESIZE; i++) {
         mTable[i] = (i / (double)STEPS) - 1.0;
      }
   }
   else {
      for (int i = 0; i < TABLESIZE; i++) {
         mTable[i] = gain * Cubic(x);
         for (int j = 0; j < ms.mRepeats; j++) {
            mTable[i] = gain * Cubic(mTable[i] * amount);
         }
         x += stepsize;
      }
   }
}

double EffectDistortion::Cubic(double x)
{
   const auto& ms = mSettings;

   if (ms.mParam1 == 0.0)
      return x;

   return x - (std::pow(x, 3.0) / 3.0);
}


void EffectDistortion::EvenHarmonicTable()
{
   const auto& ms = mSettings;

   double amount = ms.mParam1 / -100.0;
   // double C = std::sin(std::max(0.001, mParams.mParam2) / 100.0) * 10.0;
   double C = std::max(0.001, ms.mParam2) / 10.0;

   double step = 1.0 / STEPS;
   double xval = -1.0;

   for (int i = 0; i < TABLESIZE; i++) {
      mTable[i] = ((1 + amount) * xval) -
                  (xval * (amount / std::tanh(C)) * std::tanh(C * xval));
      xval += step;
   }
}

void EffectDistortion::SineTable()
{
   const auto& ms = mSettings;

   int iter = std::floor(ms.mParam1 / 20.0);
   double fractionalpart = (ms.mParam1 / 20.0) - iter;
   double stepsize = 1.0 / STEPS;
   double linVal = 0.0;

   for (int n = STEPS; n < TABLESIZE; n++) {
      mTable[n] = linVal;
      for (int i = 0; i < iter; i++) {
         mTable[n] = (1.0 + std::sin((mTable[n] * M_PI) - M_PI_2)) / 2.0;
      }
      mTable[n] += (((1.0 + std::sin((mTable[n] * M_PI) - M_PI_2)) / 2.0) - mTable[n]) * fractionalpart;
      linVal += stepsize;
   }
   CopyHalfTable();
}

void EffectDistortion::Leveller()
{
   const auto& ms = mSettings;

   double noiseFloor = DB_TO_LINEAR(ms.mNoiseFloor);
   int numPasses = ms.mRepeats;
   double fractionalPass = ms.mParam1 / 100.0;

   const int numPoints = 6;
   const double gainFactors[numPoints] = { 0.80, 1.00, 1.20, 1.20, 1.00, 0.80 };
   double gainLimits[numPoints] = { 0.0001, 0.0, 0.1, 0.3, 0.5, 1.0 };
   double addOnValues[numPoints];

   gainLimits[1] = noiseFloor;
   /* In the original Leveller effect, behaviour was undefined for threshold > 20 dB.
    * If we want to support > 20 dB we need to scale the points to keep them non-decreasing.
    * 
    * if (noiseFloor > gainLimits[2]) {
    *    for (int i = 3; i < numPoints; i++) {
    *    gainLimits[i] = noiseFloor + ((1 - noiseFloor)*((gainLimits[i] - 0.1) / 0.9));
    * }
    * gainLimits[2] = noiseFloor;
    * }
    */

   // Calculate add-on values
   addOnValues[0] = 0.0;
   for (int i = 0; i < numPoints-1; i++) {
      addOnValues[i+1] = addOnValues[i] + (gainLimits[i] * (gainFactors[i] - gainFactors[1 + i]));
   }

   // Positive half of table.
   // The original effect increased the 'strength' of the effect by
   // repeated passes over the audio data.
   // Here we model that more efficiently by repeated passes over a linear table.
   for (int n = STEPS; n < TABLESIZE; n++) {
      mTable[n] = ((double) (n - STEPS) / (double) STEPS);
      for (int j = 0; j < numPasses; j++) {
         // Find the highest index for gain adjustment
         int index = numPoints - 1;
         for (int i = index; i >= 0 && mTable[n] < gainLimits[i]; i--) {
            index = i;
         }
         // the whole number of 'repeats'
         mTable[n] = (mTable[n] * gainFactors[index]) + addOnValues[index];
      }
      // Extrapolate for fine adjustment.
      // tiny fractions are not worth the processing time
      if (fractionalPass > 0.001) {
         int index = numPoints - 1;
         for (int i = index; i >= 0 && mTable[n] < gainLimits[i]; i--) {
            index = i;
         }
         mTable[n] += fractionalPass * ((mTable[n] * (gainFactors[index] - 1)) + addOnValues[index]);
      }
   }
   CopyHalfTable();
}

void EffectDistortion::Rectifier()
{
   const auto& ms = mSettings;

   double amount = (ms.mParam1 / 50.0) - 1;
   double stepsize = 1.0 / STEPS;
   int index = STEPS;

   // positive half of waveform is passed unaltered.
   for  (int n = 0; n <= STEPS; n++) {
      mTable[index] = n * stepsize;
      index += 1;
   }

   // negative half of table
   index = STEPS - 1;
   for (int n = 1; n <= STEPS; n++) {
      mTable[index] = n * stepsize * amount;
      index--;
   }
}

void EffectDistortion::HardLimiter()
{
   // The LADSPA "hardLimiter 1413" is basically hard clipping,
   // but with a 'kind of' wet/dry mix:
   // out = ((wet-residual)*clipped) + (residual*in)
   HardClip();
}


// Helper functions for lookup tables

void EffectDistortion::CopyHalfTable()
{
   // Copy negative half of table from positive half
   int count = TABLESIZE - 1;
   for (int n = 0; n < STEPS; n++) {
      mTable[n] = -mTable[count];
      count--;
   }
}


float EffectDistortion::WaveShaper(float sample)
{
   float out;
   int index;
   double xOffset;
   double amount = 1;

   const auto& ms = mSettings;

   switch (ms.mTableChoiceIndx)
   {
      // Do any pre-processing here
      case kHardClip:
         // Pre-gain
         amount = ms.mParam1 / 100.0;
         sample *= 1+amount;
         break;
      default: break;
   }

   index = std::floor(sample * STEPS) + STEPS;
   index = wxMax<int>(wxMin<int>(index, 2 * STEPS - 1), 0);
   xOffset = ((1 + sample) * STEPS) - index;
   xOffset = wxMin<double>(wxMax<double>(xOffset, 0.0), 1.0);   // Clip at 0dB

   // linear interpolation: y = y0 + (y1-y0)*(x-x0)
   out = mTable[index] + (mTable[index + 1] - mTable[index]) * xOffset;

   return out;
}


float EffectDistortion::DCFilter(EffectDistortionState& data, float sample)
{
   // Rolling average gives less offset at the start than an IIR filter.
   const unsigned int queueLength = std::floor(data.samplerate / 20.0);

   data.queuetotal += sample;
   data.queuesamples.push(sample);

   if (data.queuesamples.size() > queueLength) {
      data.queuetotal -= data.queuesamples.front();
      data.queuesamples.pop();
   }

   return sample - (data.queuetotal / data.queuesamples.size());
}
