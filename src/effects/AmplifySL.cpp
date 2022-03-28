/**********************************************************************

  Audacity: A Digital Audio Editor

  Amplify.cpp

  Dominic Mazzoni
  Vaughan Johnson (Preview)

*******************************************************************//**

\class EffectAmplify
\brief An Effect that makes a sound louder or softer.

  This rewritten class supports a smart Amplify effect - it calculates
  the maximum amount of gain that can be applied to all tracks without
  causing clipping and selects this as the default parameter.

*//*******************************************************************/


#include "AmplifySL.h"
#include "LoadEffects.h"

#include <math.h>
#include <float.h>

#include <wx/button.h>
#include <wx/checkbox.h>
#include <wx/intl.h>
#include <wx/sizer.h>
#include <wx/slider.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/valtext.h>
#include <wx/log.h>

#include "../ShuttleGui.h"
#include "../WaveTrack.h"
#include "../widgets/valnum.h"


enum
{
   ID_Amp = 10000,
   ID_Peak,
   ID_Clip
};

const EffectParameterMethods& EffectAmplifySL::Parameters() const
{
   static CapturedParameters<EffectAmplifySL,
      // Interactive case
      Ratio, Clipping
   > parameters;

   static CapturedParameters<EffectAmplifySL,
      Ratio
   > batchParameters{
      // If invoking Amplify from a macro, mCanClip is not a parameter
      // but is always true
      [](EffectAmplifySL&, EffectSettings &, Settings& settings, bool) {
         settings.mCanClip = true;
         return true;
      },
   };

   // Parameters differ depending on batch mode.  Option to disable clipping
   // is interactive only.
   if (IsBatchProcessing())
      return parameters;
   else
      return batchParameters;
}

//
// EffectAmplify
//

const ComponentInterfaceSymbol EffectAmplifySL::Symbol
{ XO("AmplifySL_") };

namespace{ BuiltinEffectsModule::Registration< EffectAmplifySL > reg; }

BEGIN_EVENT_TABLE(EffectAmplifySL, wxEvtHandler)
   EVT_SLIDER(ID_Amp, EffectAmplifySL::OnAmpSlider)
   EVT_TEXT(ID_Amp, EffectAmplifySL::OnAmpText)
   EVT_TEXT(ID_Peak, EffectAmplifySL::OnPeakText)
   EVT_CHECKBOX(ID_Clip, EffectAmplifySL::OnClipCheckBox)
END_EVENT_TABLE()

EffectAmplifySL::EffectAmplifySL()
{
   mSettings.mAmp = Amp.def;
   // Ratio.def == DB_TO_LINEAR(Amp.def)
   Parameters().Reset(*this);
   mState.mRatioClip = 0.0;
   mState.mPeak = 0.0;

   SetLinearEffectFlag(true);
}

EffectAmplifySL::~EffectAmplifySL()
{
}

// ComponentInterface implementation

ComponentInterfaceSymbol EffectAmplifySL::GetSymbol() const
{
   return Symbol;
}

TranslatableString EffectAmplifySL::GetDescription() const
{
   // Note: This is useful only after ratio has been set.
   return XO("Increases or decreases the volume of the audio you have selected");
}

ManualPageID EffectAmplifySL::ManualPage() const
{
   return L"Amplify";
}

// EffectDefinitionInterface implementation

EffectType EffectAmplifySL::GetType() const
{
   return EffectTypeProcess;
}

// EffectProcessor implementation

unsigned EffectAmplifySL::GetAudioInCount() const
{
   return 1;
}

unsigned EffectAmplifySL::GetAudioOutCount() const
{
   return 1;
}

size_t EffectAmplifySL::ProcessBlock(EffectSettings &,
   const float *const *inBlock, float *const *outBlock, size_t blockLen)
{
   for (decltype(blockLen) i = 0; i < blockLen; i++)
   {
      outBlock[0][i] = inBlock[0][i] * mSettings.mRatio;
   }

   return blockLen;
}

bool EffectAmplifySL::LoadFactoryDefaults(EffectSettings &) const
{
   // To do: externalize state so const_cast isn't needed
   return const_cast<EffectAmplifySL&>(*this).DoLoadFactoryDefaults();
}

bool EffectAmplifySL::DoLoadFactoryDefaults()
{
   Init();

   mState.mRatioClip = 0.0;
   if (mState.mPeak > 0.0)
   {
      mSettings.mRatio = 1.0 / mState.mPeak;
      mState.mRatioClip = mSettings.mRatio;
   }
   else
   {
      mSettings.mRatio = 1.0;
   }
   mSettings.mCanClip = false;

   ClampRatio();
   return true;
}

// Effect implementation

bool EffectAmplifySL::Init()
{
   mState.mPeak = 0.0;

   for (auto t : inputTracks()->Selected< const WaveTrack >())
   {
      auto pair = t->GetMinMax(mT0, mT1); // may throw
      const float min = pair.first, max = pair.second;
      float newpeak = (fabs(min) > fabs(max) ? fabs(min) : fabs(max));

      if (newpeak > mState.mPeak)
      {
         mState.mPeak = newpeak;
      }
   }

   return true;
}

void EffectAmplifySL::Preview(EffectSettingsAccess &access, bool dryOnly)
{
   auto cleanup1 = valueRestorer(mSettings.mRatio );
   auto cleanup2 = valueRestorer(mState.mPeak );

   Effect::Preview(access, dryOnly);
}

// Event handler object
struct EffectAmplifySL::Validator
   : DefaultEffectUIValidator
{
   Validator(Effect&                    effect,
             EffectSettingsAccess&      access,
             EffectAmplifySL::Settings& settings,
             EffectAmplifySL::State&    state
            )

      : DefaultEffectUIValidator{ effect, access }
      , mActualEffect{ effect }
      , mSettings{ settings }
      , mState{ state }
   {}
   virtual ~Validator() = default;

   
   Effect& GetEffect() const { return static_cast<Effect&>(mEffect); }

   bool ValidateUI() override;
   bool UpdateUI() override;
   void DoUpdateUI();
      
   void PopulateOrExchange(ShuttleGui& S,
      const EffectSettings& settings, bool batch);

   void OnAmpSlider(wxCommandEvent& evt);
   void OnAmpText(wxCommandEvent& evt);
   void OnPeakText(wxCommandEvent& evt);
   void OnClipCheckBox(wxCommandEvent& evt);

   void CheckClip()
   {
      // TO SOLVE: Effect::EnableApply is protected
      // 
      //mActualEffect.EnableApply(mClip->GetValue() || (mState.mPeak > 0.0 && mSettings.mRatio <= mState.mRatioClip));
   }

   void EnableApply(bool a)
   {
      // TO SOLVE: Effect::EnableApply is protected
      // 
      //mActualEffect.EnableApply(a)
   }


   Effect& mActualEffect;
   EffectAmplifySL::Settings& mSettings;
   EffectAmplifySL::State& mState;

   wxSlider*   mAmpS;
   wxTextCtrl* mAmpT;
   wxTextCtrl* mNewPeakT;
   wxCheckBox* mClip;
};


void EffectAmplifySL::Validator::PopulateOrExchange(ShuttleGui& S,
   const EffectSettings& settings, bool batch)
{
   enum { precision = 3 }; // allow (a generous) 3 decimal  places for Amplification (dB)

   if (batch)
   {
      mSettings.mCanClip = true;
      mState.mPeak = 1.0;
   }
   else
   {
      if (mState.mPeak > 0.0)
      {
         mSettings.mRatio = 1.0 / mState.mPeak;
         mState.mRatioClip = mSettings.mRatio;
      }
      else
      {
         mSettings.mRatio = 1.0;
      }
   }

   S.AddSpace(0, 5);

   S.StartVerticalLay(0);
   {
      // Amplitude
      S.StartMultiColumn(2, wxCENTER);
      {
         mAmpT = S.Id(ID_Amp)
            .Validator<FloatingPointValidator<double>>(
               precision, &mSettings.mAmp, NumValidatorStyle::ONE_TRAILING_ZERO, Amp.min, Amp.max)
            .AddTextBox(XXO("&Amplification (dB):"), L"", 12);
      }
      S.EndMultiColumn();

      // Amplitude
      S.StartHorizontalLay(wxEXPAND);
      {
         mAmpS = S.Id(ID_Amp)
            .Style(wxSL_HORIZONTAL)
            .Name(XO("Amplification dB"))
            .AddSlider({}, 0, Amp.max * Amp.scale, Amp.min * Amp.scale);
      }
      S.EndHorizontalLay();

      // Peak
      S.StartMultiColumn(2, wxCENTER);
      {
         mNewPeakT = S.Id(ID_Peak)
            .Validator<FloatingPointValidator<double>>(
               // One extra decimal place so that rounding is visible to user
               // (see: bug 958)
               precision + 1,
               &(mState.mNewPeak), NumValidatorStyle::ONE_TRAILING_ZERO,
               // min and max need same precision as what we're validating (bug 963)
               RoundValue(precision + 1, Amp.min + LINEAR_TO_DB(mState.mPeak)),
               RoundValue(precision + 1, Amp.max + LINEAR_TO_DB(mState.mPeak)))
            .AddTextBox(XXO("&New Peak Amplitude (dB):"), L"", 12);
      }
      S.EndMultiColumn();

      // Clipping
      S.StartHorizontalLay(wxCENTER);
      {

         mClip = S.Id(ID_Clip).Disable(batch)
            .AddCheckBox(XXO("Allo&w clipping"), false);
      }
      S.EndHorizontalLay();
   }
   S.EndVerticalLay();
}


void EffectAmplifySL::Validator::OnAmpSlider(wxCommandEvent& evt)
{
   double dB = evt.GetInt() / Amp.scale;
   mSettings.mRatio = DB_TO_LINEAR(std::clamp<double>(dB, Amp.min, Amp.max));

   double dB2 = (evt.GetInt() - 1) / Amp.scale;
   double ratio2 = DB_TO_LINEAR(std::clamp<double>(dB2, Amp.min, Amp.max));

   if (!mClip->GetValue() && mSettings.mRatio * mState.mPeak > 1.0 && ratio2 * mState.mPeak < 1.0)
   {
      mSettings.mRatio = 1.0 / mState.mPeak;
   }

   mSettings.mAmp = LINEAR_TO_DB(mSettings.mRatio);
   mAmpT->GetValidator()->TransferToWindow();

   mState.mNewPeak = LINEAR_TO_DB(mSettings.mRatio * mState.mPeak);
   mNewPeakT->GetValidator()->TransferToWindow();

   CheckClip();
}


void EffectAmplifySL::Validator::OnAmpText(wxCommandEvent& evt)
{
   if (!mAmpT->GetValidator()->TransferFromWindow())
   {
      EnableApply(false);
      return;
   }

   mSettings.mRatio = DB_TO_LINEAR(std::clamp<double>(mSettings.mAmp * Amp.scale, Amp.min * Amp.scale, Amp.max * Amp.scale) / Amp.scale);

   mAmpS->SetValue((int)(LINEAR_TO_DB(mSettings.mRatio) * Amp.scale + 0.5));

   mState.mNewPeak = LINEAR_TO_DB(mSettings.mRatio * mState.mPeak);
   mNewPeakT->GetValidator()->TransferToWindow();

   CheckClip();
}


void EffectAmplifySL::Validator::OnPeakText(wxCommandEvent& evt)
{
   if (!mNewPeakT->GetValidator()->TransferFromWindow())
   {
      EnableApply(false);
      return;
   }

   if (mState.mNewPeak == 0.0)
      mSettings.mRatio = mState.mRatioClip;
   else
      mSettings.mRatio = DB_TO_LINEAR(mState.mNewPeak) / mState.mPeak;

   double ampInit = LINEAR_TO_DB(mSettings.mRatio);
   mSettings.mAmp = std::clamp<double>(ampInit, Amp.min, Amp.max);
   if (mSettings.mAmp != ampInit)
      mSettings.mRatio = DB_TO_LINEAR(mSettings.mAmp);

   mAmpT->GetValidator()->TransferToWindow();

   mAmpS->SetValue((int)(mSettings.mAmp * Amp.scale + 0.5f));

   CheckClip();
}


void EffectAmplifySL::Validator::OnClipCheckBox(wxCommandEvent& evt)
{
   CheckClip();
}

std::unique_ptr<EffectUIValidator>
EffectAmplifySL::PopulateOrExchange(ShuttleGui & S, EffectSettingsAccess &)
{
   enum{ precision = 3 }; // allow (a generous) 3 decimal  places for Amplification (dB)

   bool batch = IsBatchProcessing();
   if ( batch )
   {
      mSettings.mCanClip = true;
      mState.mPeak = 1.0;
   }
   else 
   {
      if (mState.mPeak > 0.0)
      {
         mSettings.mRatio = 1.0 / mState.mPeak;
         mState.mRatioClip = mSettings.mRatio;
      }
      else
      {
         mSettings.mRatio = 1.0;
      }
   }

   S.AddSpace(0, 5);

   S.StartVerticalLay(0);
   {
      // Amplitude
      S.StartMultiColumn(2, wxCENTER);
      {
         mAmpT = S.Id(ID_Amp)
            .Validator<FloatingPointValidator<double>>(
               precision, &mSettings.mAmp, NumValidatorStyle::ONE_TRAILING_ZERO, Amp.min, Amp.max )
            .AddTextBox(XXO("&Amplification (dB):"), L"", 12);
      }
      S.EndMultiColumn();

      // Amplitude
      S.StartHorizontalLay(wxEXPAND);
      {
         mAmpS = S.Id(ID_Amp)
            .Style(wxSL_HORIZONTAL)
            .Name(XO("Amplification dB"))
            .AddSlider( {}, 0, Amp.max * Amp.scale, Amp.min * Amp.scale);
      }
      S.EndHorizontalLay();

      // Peak
      S.StartMultiColumn(2, wxCENTER);
      {
         mNewPeakT = S.Id(ID_Peak)
            .Validator<FloatingPointValidator<double>>(
               // One extra decimal place so that rounding is visible to user
               // (see: bug 958)
               precision + 1,
               &(mState.mNewPeak), NumValidatorStyle::ONE_TRAILING_ZERO,
               // min and max need same precision as what we're validating (bug 963)
               RoundValue( precision + 1, Amp.min + LINEAR_TO_DB(mState.mPeak) ),
               RoundValue( precision + 1, Amp.max + LINEAR_TO_DB(mState.mPeak) ) )
            .AddTextBox(XXO("&New Peak Amplitude (dB):"), L"", 12);
      }
      S.EndMultiColumn();

      // Clipping
      S.StartHorizontalLay(wxCENTER);
      {

         mClip = S.Id(ID_Clip).Disable( batch )
            .AddCheckBox(XXO("Allo&w clipping"), false);
      }
      S.EndHorizontalLay();
   }
   S.EndVerticalLay();

   return nullptr;
}

void EffectAmplifySL::ClampRatio()
{
   // limit range of gain
   double dBInit = LINEAR_TO_DB(mSettings.mRatio);
   double dB = std::clamp<double>(dBInit, Amp.min, Amp.max);
   if (dB != dBInit)
      mSettings.mRatio = DB_TO_LINEAR(dB);

   mSettings.mAmp  = LINEAR_TO_DB(mSettings.mRatio);
   mState.mNewPeak = LINEAR_TO_DB(mSettings.mRatio * mState.mPeak);
}

bool EffectAmplifySL::TransferDataToWindow(const EffectSettings &)
{
   mAmpT->GetValidator()->TransferToWindow();

   mAmpS->SetValue((int) (mSettings.mAmp * Amp.scale + 0.5f));

   mNewPeakT->GetValidator()->TransferToWindow();

   mClip->SetValue(mSettings.mCanClip);

   CheckClip();

   return true;
}

bool EffectAmplifySL::TransferDataFromWindow(EffectSettings &)
{
   mSettings.mRatio = DB_TO_LINEAR(std::clamp<double>(mSettings.mAmp * Amp.scale, Amp.min * Amp.scale, Amp.max * Amp.scale) / Amp.scale);

   mSettings.mCanClip = mClip->GetValue();

   if (!mSettings.mCanClip && mSettings.mRatio * mState.mPeak > 1.0)
   {
      mSettings.mRatio = 1.0 / mState.mPeak;
   }

   ClampRatio();

   return true;
}

// EffectAmplify implementation

void EffectAmplifySL::CheckClip()
{
   EnableApply(mClip->GetValue() || (mState.mPeak > 0.0 && mSettings.mRatio <= mState.mRatioClip));
}

void EffectAmplifySL::OnAmpText(wxCommandEvent & WXUNUSED(evt))
{
   if (!mAmpT->GetValidator()->TransferFromWindow())
   {
      EnableApply(false);
      return;
   }

   mSettings.mRatio = DB_TO_LINEAR(std::clamp<double>(mSettings.mAmp * Amp.scale, Amp.min * Amp.scale, Amp.max * Amp.scale) / Amp.scale);

   mAmpS->SetValue((int) (LINEAR_TO_DB(mSettings.mRatio) * Amp.scale + 0.5));

   mState.mNewPeak = LINEAR_TO_DB(mSettings.mRatio * mState.mPeak);
   mNewPeakT->GetValidator()->TransferToWindow();

   CheckClip();
}

void EffectAmplifySL::OnPeakText(wxCommandEvent & WXUNUSED(evt))
{
   if (!mNewPeakT->GetValidator()->TransferFromWindow())
   {
      EnableApply(false);
      return;
   }

   if (mState.mNewPeak == 0.0)
      mSettings.mRatio = mState.mRatioClip;
   else
      mSettings.mRatio = DB_TO_LINEAR(mState.mNewPeak) / mState.mPeak;

   double ampInit = LINEAR_TO_DB(mSettings.mRatio);
   mSettings.mAmp = std::clamp<double>(ampInit, Amp.min, Amp.max);
   if (mSettings.mAmp != ampInit)
      mSettings.mRatio = DB_TO_LINEAR(mSettings.mAmp);

   mAmpT->GetValidator()->TransferToWindow();

   mAmpS->SetValue((int) (mSettings.mAmp * Amp.scale + 0.5f));

   CheckClip();
}

void EffectAmplifySL::OnAmpSlider(wxCommandEvent & evt)
{
   double dB = evt.GetInt() / Amp.scale;
   mSettings.mRatio = DB_TO_LINEAR(std::clamp<double>(dB, Amp.min, Amp.max));

   double dB2 = (evt.GetInt() - 1) / Amp.scale;
   double ratio2 = DB_TO_LINEAR(std::clamp<double>(dB2, Amp.min, Amp.max));

   if (!mClip->GetValue() && mSettings.mRatio * mState.mPeak > 1.0 && ratio2 * mState.mPeak < 1.0)
   {
      mSettings.mRatio = 1.0 / mState.mPeak;
   }

   mSettings.mAmp = LINEAR_TO_DB(mSettings.mRatio);
   mAmpT->GetValidator()->TransferToWindow();

   mState.mNewPeak = LINEAR_TO_DB(mSettings.mRatio * mState.mPeak);
   mNewPeakT->GetValidator()->TransferToWindow();

   CheckClip();
}

void EffectAmplifySL::OnClipCheckBox(wxCommandEvent & WXUNUSED(evt))
{
   CheckClip();
}
