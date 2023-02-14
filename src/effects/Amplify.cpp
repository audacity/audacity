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


#include "Amplify.h"
#include "LoadEffects.h"

#include <math.h>
#include <float.h>

#include <wx/button.h>
#include <wx/checkbox.h>
#include <wx/sizer.h>
#include <wx/slider.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/valtext.h>
#include <wx/log.h>

#include "ShuttleGui.h"
#include "WaveTrack.h"
#include "valnum.h"


enum
{
   ID_Amp = 10000,
   ID_Peak,
   ID_Clip
};

const EffectParameterMethods& EffectAmplify::Parameters() const
{
   static CapturedParameters<EffectAmplify,
      // Interactive case
      Ratio, Clipping
   > parameters;

   static CapturedParameters<EffectAmplify,
      Ratio
   > batchParameters{
      // If invoking Amplify from a macro, mCanClip is not a parameter
      // but is always true
      [](EffectAmplify &, EffectSettings &, EffectAmplify &e, bool) {
         e.mCanClip = true;
         return true;
      },
   };

   // Parameters differ depending on batch mode.  Option to disable clipping
   // is interactive only.
   if (IsBatchProcessing())
      return batchParameters;
   else
      return parameters;
}

//
// EffectAmplify
//

const ComponentInterfaceSymbol EffectAmplify::Symbol
{ XO("Amplify") };

namespace{ BuiltinEffectsModule::Registration< EffectAmplify > reg; }

BEGIN_EVENT_TABLE(EffectAmplify, wxEvtHandler)
   EVT_SLIDER(ID_Amp, EffectAmplify::OnAmpSlider)
   EVT_TEXT(ID_Amp, EffectAmplify::OnAmpText)
   EVT_TEXT(ID_Peak, EffectAmplify::OnPeakText)
   EVT_CHECKBOX(ID_Clip, EffectAmplify::OnClipCheckBox)
END_EVENT_TABLE()

EffectAmplify::EffectAmplify()
{
   mAmp = Amp.def;
   // Ratio.def == DB_TO_LINEAR(Amp.def)
   Parameters().Reset(*this);
   mRatioClip = 0.0;
   mPeak = 0.0;

   SetLinearEffectFlag(true);
}

EffectAmplify::~EffectAmplify()
{
}

// ComponentInterface implementation

ComponentInterfaceSymbol EffectAmplify::GetSymbol() const
{
   return Symbol;
}

TranslatableString EffectAmplify::GetDescription() const
{
   // Note: This is useful only after ratio has been set.
   return XO("Increases or decreases the volume of the audio you have selected");
}

ManualPageID EffectAmplify::ManualPage() const
{
   return L"Amplify";
}

// EffectDefinitionInterface implementation

EffectType EffectAmplify::GetType() const
{
   return EffectTypeProcess;
}

unsigned EffectAmplify::GetAudioInCount() const
{
   return 1;
}

unsigned EffectAmplify::GetAudioOutCount() const
{
   return 1;
}

size_t EffectAmplify::ProcessBlock(EffectSettings &,
   const float *const *inBlock, float *const *outBlock, size_t blockLen)
{
   for (decltype(blockLen) i = 0; i < blockLen; i++)
   {
      outBlock[0][i] = inBlock[0][i] * mRatio;
   }

   return blockLen;
}

OptionalMessage
EffectAmplify::LoadFactoryDefaults(EffectSettings &settings) const
{
   // To do: externalize state so const_cast isn't needed
   return const_cast<EffectAmplify&>(*this).DoLoadFactoryDefaults(settings);
}

OptionalMessage EffectAmplify::DoLoadFactoryDefaults(EffectSettings &settings)
{
   Init();

   mRatioClip = 0.0;
   if (mPeak > 0.0)
   {
      mRatio = 1.0 / mPeak;
      mRatioClip = mRatio;
   }
   else
   {
      mRatio = 1.0;
   }
   mCanClip = false;

   ClampRatio();
   return { nullptr };
}

// Effect implementation

bool EffectAmplify::Init()
{
   mPeak = 0.0;

   for (auto t : inputTracks()->Selected< const WaveTrack >())
   {
      auto pair = t->GetMinMax(mT0, mT1); // may throw
      const float min = pair.first, max = pair.second;
      float newpeak = (fabs(min) > fabs(max) ? fabs(min) : fabs(max));

      if (newpeak > mPeak)
      {
         mPeak = newpeak;
      }
   }

   return true;
}

void EffectAmplify::Preview(EffectSettingsAccess &access, bool dryOnly)
{
   auto cleanup1 = valueRestorer( mRatio );
   auto cleanup2 = valueRestorer( mPeak );

   Effect::Preview(access, dryOnly);
}

std::unique_ptr<EffectUIValidator> EffectAmplify::PopulateOrExchange(
   ShuttleGui & S, EffectInstance &, EffectSettingsAccess &,
   const EffectOutputs *)
{
   mUIParent = S.GetParent();

   enum{ precision = 3 }; // allow (a generous) 3 decimal  places for Amplification (dB)

   bool batch = IsBatchProcessing();
   if ( batch )
   {
      mCanClip = true;
      mPeak = 1.0;
   }
   else 
   {
      if (mPeak > 0.0)
      {
         mRatio = 1.0 / mPeak;
         mRatioClip = mRatio;
      }
      else
      {
         mRatio = 1.0;
      }
   }

   // At this point mNewPeak is still uninitialized; this will initialize it
   ClampRatio();

   S.AddSpace(0, 5);

   S.StartVerticalLay(0);
   {
      // Amplitude
      S.StartMultiColumn(2, wxCENTER);
      {
         mAmpT = S.Id(ID_Amp)
            .Validator<FloatingPointValidator<double>>(
               precision, &mAmp, NumValidatorStyle::ONE_TRAILING_ZERO, Amp.min, Amp.max )
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
               &mNewPeak, NumValidatorStyle::ONE_TRAILING_ZERO,
               // min and max need same precision as what we're validating (bug 963)
               RoundValue( precision + 1, Amp.min + LINEAR_TO_DB(mPeak) ),
               RoundValue( precision + 1, Amp.max + LINEAR_TO_DB(mPeak) ) )
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

void EffectAmplify::ClampRatio()
{
   // limit range of gain
   double dBInit = LINEAR_TO_DB(mRatio);
   double dB = std::clamp<double>(dBInit, Amp.min, Amp.max);
   if (dB != dBInit)
      mRatio = DB_TO_LINEAR(dB);

   mAmp = LINEAR_TO_DB(mRatio);
   mNewPeak = LINEAR_TO_DB(mRatio * mPeak);
}

bool EffectAmplify::TransferDataToWindow(const EffectSettings &)
{
   mAmpT->GetValidator()->TransferToWindow();

   mAmpS->SetValue((int) (mAmp * Amp.scale + 0.5f));

   mNewPeakT->GetValidator()->TransferToWindow();

   mClip->SetValue(mCanClip);

   CheckClip();

   return true;
}

bool EffectAmplify::TransferDataFromWindow(EffectSettings &)
{
   if (!mUIParent->Validate() || !mUIParent->TransferDataFromWindow())
   {
      return false;
   }

   mRatio = DB_TO_LINEAR(std::clamp<double>(mAmp * Amp.scale, Amp.min * Amp.scale, Amp.max * Amp.scale) / Amp.scale);

   mCanClip = mClip->GetValue();

   if (!mCanClip && mRatio * mPeak > 1.0)
   {
      mRatio = 1.0 / mPeak;
   }

   ClampRatio();

   return true;
}

// EffectAmplify implementation

void EffectAmplify::CheckClip()
{
   EffectUIValidator::EnableApply(mUIParent,
      mClip->GetValue() || (mPeak > 0.0 && mRatio <= mRatioClip));
}

void EffectAmplify::OnAmpText(wxCommandEvent & WXUNUSED(evt))
{
   if (!mAmpT->GetValidator()->TransferFromWindow())
   {
      EffectUIValidator::EnableApply(mUIParent, false);
      return;
   }

   mRatio = DB_TO_LINEAR(std::clamp<double>(mAmp * Amp.scale, Amp.min * Amp.scale, Amp.max * Amp.scale) / Amp.scale);

   mAmpS->SetValue((int) (LINEAR_TO_DB(mRatio) * Amp.scale + 0.5));

   mNewPeak = LINEAR_TO_DB(mRatio * mPeak);
   mNewPeakT->GetValidator()->TransferToWindow();

   CheckClip();
}

void EffectAmplify::OnPeakText(wxCommandEvent & WXUNUSED(evt))
{
   if (!mNewPeakT->GetValidator()->TransferFromWindow())
   {
      EffectUIValidator::EnableApply(mUIParent, false);
      return;
   }

   if (mNewPeak == 0.0)
      mRatio = mRatioClip;
   else
      mRatio = DB_TO_LINEAR(mNewPeak) / mPeak;

   double ampInit = LINEAR_TO_DB(mRatio);
   mAmp = std::clamp<double>(ampInit, Amp.min, Amp.max);
   if (mAmp != ampInit)
      mRatio = DB_TO_LINEAR(mAmp);

   mAmpT->GetValidator()->TransferToWindow();

   mAmpS->SetValue((int) (mAmp * Amp.scale + 0.5f));

   CheckClip();
}

void EffectAmplify::OnAmpSlider(wxCommandEvent & evt)
{
   double dB = evt.GetInt() / Amp.scale;
   mRatio = DB_TO_LINEAR(std::clamp<double>(dB, Amp.min, Amp.max));

   double dB2 = (evt.GetInt() - 1) / Amp.scale;
   double ratio2 = DB_TO_LINEAR(std::clamp<double>(dB2, Amp.min, Amp.max));

   if (!mClip->GetValue() && mRatio * mPeak > 1.0 && ratio2 * mPeak < 1.0)
   {
      mRatio = 1.0 / mPeak;
   }

   mAmp = LINEAR_TO_DB(mRatio);
   mAmpT->GetValidator()->TransferToWindow();

   mNewPeak = LINEAR_TO_DB(mRatio * mPeak);
   mNewPeakT->GetValidator()->TransferToWindow();

   CheckClip();
}

void EffectAmplify::OnClipCheckBox(wxCommandEvent & WXUNUSED(evt))
{
   CheckClip();
}
