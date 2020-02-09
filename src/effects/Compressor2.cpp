/**********************************************************************

  Audacity: A Digital Audio Editor

  Compressor2.cpp

  Max Maisel

*******************************************************************//**

\class EffectCompressor2
\brief An Effect which reduces the dynamic level.

*//*******************************************************************/


#include "../Audacity.h" // for rint from configwin.h
#include "Compressor2.h"

#include <math.h>

#include <wx/intl.h>
#include <wx/valgen.h>

#include "../Internat.h"
#include "../Prefs.h"
#include "../ProjectFileManager.h"
#include "../Shuttle.h"
#include "../ShuttleGui.h"
#include "../WaveTrack.h"
#include "../widgets/valnum.h"
#include "../widgets/ProgressDialog.h"
#include "../widgets/SliderTextCtrl.h"

#include "LoadEffects.h"

enum kAlgorithms
{
   kExpFit,
   kEnvPT1,
   nAlgos
};

static const ComponentInterfaceSymbol kAlgorithmStrings[nAlgos] =
{
   { XO("Exponential-Fit") },
   { XO("Analog Model (PT1)") }
};

enum kCompressBy
{
   kAmplitude,
   kRMS,
   nBy
};

static const ComponentInterfaceSymbol kCompressByStrings[nBy] =
{
   { XO("peak amplitude") },
   { XO("RMS") }
};

// Define keys, defaults, minimums, and maximums for the effect parameters
//
//     Name         Type     Key                        Def         Min      Max       Scale
Param( Algorithm,      int,     wxT("Algorithm"),         kEnvPT1,    0,    nAlgos-1,  1   );
Param( CompressBy,     int,     wxT("CompressBy"),   kAmplitude,      0,   nBy-1,      1   );
Param( StereoInd,      bool,    wxT("StereoIndependent"), false,   false,   true,      1   );

Param( Threshold,      double,  wxT("Threshold"),        -12.0,   -60.0,     -1.0,     1.0 );
Param( Ratio,          double,  wxT("Ratio"),              2.0,     1.1,    100.0,    10.0 );
Param( KneeWidth,      double,  wxT("KneeWidth"),         10.0,     0.0,     20.0,    10.0 );
Param( AttackTime,     double,  wxT("AttackTime"),         0.2,     0.00001, 30.0, 20000.0 );
Param( ReleaseTime,    double,  wxT("ReleaseTime"),        1.0,     0.00001, 30.0, 20000.0 );
Param( LookaheadTime,  double,  wxT("LookaheadTime"),      0.0,     0.0,     10.0,   200.0 );
Param( LookbehindTime, double,  wxT("LookbehindTime"),     0.1,     0.0,     10.0,   200.0 );
Param( MakeupGain,     double,  wxT("MakeupGain"),         0.0,     0.0,    100.0,     1.0 );
Param( DryWet,         double,  wxT("DryWet"),           100.0,     0.0,    100.0,     1.0 );

inline int ScaleToPrecision(double scale)
{
   return ceil(log10(scale));
}

BEGIN_EVENT_TABLE(EffectCompressor2, wxEvtHandler)
   EVT_CHECKBOX(wxID_ANY, EffectCompressor2::OnUpdateUI)
   EVT_CHOICE(wxID_ANY, EffectCompressor2::OnUpdateUI)
   EVT_SLIDERTEXT(wxID_ANY, EffectCompressor2::OnUpdateUI)
END_EVENT_TABLE()

const ComponentInterfaceSymbol EffectCompressor2::Symbol
{ XO("Compressor v2") };

namespace{ BuiltinEffectsModule::Registration< EffectCompressor2 > reg; }

EffectCompressor2::EffectCompressor2()
   : mIgnoreGuiEvents(false)
{
   mAlgorithm = DEF_Algorithm;
   mCompressBy = DEF_CompressBy;
   mStereoInd = DEF_StereoInd;

   mThresholdDB = DEF_Threshold;
   mRatio = DEF_Ratio;                    // positive number > 1.0
   mKneeWidthDB = DEF_KneeWidth;
   mAttackTime = DEF_AttackTime;          // seconds
   mReleaseTime = DEF_ReleaseTime;          // seconds
   mLookaheadTime = DEF_LookaheadTime;
   mLookbehindTime = DEF_LookbehindTime;
   mMakeupGainPct = DEF_MakeupGain;
   mDryWetPct = DEF_DryWet;

   SetLinearEffectFlag(false);
}

EffectCompressor2::~EffectCompressor2()
{
}

// ComponentInterface implementation

ComponentInterfaceSymbol EffectCompressor2::GetSymbol()
{
   return Symbol;
}

TranslatableString EffectCompressor2::GetDescription()
{
   return XO("Reduces the dynamic of one or more tracks");
}

wxString EffectCompressor2::ManualPage()
{
   return wxT("Compressor2");
}

// EffectDefinitionInterface implementation

EffectType EffectCompressor2::GetType()
{
   return EffectTypeProcess;
}

// EffectClientInterface implementation
bool EffectCompressor2::DefineParams( ShuttleParams & S )
{
   S.SHUTTLE_PARAM(mAlgorithm, Algorithm);
   S.SHUTTLE_PARAM(mCompressBy, CompressBy);
   S.SHUTTLE_PARAM(mStereoInd, StereoInd);

   S.SHUTTLE_PARAM(mThresholdDB, Threshold);
   S.SHUTTLE_PARAM(mRatio, Ratio);
   S.SHUTTLE_PARAM(mKneeWidthDB, KneeWidth);
   S.SHUTTLE_PARAM(mAttackTime, AttackTime);
   S.SHUTTLE_PARAM(mReleaseTime, ReleaseTime);
   S.SHUTTLE_PARAM(mLookaheadTime, LookaheadTime);
   S.SHUTTLE_PARAM(mLookbehindTime, LookbehindTime);
   S.SHUTTLE_PARAM(mMakeupGainPct, MakeupGain);
   S.SHUTTLE_PARAM(mDryWetPct, DryWet);

   return true;
}

bool EffectCompressor2::GetAutomationParameters(CommandParameters & parms)
{
   parms.Write(KEY_Algorithm, mAlgorithm);
   parms.Write(KEY_CompressBy, mCompressBy);
   parms.Write(KEY_StereoInd, mStereoInd);

   parms.Write(KEY_Threshold, mThresholdDB);
   parms.Write(KEY_Ratio, mRatio);
   parms.Write(KEY_KneeWidth, mKneeWidthDB);
   parms.Write(KEY_AttackTime, mAttackTime);
   parms.Write(KEY_ReleaseTime, mReleaseTime);
   parms.Write(KEY_LookaheadTime, mLookaheadTime);
   parms.Write(KEY_LookbehindTime, mLookbehindTime);
   parms.Write(KEY_MakeupGain, mMakeupGainPct);
   parms.Write(KEY_DryWet, mDryWetPct);

   return true;
}

bool EffectCompressor2::SetAutomationParameters(CommandParameters & parms)
{
   ReadAndVerifyInt(Algorithm);
   ReadAndVerifyInt(CompressBy);
   ReadAndVerifyBool(StereoInd);

   ReadAndVerifyDouble(Threshold);
   ReadAndVerifyDouble(Ratio);
   ReadAndVerifyDouble(KneeWidth);
   ReadAndVerifyDouble(AttackTime);
   ReadAndVerifyDouble(ReleaseTime);
   ReadAndVerifyDouble(LookaheadTime);
   ReadAndVerifyDouble(LookbehindTime);
   ReadAndVerifyDouble(MakeupGain);
   ReadAndVerifyDouble(DryWet);

   mAlgorithm = Algorithm;
   mCompressBy = CompressBy;
   mStereoInd = StereoInd;

   mThresholdDB = Threshold;
   mRatio = Ratio;
   mKneeWidthDB = KneeWidth;
   mAttackTime = AttackTime;
   mReleaseTime = ReleaseTime;
   mLookaheadTime = LookaheadTime;
   mLookbehindTime = LookbehindTime;
   mMakeupGainPct = MakeupGain;
   mDryWetPct = DryWet;

   return true;
}

// Effect implementation

bool EffectCompressor2::CheckWhetherSkipEffect()
{
   return false;
}

bool EffectCompressor2::Startup()
{
   wxString base = wxT("/Effects/Compressor2/");
   // Load the old "current" settings
   if (gPrefs->Exists(base))
   {
      mAlgorithm = DEF_Algorithm;
      mCompressBy = DEF_CompressBy;
      mStereoInd = DEF_StereoInd;

      mThresholdDB = DEF_Threshold;
      mRatio = DEF_Ratio;                    // positive number > 1.0
      mKneeWidthDB = DEF_KneeWidth;
      mAttackTime = DEF_AttackTime;          // seconds
      mReleaseTime = DEF_ReleaseTime;          // seconds
      mLookaheadTime = DEF_LookaheadTime;
      mLookbehindTime = DEF_LookbehindTime;
      mMakeupGainPct = DEF_MakeupGain;
      mDryWetPct = DEF_DryWet;

      SaveUserPreset(GetCurrentSettingsGroup());

      gPrefs->Flush();
   }
   return true;
}

bool EffectCompressor2::Process()
{
   return false;
}

void EffectCompressor2::PopulateOrExchange(ShuttleGui & S)
{
   S.StartStatic(XO("Algorithm"));
   {
      S.StartMultiColumn(2, wxALIGN_LEFT);
      {
         S.SetStretchyCol(1);

         wxChoice* ctrl = nullptr;

         ctrl = S.Validator<wxGenericValidator>(&mAlgorithm)
            .AddChoice(XO("Envelope Algorithm:"),
               Msgids(kAlgorithmStrings, nAlgos),
               mAlgorithm);

         wxSize box_size = ctrl->GetMinSize();
         int width = S.GetParent()->GetTextExtent(wxString::Format(
            "%sxxxx",  kAlgorithmStrings[nAlgos-1].Translation())).GetWidth();
         box_size.SetWidth(width);
         ctrl->SetMinSize(box_size);

         ctrl = S.Validator<wxGenericValidator>(&mCompressBy)
            .AddChoice(XO("Compress based on:"),
               Msgids(kCompressByStrings, nBy),
               mCompressBy);
         ctrl->SetMinSize(box_size);

         S.Validator<wxGenericValidator>(&mStereoInd)
            .AddCheckBox(XO("Compress stereo channels independently"),
               DEF_StereoInd);
      }
      S.EndMultiColumn();
   }
   S.EndStatic();

   S.StartStatic(XO("Compressor"));
   {
      S.StartMultiColumn(3, wxEXPAND);
      {
         S.SetStretchyCol(1);
         int textbox_width = S.GetParent()->GetTextExtent("0.000001").GetWidth();
         SliderTextCtrl* ctrl = nullptr;

         S.AddVariableText(XO("Threshold:"), true,
            wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL);
         ctrl = S.Name(XO("Threshold"))
            .Style(SliderTextCtrl::HORIZONTAL)
            .AddSliderTextCtrl({}, DEF_Threshold, MAX_Threshold,
               MIN_Threshold, ScaleToPrecision(SCL_Threshold), &mThresholdDB);
         ctrl->SetMinTextboxWidth(textbox_width);
         S.AddVariableText(XO("dB"), true,
            wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL);

         S.AddVariableText(XO("Ratio:"), true,
            wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL);
         ctrl = S.Name(XO("Ratio"))
            .Style(SliderTextCtrl::HORIZONTAL | SliderTextCtrl::LOG)
            .AddSliderTextCtrl({}, DEF_Ratio, MAX_Ratio, MIN_Ratio,
               ScaleToPrecision(SCL_Ratio), &mRatio);
         /* i18n-hint: Unless your language has a different convention for ratios,
          * like 8:1, leave as is.*/
         ctrl->SetMinTextboxWidth(textbox_width);
         S.AddVariableText(XO(":1"), true,
            wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL);

         S.AddVariableText(XO("Knee Width:"), true,
            wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL);
         ctrl = S.Name(XO("Knee Width"))
            .Style(SliderTextCtrl::HORIZONTAL)
            .AddSliderTextCtrl({}, DEF_KneeWidth, MAX_KneeWidth,
               MIN_KneeWidth, ScaleToPrecision(SCL_KneeWidth),
               &mKneeWidthDB);
         ctrl->SetMinTextboxWidth(textbox_width);
         S.AddVariableText(XO("dB"), true,
            wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL);

         S.AddVariableText(XO("Attack Time:"), true,
            wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL);
         ctrl = S.Name(XO("Attack Time"))
            .Style(SliderTextCtrl::HORIZONTAL | SliderTextCtrl::LOG)
            .AddSliderTextCtrl({}, DEF_AttackTime, MAX_AttackTime,
               MIN_AttackTime, ScaleToPrecision(SCL_AttackTime),
               &mAttackTime, SCL_AttackTime / 1000);
         ctrl->SetMinTextboxWidth(textbox_width);
         S.AddVariableText(XO("s"), true,
            wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL);

         S.AddVariableText(XO("Release Time:"), true,
            wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL);
         ctrl = S.Name(XO("Release Time"))
            .Style(SliderTextCtrl::HORIZONTAL | SliderTextCtrl::LOG)
            .AddSliderTextCtrl({}, DEF_ReleaseTime, MAX_ReleaseTime,
               MIN_ReleaseTime, ScaleToPrecision(SCL_ReleaseTime),
               &mReleaseTime, SCL_ReleaseTime / 1000);
         ctrl->SetMinTextboxWidth(textbox_width);
         S.AddVariableText(XO("s"), true,
            wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL);

         S.AddVariableText(XO("Lookahead Time:"), true,
            wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL);
         ctrl = S.Name(XO("Lookahead Time"))
            .Style(SliderTextCtrl::HORIZONTAL | SliderTextCtrl::LOG)
            .AddSliderTextCtrl({}, DEF_LookaheadTime, MAX_LookaheadTime,
               MIN_LookaheadTime, ScaleToPrecision(SCL_LookaheadTime),
               &mLookaheadTime);
         ctrl->SetMinTextboxWidth(textbox_width);
         S.AddVariableText(XO("s"), true,
            wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL);

         S.AddVariableText(XO("Hold Time:"), true,
            wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL);
         ctrl = S.Name(XO("Hold Time"))
            .Style(SliderTextCtrl::HORIZONTAL | SliderTextCtrl::LOG)
            .AddSliderTextCtrl({}, DEF_LookbehindTime, MAX_LookbehindTime,
               MIN_LookbehindTime, ScaleToPrecision(SCL_LookbehindTime),
               &mLookbehindTime);
         ctrl->SetMinTextboxWidth(textbox_width);
         S.AddVariableText(XO("s"), true,
            wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL);

         /* i18n-hint: Make-up, i.e. correct for any reduction, rather than fabricate it.*/
         S.AddVariableText(XO("Make-up Gain:"), true,
            wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL);
         ctrl = S.Name(XO("Make-up Gain"))
            .Style(SliderTextCtrl::HORIZONTAL)
            .AddSliderTextCtrl({}, DEF_MakeupGain, MAX_MakeupGain,
               MIN_MakeupGain, ScaleToPrecision(SCL_MakeupGain),
               &mMakeupGainPct);
         ctrl->SetMinTextboxWidth(textbox_width);
         S.AddVariableText(XO("%"), true,
            wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL);

         S.AddVariableText(XO("Dry/Wet:"), true,
            wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL);
         ctrl = S.Name(XO("Dry/Wet"))
            .Style(SliderTextCtrl::HORIZONTAL)
            .AddSliderTextCtrl({}, DEF_DryWet, MAX_DryWet,
               MIN_DryWet, ScaleToPrecision(SCL_DryWet),
               &mDryWetPct);
         ctrl->SetMinTextboxWidth(textbox_width);
         S.AddVariableText(XO("%"), true,
            wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL);
      }
      S.EndMultiColumn();
   }
   S.EndVerticalLay();
}

bool EffectCompressor2::TransferDataToWindow()
{
   // Transferring data to window causes spurious UpdateUI events
   // which would reset the UI values to the previous value.
   // This guard lets the program ignore them.
   mIgnoreGuiEvents = true;
   if (!mUIParent->TransferDataToWindow())
   {
      mIgnoreGuiEvents = false;
      return false;
   }

   UpdateUI();
   mIgnoreGuiEvents = false;
   return true;
}

bool EffectCompressor2::TransferDataFromWindow()
{
   if (!mUIParent->Validate() || !mUIParent->TransferDataFromWindow())
   {
      return false;
   }
   return true;
}

// EffectCompressor2 implementation

void EffectCompressor2::OnUpdateUI(wxCommandEvent & WXUNUSED(evt))
{
   if(!mIgnoreGuiEvents)
      TransferDataFromWindow();
   UpdateUI();
}

void EffectCompressor2::UpdateUI()
{
}
