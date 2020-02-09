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

BEGIN_EVENT_TABLE(EffectCompressor2, wxEvtHandler)
END_EVENT_TABLE()

const ComponentInterfaceSymbol EffectCompressor2::Symbol
{ XO("Compressor v2") };

namespace{ BuiltinEffectsModule::Registration< EffectCompressor2 > reg; }

EffectCompressor2::EffectCompressor2()
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
}

bool EffectCompressor2::TransferDataToWindow()
{
   if (!mUIParent->TransferDataToWindow())
   {
      return false;
   }

   UpdateUI();
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
   UpdateUI();
}

void EffectCompressor2::UpdateUI()
{
}
