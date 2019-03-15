/**********************************************************************

  Audacity: A Digital Audio Editor

  Loudness.cpp

  Max Maisel

*******************************************************************//**

\class EffectLoudness
\brief An Effect to bring the loudness level up to a chosen level.

*//*******************************************************************/


#include "../Audacity.h" // for rint from configwin.h
#include "Loudness.h"

#include <math.h>

#include <wx/intl.h>
#include <wx/valgen.h>

#include "../Internat.h"
#include "../Prefs.h"
#include "../Shuttle.h"
#include "../ShuttleGui.h"
#include "../WaveTrack.h"
#include "../widgets/valnum.h"

enum kNormalizeTargets
{
   kLoudness,
   kRMS,
   nAlgos
};

static const ComponentInterfaceSymbol kNormalizeTargetStrings[nAlgos] =
{
   { XO("perceived loudness") },
   { XO("RMS") }
};
// Define keys, defaults, minimums, and maximums for the effect parameters
//
//     Name         Type     Key                        Def         Min      Max       Scale
Param( StereoInd,   bool,    wxT("StereoIndependent"),   false,      false,   true,     1  );
Param( LUFSLevel,   double,  wxT("LUFSLevel"),           -23.0,      -145.0,  0.0,      1  );
Param( RMSLevel,    double,  wxT("RMSLevel"),            -20.0,      -145.0,  0.0,      1  );
Param( DualMono,    bool,    wxT("DualMono"),            true,       false,   true,     1  );
Param( NormalizeTo, int,     wxT("NormalizeTo"),         kLoudness , 0    ,   nAlgos-1, 1  );

BEGIN_EVENT_TABLE(EffectLoudness, wxEvtHandler)
   EVT_CHOICE(wxID_ANY, EffectLoudness::OnUpdateUI)
   EVT_CHECKBOX(wxID_ANY, EffectLoudness::OnUpdateUI)
   EVT_TEXT(wxID_ANY, EffectLoudness::OnUpdateUI)
END_EVENT_TABLE()

EffectLoudness::EffectLoudness()
{
   mStereoInd = DEF_StereoInd;
   mLUFSLevel = DEF_LUFSLevel;
   mRMSLevel = DEF_RMSLevel;
   mDualMono = DEF_DualMono;
   mNormalizeTo = DEF_NormalizeTo;

   SetLinearEffectFlag(false);
}

EffectLoudness::~EffectLoudness()
{
}

// ComponentInterface implementation

ComponentInterfaceSymbol EffectLoudness::GetSymbol()
{
   return LOUDNESS_PLUGIN_SYMBOL;
}

wxString EffectLoudness::GetDescription()
{
   return _("Sets the loudness of one or more tracks");
}

wxString EffectLoudness::ManualPage()
{
   return wxT("Loudness");
}

// EffectDefinitionInterface implementation

EffectType EffectLoudness::GetType()
{
   return EffectTypeProcess;
}

// EffectClientInterface implementation
bool EffectLoudness::DefineParams( ShuttleParams & S )
{
   S.SHUTTLE_PARAM( mStereoInd, StereoInd );
   S.SHUTTLE_PARAM( mLUFSLevel, LUFSLevel );
   S.SHUTTLE_PARAM( mRMSLevel, RMSLevel );
   S.SHUTTLE_PARAM( mDualMono, DualMono );
   S.SHUTTLE_PARAM( mNormalizeTo, NormalizeTo );
   return true;
}

bool EffectLoudness::GetAutomationParameters(CommandParameters & parms)
{
   parms.Write(KEY_StereoInd, mStereoInd);
   parms.Write(KEY_LUFSLevel, mLUFSLevel);
   parms.Write(KEY_RMSLevel, mRMSLevel);
   parms.Write(KEY_DualMono, mDualMono);
   parms.Write(KEY_NormalizeTo, mNormalizeTo);

   return true;
}

bool EffectLoudness::SetAutomationParameters(CommandParameters & parms)
{
   ReadAndVerifyBool(StereoInd);
   ReadAndVerifyDouble(LUFSLevel);
   ReadAndVerifyDouble(RMSLevel);
   ReadAndVerifyBool(DualMono);
   ReadAndVerifyBool(NormalizeTo);

   mStereoInd = StereoInd;
   mLUFSLevel = LUFSLevel;
   mRMSLevel = RMSLevel;
   mDualMono = DualMono;
   mNormalizeTo = NormalizeTo;

   return true;
}

// Effect implementation

bool EffectLoudness::CheckWhetherSkipEffect()
{
   return false;
}

bool EffectLoudness::Startup()
{
   wxString base = wxT("/Effects/Loudness/");
   // Load the old "current" settings
   if (gPrefs->Exists(base))
   {
      mStereoInd = true;
      mDualMono = DEF_DualMono;
      mNormalizeTo = kLoudness;
      mLUFSLevel = DEF_LUFSLevel;
      mRMSLevel = DEF_RMSLevel;

      SaveUserPreset(GetCurrentSettingsGroup());

      gPrefs->Flush();
   }
   return true;
}

bool EffectLoudness::Process()
{
   // TODO
   return true;
}

void EffectLoudness::PopulateOrExchange(ShuttleGui & S)
{
   S.StartVerticalLay(0);
   {
      S.StartMultiColumn(2, wxALIGN_CENTER);
      {
         S.StartVerticalLay(false);
         {
            S.StartHorizontalLay(wxALIGN_LEFT, false);
            {
               S.AddVariableText(_("Normalize"), false,
                                 wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT);

               auto targetChoices = LocalizedStrings(kNormalizeTargetStrings, nAlgos);
               mNormalizeToCtl = S.AddChoice(wxEmptyString, targetChoices, mNormalizeTo);
               mNormalizeToCtl->SetValidator(wxGenericValidator(&mNormalizeTo));
               S.AddVariableText(_("to"), false,
                                 wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT);

               FloatingPointValidator<double> vldLevel(2, &mLUFSLevel,
                                                       NumValidatorStyle::ONE_TRAILING_ZERO);
               vldLevel.SetRange( MIN_LUFSLevel, MAX_LUFSLevel);

               mLevelTextCtrl = S.AddTextBox( {}, wxT(""), 10);
               /* i18n-hint: LUFS is a particular method for measuring loudnesss */
               mLevelTextCtrl->SetName( _("Loudness LUFS"));
               mLevelTextCtrl->SetValidator(vldLevel);
               /* i18n-hint: LUFS is a particular method for measuring loudnesss */
               mLeveldB = S.AddVariableText(_("LUFS"), false,
                                            wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT);
               mWarning = S.AddVariableText( {}, false,
                                            wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT);
            }
            S.EndHorizontalLay();

            mStereoIndCheckBox = S.AddCheckBox(_("Normalize stereo channels independently"),
                                               mStereoInd ? wxT("true") : wxT("false"));
            mStereoIndCheckBox->SetValidator(wxGenericValidator(&mStereoInd));

            mDualMonoCheckBox = S.AddCheckBox(_("Treat mono as dual-mono (recommended)"),
                                              mDualMono ? wxT("true") : wxT("false"));
            mDualMonoCheckBox->SetValidator(wxGenericValidator(&mDualMono));
         }
         S.EndVerticalLay();
      }
      S.EndMultiColumn();
   }
   S.EndVerticalLay();
   // To ensure that the UpdateUI on creation sets the prompts correctly.
   mGUINormalizeTo = !mNormalizeTo;
}

bool EffectLoudness::TransferDataToWindow()
{
   if (!mUIParent->TransferDataToWindow())
   {
      return false;
   }

   UpdateUI();
   return true;
}

bool EffectLoudness::TransferDataFromWindow()
{
   if (!mUIParent->Validate() || !mUIParent->TransferDataFromWindow())
   {
      return false;
   }
   return true;
}

// EffectLoudness implementation

// TODO

bool EffectLoudness::UpdateProgress()
{
   mProgressVal += (double(1+mProcStereo) * double(mTrackBufferLen)
                 / (double(GetNumWaveTracks()) * double(mSteps) * mTrackLen));
   return !TotalProgress(mProgressVal, mProgressMsg);
}

void EffectLoudness::OnUpdateUI(wxCommandEvent & WXUNUSED(evt))
{
   UpdateUI();
}

void EffectLoudness::UpdateUI()
{
   if (!mUIParent->TransferDataFromWindow())
   {
      mWarning->SetLabel(_("(Maximum 0dB)"));
      // TODO: recalculate layout here
      EnableApply(false);
      return;
   }
   mWarning->SetLabel(wxT(""));
   EnableApply(true);

   // Changing the prompts causes an unwanted UpdateUI event.  
   // This 'guard' stops that becoming an infinite recursion.
   if (mNormalizeTo != mGUINormalizeTo)
   {
      mGUINormalizeTo = mNormalizeTo;
      if(mNormalizeTo == kLoudness)
      {
         FloatingPointValidator<double> vldLevel(2, &mLUFSLevel, NumValidatorStyle::ONE_TRAILING_ZERO);
         vldLevel.SetRange(MIN_LUFSLevel, MAX_LUFSLevel);
         mLevelTextCtrl->SetValidator(vldLevel);
         /* i18n-hint: LUFS is a particular method for measuring loudnesss */
         mLevelTextCtrl->SetName(_("Loudness LUFS"));
         mLevelTextCtrl->SetValue(wxString::FromDouble(mLUFSLevel));
         /* i18n-hint: LUFS is a particular method for measuring loudnesss */
         mLeveldB->SetLabel(_("LUFS"));
      }
      else // RMS
      {
         FloatingPointValidator<double> vldLevel(2, &mRMSLevel, NumValidatorStyle::ONE_TRAILING_ZERO);
         vldLevel.SetRange(MIN_RMSLevel, MAX_RMSLevel);
         mLevelTextCtrl->SetValidator(vldLevel);
         mLevelTextCtrl->SetName(_("RMS dB"));
         mLevelTextCtrl->SetValue(wxString::FromDouble(mRMSLevel));
         mLeveldB->SetLabel(_("dB"));
      }
   }

   mDualMonoCheckBox->Enable(mNormalizeTo == kLoudness);
}
