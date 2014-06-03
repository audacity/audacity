/**********************************************************************

  Audacity: A Digital Audio Editor

  Leveller.cpp

  Lynn Allan

******************************************************************//**

\class EffectLeveller
\brief An EffectSimpleMono

*//***************************************************************//**

\class LevellerDialog
\brief Dialog for EffectLeveller

*//*******************************************************************/



#include "../Audacity.h"

// For compilers that support precompilation, includes "wx.h".
#include <wx/wxprec.h>

#ifdef __BORLANDC__
#pragma hdrstop
#endif

#ifndef WX_PRECOMP
// Include your minimal set of headers here, or wx.h
#include <wx/wx.h>
#endif

#include <math.h>
#include "../Prefs.h"
#include "Leveller.h"

EffectLeveller::EffectLeveller()
{
   Init();
}

#define NUM_PASSES_CHOICES 5

bool EffectLeveller::Init()
{
   mLevellerNumPasses = gPrefs->Read(wxT("/Effects/Leveller/LevellerNumPasses"), 2L) ;
   if ((mLevellerNumPasses <= 0) || (mLevellerNumPasses > NUM_PASSES_CHOICES)) {  // corrupted Prefs?
      mLevellerNumPasses = 1;
      gPrefs->Write(wxT("/Effects/Leveller/LevellerNumPasses"), 1);
   }
   mLevellerDbChoiceIndex = gPrefs->Read(wxT("/Effects/Leveller/LevellerDbChoiceIndex"), 10L);
   if ((mLevellerDbChoiceIndex < 0) || (mLevellerDbChoiceIndex >= Enums::NumDbChoices)) {  // corrupted Prefs?
      mLevellerDbChoiceIndex = 0;  //Least dB
      gPrefs->Write(wxT("/Effects/Leveller/LevellerDbChoiceIndex"), mLevellerDbChoiceIndex);
   }
   gPrefs->Flush();

   mLevellerDbSilenceThreshold = Enums::Db2Signal[mLevellerDbChoiceIndex];

   CalcLevellerFactors();

   return true;
}

bool EffectLeveller::CheckWhetherSkipEffect()
{
   return mLevellerNumPasses == 0;
}

void EffectLeveller::End()
{
   int frameSum = (int)mFrameSum;
   gPrefs->Write(wxT("/Validate/LevellerFrameSum"), frameSum);
   gPrefs->Flush();
}

#define LEVELER_FACTORS 6
static double gLimit[LEVELER_FACTORS] = { 0.0001, 0.0, 0.1, 0.3, 0.5, 1.0 };
static double gAdjLimit[LEVELER_FACTORS];
static double gAddOnValue[LEVELER_FACTORS];
// static double gAdjFactor[LEVELER_FACTORS] = { 0.9, 1.0, 1.1, 1.1, 1.0, 0.9 };
static double gAdjFactor[LEVELER_FACTORS] = { 0.80, 1.00, 1.20, 1.20, 1.00, 0.80 };

void EffectLeveller::CalcLevellerFactors()
{
   mFrameSum            = 0.0;
   gLimit[1]            = mLevellerDbSilenceThreshold;
   int    prev          = 0;
   double addOnValue    = 0.0;
   double prevLimit     = 0.0;
   double limit         = gLimit[0];
   gAddOnValue[0]       = addOnValue;
   double adjFactor     = gAdjFactor[0];
   double upperAdjLimit = gLimit[0] * adjFactor;
   double prevAdjLimit  = upperAdjLimit;
   gAdjLimit[0]         = upperAdjLimit;

   for (int f = 1; f < LEVELER_FACTORS; ++f) {
      prev          = f - 1;
      adjFactor     = gAdjFactor[f];
      prevLimit     = gLimit[prev];
      limit         = gLimit[f];
      prevAdjLimit  = gAdjLimit[prev];
      addOnValue    = prevAdjLimit - (adjFactor * prevLimit);
      upperAdjLimit = (adjFactor * limit) + addOnValue;

      gAddOnValue[f] = addOnValue;
      gAdjLimit[f]   = (adjFactor * limit) + addOnValue;
   }
}

bool EffectLeveller::PromptUser()
{
   LevellerDialog dlog(this, mParent);
   dlog.mLevellerDbChoiceIndex = mLevellerDbChoiceIndex;
   dlog.mLevellerNumPassesChoiceIndex = mLevellerNumPasses-1;
   dlog.TransferDataToWindow();

   dlog.CentreOnParent();
   dlog.ShowModal();

   if (dlog.GetReturnCode() == wxID_CANCEL) {
      return false;
   }

   mLevellerNumPasses = dlog.mLevellerNumPassesChoiceIndex+1;
   mLevellerDbChoiceIndex = dlog.mLevellerDbChoiceIndex;
   mLevellerDbSilenceThreshold = Enums::Db2Signal[mLevellerDbChoiceIndex];

   gPrefs->Write(wxT("/Effects/Leveller/LevellerDbChoiceIndex"), mLevellerDbChoiceIndex);
   gPrefs->Write(wxT("/Effects/Leveller/LevellerNumPasses"), mLevellerNumPasses);
   gPrefs->Flush();

   CalcLevellerFactors();

   return true;
}

bool EffectLeveller::TransferParameters( Shuttle & shuttle )
{
   shuttle.TransferEnum(wxT("dB"),mLevellerDbChoiceIndex,Enums::NumDbChoices,Enums::GetDbChoices());
   shuttle.TransferInt(wxT("Passes"),mLevellerNumPasses,1);
   return true;
}

float EffectLeveller::LevelOneFrame(float frameInBuffer)
{
   float  curFrame;
   float  fabsCurFrame;
   float  curSign;

   curFrame = frameInBuffer;
   if (curFrame < 0.0) {
      curSign = -1.0;
   }
   else {
      curSign = 1.0;
   }
   fabsCurFrame = (float)fabs(curFrame);
   mFrameSum += fabsCurFrame;

   for (int f = 0; f < LEVELER_FACTORS; ++f) {
     if (fabsCurFrame <= gLimit[f]) {
        curFrame *= (float)gAdjFactor[f];
        curFrame += (float)(gAddOnValue[f] * curSign);
        return curFrame;
     }
   }
   return (float)0.99;
}

bool EffectLeveller::ProcessSimpleMono(float *buffer, sampleCount len)
{
   for (int pass = 0; pass < mLevellerNumPasses; ++pass) {
      for (int i = 0; i < len; ++i) {
         buffer[i] = LevelOneFrame(buffer[i]);
      }
   }
   return true;
}

//----------------------------------------------------------------------------
// LevellerDialog
//----------------------------------------------------------------------------

BEGIN_EVENT_TABLE(LevellerDialog, EffectDialog)
   EVT_BUTTON(ID_EFFECT_PREVIEW, LevellerDialog::OnPreview)
END_EVENT_TABLE()

LevellerDialog::LevellerDialog(EffectLeveller *effect, wxWindow *parent)
:  EffectDialog(parent, _("Leveler"), PROCESS_EFFECT), // Lynn called it "Leveller", but preferred spelling is "Leveler".
   mEffect(effect)
{
   mLevellerNumPassesChoiceIndex = 0;//
   mLevellerDbChoiceIndex = 0;
   Init();
}

void LevellerDialog::PopulateOrExchange(ShuttleGui & S)
{
   wxArrayString db(Enums::NumDbChoices, Enums::GetDbChoices());
   wxArrayString numPasses;

   /* i18n-hint: Of strength of an effect.  Not strongly.*/
   numPasses.Add(_("Light"));
   numPasses.Add(_("Moderate"));
   /* i18n-hint: Of strength of an effect.  Strongly.*/
   numPasses.Add(_("Heavy"));
   numPasses.Add(_("Heavier"));
   numPasses.Add(_("Heaviest"));

   S.SetBorder(5);
   S.AddSpace(5);

   S.StartMultiColumn(2);
   {
      S.TieChoice(_("Degree of Leveling:"),
                  mLevellerNumPassesChoiceIndex,
                  &numPasses);
      S.TieChoice(_("Noise Threshold:"),
                  mLevellerDbChoiceIndex,
                  &db);
   }
   S.EndMultiColumn();
}

void LevellerDialog::OnPreview(wxCommandEvent & WXUNUSED(event))
{
   TransferDataFromWindow();

   // Save & restore parameters around Preview.
   int oldLevellerDbChoiceIndex = mEffect->mLevellerDbChoiceIndex;
   int oldLevellerNumPasses = mEffect->mLevellerNumPasses;

   mEffect->mLevellerDbChoiceIndex = mLevellerDbChoiceIndex;
   mEffect->mLevellerNumPasses = mLevellerNumPassesChoiceIndex+1;

   mEffect->Preview();

   mEffect->mLevellerDbChoiceIndex = oldLevellerDbChoiceIndex;
   mEffect->mLevellerNumPasses = oldLevellerNumPasses;
}
