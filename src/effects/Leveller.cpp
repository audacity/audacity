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
#ifdef CLEANSPEECH
   mLevellerNumPasses = gPrefs->Read(wxT("/CsPresets/LevellerNumPasses"), 2L) ;
   if ((mLevellerNumPasses <= 0) || (mLevellerNumPasses >= NUM_PASSES_CHOICES)) {  // corrupted Prefs?
      mLevellerNumPasses = 1;
      gPrefs->Write(wxT("/CsPresets/LevellerNumPasses"), 1);
   }
   mLevellerDbChoiceIndex = gPrefs->Read(wxT("/CsPresets/LevellerDbChoiceIndex"), 10L);
   if ((mLevellerDbChoiceIndex < 0) || (mLevellerDbChoiceIndex >= Enums::NumDbChoices)) {  // corrupted Prefs?
      mLevellerDbChoiceIndex = 0;  //Least dB
      gPrefs->Write(wxT("/CsPresets/LevellerDbChoiceIndex"), mLevellerDbChoiceIndex);
   }
#else   // CLEANSPEECH
   mLevellerNumPasses = gPrefs->Read(wxT("/Effects/Leveller/LevellerNumPasses"), 2L) ;
   if ((mLevellerNumPasses <= 0) || (mLevellerNumPasses >= NUM_PASSES_CHOICES)) {  // corrupted Prefs?
      mLevellerNumPasses = 1;
      gPrefs->Write(wxT("/Effects/Leveller/LevellerNumPasses"), 1);
   }
   mLevellerDbChoiceIndex = gPrefs->Read(wxT("/Effects/Leveller/LevellerDbChoiceIndex"), 10L);
   if ((mLevellerDbChoiceIndex < 0) || (mLevellerDbChoiceIndex >= Enums::NumDbChoices)) {  // corrupted Prefs?
      mLevellerDbChoiceIndex = 0;  //Least dB
      gPrefs->Write(wxT("/Effects/Leveller/LevellerDbChoiceIndex"), mLevellerDbChoiceIndex);
   }
#endif   // CLEANSPEECH
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
   double lowerAdjLimit = 0.0;
   double adjFactor     = gAdjFactor[0];
   double upperAdjLimit = gLimit[0] * adjFactor;
   double prevAdjLimit  = upperAdjLimit;
   gAdjLimit[0]         = upperAdjLimit;

   for (int f = 1; f < LEVELER_FACTORS; ++f) {
      prev          = f - 1;
      adjFactor     = gAdjFactor[f];
      lowerAdjLimit = gAdjLimit[prev];
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
   dlog.mLevellerNumPassesChoicIndex = mLevellerNumPasses-1;
   dlog.TransferDataToWindow();

   dlog.CentreOnParent();
   dlog.ShowModal();

   if (dlog.GetReturnCode() == wxID_CANCEL) {
      return false;
   }

   mLevellerNumPasses = dlog.mLevellerNumPassesChoicIndex+1;
   mLevellerDbChoiceIndex = dlog.mLevellerDbChoiceIndex;
   mLevellerDbSilenceThreshold = Enums::Db2Signal[mLevellerDbChoiceIndex];
#ifdef CLEANSPEECH
   gPrefs->Write(wxT("/CsPresets/LevellerDbChoiceIndex"), mLevellerDbChoiceIndex);
   gPrefs->Write(wxT("/CsPresets/LevellerNumPasses"), mLevellerNumPasses);
#else   // CLEANSPEECH
   gPrefs->Write(wxT("/Effects/Leveller/LevellerDbChoiceIndex"), mLevellerDbChoiceIndex);
   gPrefs->Write(wxT("/Effects/Leveller/LevellerNumPasses"), mLevellerNumPasses);
#endif   // CLEANSPEECH
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
:  EffectDialog(parent, _("Leveller"), PROCESS_EFFECT),
   mEffect(effect)
{
   mLevellerNumPassesChoicIndex = 0;// 
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

   S.StartHorizontalLay(wxCENTER, false);
   {
      S.AddTitle(_("by Lynn Allan"));
   }
   S.EndHorizontalLay();

   S.StartHorizontalLay(wxCENTER, false);
   {
      // Add a little space
   }
   S.EndHorizontalLay();

   S.StartStatic(_("Degree of Leveling"));
   {
      S.StartHorizontalLay();
      {
         S.TieChoice(_("Degree of Leveling:"),
                     mLevellerNumPassesChoicIndex,
                     &numPasses);
      }
      S.EndHorizontalLay();
   }
   S.EndStatic();
                                              
   S.StartStatic(_("Noise Threshold (Hiss/Hum/Ambient Noise)"));
   {
      S.StartHorizontalLay();
      {
         S.TieChoice(_("Threshold for Noise:"),
                     mLevellerDbChoiceIndex,
                     &db);
      }
      S.EndHorizontalLay();
   }
   S.EndStatic();
}

void LevellerDialog::OnPreview(wxCommandEvent &event)
{
   TransferDataFromWindow();

	// Save & restore parameters around Preview
   int oldLevellerDbChoiceIndex = mEffect->mLevellerDbChoiceIndex;
   int oldLevellerNumPasses = mEffect->mLevellerNumPasses;

   mEffect->mLevellerDbChoiceIndex = mLevellerDbChoiceIndex;
   mEffect->mLevellerNumPasses = mLevellerNumPassesChoicIndex+1;

   mEffect->Preview();
   
	mEffect->mLevellerDbChoiceIndex = oldLevellerDbChoiceIndex;
   mEffect->mLevellerNumPasses = oldLevellerNumPasses;
}

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 0e9ab1c7-3cb3-4864-8f30-876218bea476

