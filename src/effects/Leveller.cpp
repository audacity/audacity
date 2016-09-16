/**********************************************************************

  Audacity: A Digital Audio Editor

  Leveller.cpp

  Lynn Allan

******************************************************************//**

\class EffectLeveller
\brief An Effect that aims to selectively make softer sounds louder.

*//*******************************************************************/

#include "../Audacity.h"
#include "Leveller.h"

#include <math.h>

#include <wx/choice.h>
#include <wx/intl.h>
#include <wx/valgen.h>

#include "../Prefs.h"
#include "../ShuttleGui.h"

enum kPasses
{
   kLight,
   kModerate,
   kHeavy,
   kHeavier,
   kHeaviest,
   kNumPasses
};

static const wxString kPassStrings[kNumPasses] =
{
   /* i18n-hint: Of strength of an effect.  Not strongly.*/
   XO("Light"),
   XO("Moderate"),
   /* i18n-hint: Of strength of an effect.  Strongly.*/
   XO("Heavy"),
   XO("Heavier"),
   XO("Heaviest"),
};

// Define keys, defaults, minimums, and maximums for the effect parameters
//
//     Name    Type  Key            Def        Min   Max                        Scale
Param( Level,  int,  XO("dB"),      10,        0,    Enums::NumDbChoices - 1,   1  );
Param( Passes, int,  XO("Passes"),  kModerate, 0,    kNumPasses - 1,            1  );

//
// EffectLeveller
//

EffectLeveller::EffectLeveller()
{
   mPassIndex = DEF_Passes;
   mDbIndex = DEF_Level;

   mNumPasses = mPassIndex + 1;
   mDbSilenceThreshold = Enums::Db2Signal[mDbIndex];

   CalcLevellerFactors();
}

EffectLeveller::~EffectLeveller()
{
}

// IdentInterface implementation

wxString EffectLeveller::GetSymbol()
{
   return LEVELLER_PLUGIN_SYMBOL;
}

wxString EffectLeveller::GetDescription()
{
   return XO("A simple, combined compressor and limiter effect for reducing the dynamic range of audio");
}

// EffectIdentInterface implementation

EffectType EffectLeveller::GetType()
{
   return EffectTypeProcess;
}

// EffectClientInterface implementation

unsigned EffectLeveller::GetAudioInCount()
{
   return 1;
}

unsigned EffectLeveller::GetAudioOutCount()
{
   return 1;
}

size_t EffectLeveller::ProcessBlock(float **inBlock, float **outBlock, size_t blockLen)
{
   float *ibuf = inBlock[0];
   float *obuf = outBlock[0];
   
   for (decltype(blockLen) i = 0; i < blockLen; i++)
   {
      float frame = ibuf[i];
      for (int pass = 0; pass < mNumPasses; pass++)
      {
         frame = LevelOneFrame(frame);
      }
      obuf[i] = frame;
   }

   return blockLen;
}

bool EffectLeveller::GetAutomationParameters(EffectAutomationParameters & parms)
{
   parms.Write(KEY_Level, Enums::DbChoices[mDbIndex]);
   parms.Write(KEY_Passes, kPassStrings[mPassIndex]);

   return true;
}

bool EffectLeveller::SetAutomationParameters(EffectAutomationParameters & parms)
{
   // Allow for 2.1.0 and before
   wxArrayString passChoices(kNumPasses, kPassStrings);
   passChoices.Insert(wxT("1"), 0);
   passChoices.Insert(wxT("2"), 1);
   passChoices.Insert(wxT("3"), 2);
   passChoices.Insert(wxT("4"), 3);
   passChoices.Insert(wxT("5"), 4);

   ReadAndVerifyEnum(Level, wxArrayString(Enums::NumDbChoices,Enums::GetDbChoices()));
   ReadAndVerifyEnum(Passes, passChoices);

   mDbIndex = Level;
   mPassIndex = Passes;

   // Readjust for 2.1.0 or before
   if (mPassIndex >= kNumPasses)
   {
      mPassIndex -= kNumPasses;
   }

   mNumPasses = mPassIndex + 1;
   mDbSilenceThreshold = Enums::Db2Signal[mDbIndex];

   CalcLevellerFactors();

   return true;
}

// Effect implementation

bool EffectLeveller::Startup()
{
   wxString base = wxT("/Effects/Leveller/");

   // Migrate settings from 2.1.0 or before

   // Already migrated, so bail
   if (gPrefs->Exists(base + wxT("Migrated")))
   {
      return true;
   }

   // Load the old "current" settings
   if (gPrefs->Exists(base))
   {
      mNumPasses = gPrefs->Read(base + wxT("LevellerNumPasses"), 2L);
      if ((mNumPasses <= 0) || (mNumPasses > kNumPasses))
      {  // corrupted Pr
         mNumPasses = 1;
      }
      mDbIndex = gPrefs->Read(base + wxT("LevellerDbChoiceIndex"), 10L);
      if ((mDbIndex < 0) || (mDbIndex >= Enums::NumDbChoices))
      {  // cor
         mDbIndex = 0;  //Least dB
      }

      SaveUserPreset(GetCurrentSettingsGroup());

      // Do not migrate again
      gPrefs->Write(base + wxT("Migrated"), true);
      gPrefs->Flush();
   }

   return true;
}

void EffectLeveller::PopulateOrExchange(ShuttleGui & S)
{
   wxASSERT(kNumPasses == WXSIZEOF(kPassStrings));

   wxArrayString passChoices;
   for (int i = 0; i < kNumPasses; i++)
   {
      passChoices.Add(wxGetTranslation(kPassStrings[i]));
   }

   wxArrayString dBChoices(Enums::NumDbChoices,Enums::GetDbChoices());

   S.SetBorder(5);

   S.StartVerticalLay();
   {
      S.AddSpace(5);
      S.StartMultiColumn(2, wxALIGN_CENTER);
      {
         S.AddChoice(_("Degree of Leveling:"),
                     wxT(""),
                     &passChoices)->SetValidator(wxGenericValidator(&mPassIndex));
         S.AddChoice(_("Noise Threshold:"),
                     wxT(""),
                     &dBChoices)->SetValidator(wxGenericValidator(&mDbIndex));
      }
      S.EndMultiColumn();
   }
   S.EndVerticalLay();

   return;
}

bool EffectLeveller::TransferDataToWindow()
{
   if (!mUIParent->TransferDataToWindow())
   {
      return false;
   }

   return true;
}

bool EffectLeveller::TransferDataFromWindow()
{
   if (!mUIParent->Validate() || !mUIParent->TransferDataFromWindow())
   {
      return false;
   }

   mNumPasses = mPassIndex + 1;
   mDbSilenceThreshold = Enums::Db2Signal[mDbIndex];

   CalcLevellerFactors();

   return true;
}

// EffectLeveller implementation

#define LEVELER_FACTORS 6
static double gLimit[LEVELER_FACTORS] = { 0.0001, 0.0, 0.1, 0.3, 0.5, 1.0 };
static double gAdjLimit[LEVELER_FACTORS];
static double gAddOnValue[LEVELER_FACTORS];
// static double gAdjFactor[LEVELER_FACTORS] = { 0.9, 1.0, 1.1, 1.1, 1.0, 0.9 };
static double gAdjFactor[LEVELER_FACTORS] = { 0.80, 1.00, 1.20, 1.20, 1.00, 0.80 };

void EffectLeveller::CalcLevellerFactors()
{
   gLimit[1]            = mDbSilenceThreshold;
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

      gAddOnValue[f] = addOnValue;
      gAdjLimit[f]   = (adjFactor * limit) + addOnValue;
   }
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

   for (int f = 0; f < LEVELER_FACTORS; ++f) {
     if (fabsCurFrame <= gLimit[f]) {
        curFrame *= (float)gAdjFactor[f];
        curFrame += (float)(gAddOnValue[f] * curSign);
        return curFrame;
     }
   }
   return (float)0.99;
}

