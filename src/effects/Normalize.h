/**********************************************************************

  Audacity: A Digital Audio Editor

  Normalize.h

  Dominic Mazzoni
  Vaughan Johnson (Preview)

**********************************************************************/

#ifndef __AUDACITY_EFFECT_NORMALIZE__
#define __AUDACITY_EFFECT_NORMALIZE__

#include <wx/checkbox.h>
#include <wx/event.h>
#include <wx/stattext.h>
#include <wx/string.h>
#include <wx/textctrl.h>

#include "../ShuttleGui.h"
#include "../WaveTrack.h"

#include "Effect.h"

#define NORMALIZE_PLUGIN_SYMBOL XO("Normalize")

class EffectNormalize : public Effect
{
public:
   EffectNormalize();
   virtual ~EffectNormalize();

   // IdentInterface implementation

   virtual wxString GetSymbol();
   virtual wxString GetDescription();

   // EffectIdentInterface implementation

   virtual EffectType GetType();

   // EffectClientInterface implementation

   virtual bool GetAutomationParameters(EffectAutomationParameters & parms);
   virtual bool SetAutomationParameters(EffectAutomationParameters & parms);

   // Effect implementation

   virtual bool CheckWhetherSkipEffect();
   virtual bool Startup();
   virtual bool Process();
   virtual void PopulateOrExchange(ShuttleGui & S);
   virtual bool TransferDataToWindow();
   virtual bool TransferDataFromWindow();

private:
   // EffectNormalize implementation

   bool ProcessOne(WaveTrack * t, wxString msg);
   virtual void AnalyseTrack(WaveTrack * track, wxString msg);
   virtual void AnalyzeData(float *buffer, sampleCount len);
   bool AnalyseDC(WaveTrack * track, wxString msg);
   virtual void ProcessData(float *buffer, sampleCount len);

   void OnUpdateUI(wxCommandEvent & evt);
   void UpdateUI();

private:
   double mLevel;
   bool   mGain;
   bool   mDC;
   bool   mStereoInd;

   int    mCurTrackNum;
   double mCurT0;
   double mCurT1;
   float  mMult;
   float  mOffset;
   float  mMin;
   float  mMax;
   double mSum;
   sampleCount    mCount;

   wxCheckBox *mGainCheckBox;
   wxCheckBox *mDCCheckBox;
   wxTextCtrl *mLevelTextCtrl;
   wxStaticText *mLeveldB;
   wxStaticText *mWarning;
   wxCheckBox *mStereoIndCheckBox;

   bool mCreating;

   DECLARE_EVENT_TABLE();
};

#endif
