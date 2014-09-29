/**********************************************************************

  Audacity: A Digital Audio Editor

  Normalize.h

  Dominic Mazzoni
  Vaughan Johnson (Preview)

**********************************************************************/

#ifndef __AUDACITY_EFFECT_NORMALIZE__
#define __AUDACITY_EFFECT_NORMALIZE__

#include "Effect.h"

#include <wx/checkbox.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>

class wxString;

class WaveTrack;

class EffectNormalize: public Effect
{
 friend class NormalizeDialog;

 public:
   EffectNormalize();

   virtual wxString GetEffectName() {
      return wxString(_("Normalize..."));
   }

   virtual std::set<wxString> GetEffectCategories() {
      std::set<wxString> result;
      result.insert(wxT("http://lv2plug.in/ns/lv2core#UtilityPlugin"));
      result.insert(wxT("http://lv2plug.in/ns/lv2core#AmplifierPlugin"));
      return result;
   }

   // This is just used internally, users should not see it.  Do not translate.
   virtual wxString GetEffectIdentifier() {
      return wxT("Normalize");
   }

   virtual wxString GetEffectAction() {
      return wxString(_("Normalizing..."));
   }

   virtual wxString GetEffectDescription(); // useful only after parameter values have been set

   virtual bool PromptUser();
   virtual bool TransferParameters( Shuttle & shuttle );

   virtual bool Init();
   virtual void End();
   virtual bool CheckWhetherSkipEffect();
   virtual bool Process();

 private:
   bool ProcessOne(WaveTrack * t, wxString msg);
   virtual void AnalyseTrack(WaveTrack * track, wxString msg);
   virtual void AnalyzeData(float *buffer, sampleCount len);
   bool AnalyseDC(WaveTrack * track, wxString msg);
   virtual void ProcessData(float *buffer, sampleCount len);

   bool   mGain;
   bool   mDC;
   double mLevel;
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
};

//----------------------------------------------------------------------------
// NormalizeDialog
//----------------------------------------------------------------------------

class NormalizeDialog: public EffectDialog
{
 public:
   // constructors and destructors
   NormalizeDialog(EffectNormalize *effect, wxWindow * parent);

   // method declarations
   void PopulateOrExchange(ShuttleGui & S);
   bool TransferDataToWindow();
   bool TransferDataFromWindow();

 private:
   // handlers
   void OnUpdateUI(wxCommandEvent& evt);
   void OnPreview(wxCommandEvent &event);

   void UpdateUI();

 private:
   EffectNormalize *mEffect;
   wxCheckBox *mGainCheckBox;
   wxCheckBox *mDCCheckBox;
   wxTextCtrl *mLevelTextCtrl;
   wxStaticText *mLeveldB;
   wxStaticText *mWarning;
   wxCheckBox *mStereoIndCheckBox;

   DECLARE_EVENT_TABLE()

 public:
   bool mGain;
   bool mDC;
   double mLevel;
   bool mStereoInd;
};

#endif
