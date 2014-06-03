/**********************************************************************

  Audacity: A Digital Audio Editor

  ChangeSpeed.h

  Vaughan Johnson, Dominic Mazzoni

  Change Speed effect, that affects both pitch & tempo.

**********************************************************************/

#ifndef __AUDACITY_EFFECT_CHANGESPEED__
#define __AUDACITY_EFFECT_CHANGESPEED__

#include "Effect.h"
#include "../Resample.h"

#include <wx/choice.h>
#include <wx/dialog.h>
#include <wx/intl.h>
#include <wx/slider.h>
#include <wx/string.h>
#include <wx/textctrl.h>

class EffectChangeSpeed : public Effect
{
 public:
   EffectChangeSpeed();

   virtual wxString GetEffectName() {
      return wxString(_("Change Speed..."));
   }

   virtual std::set<wxString> GetEffectCategories() {
      std::set<wxString> result;
      result.insert(wxT("http://audacityteam.org/namespace#PitchAndTempo"));
      return result;
   }

   virtual wxString GetEffectIdentifier() {
      return wxString(wxT("ChangeSpeed"));
   }

   virtual wxString GetEffectAction() {
      return wxString(_("Changing Speed"));
   }

   // Useful only after PromptUser values have been set.
   virtual wxString GetEffectDescription();

   double CalcPreviewInputLength(double previewLength);

 protected:
   virtual bool PromptUser();
   virtual bool TransferParameters( Shuttle & shuttle );

   virtual bool CheckWhetherSkipEffect() { return (m_PercentChange == 0.0); }
   virtual bool Process();

 private:
   bool ProcessOne(WaveTrack * t, sampleCount start, sampleCount end);
   bool ProcessLabelTrack(Track *t);

 private:
   // track related
   int    mCurTrackNum;
   double mMaxNewLength;
   double mCurT0;
   double mCurT1;

   // control values
   double   m_PercentChange;  // percent change to apply to tempo
                              // -100% is meaningless, but sky's the upper limit.
                              // Slider is (-100, 200], but textCtrls can set higher.
   int      mFromVinyl;       // from standard vinyl speed (RPM) enum
   int      mToVinyl;         // to standard vinyl speed (RPM) enum
   double   mFactor;          // scale factor calculated from percent change

friend class ChangeSpeedDialog;
};


class ChangeSpeedDialog : public EffectDialog
{
 public:
   ChangeSpeedDialog(EffectChangeSpeed * effect,
                     wxWindow * parent);

   void PopulateOrExchange(ShuttleGui& S);
   bool TransferDataToWindow();
   bool TransferDataFromWindow();

 private:
   // handlers
   void OnText_PercentChange(wxCommandEvent & event);
   void OnSlider_PercentChange(wxCommandEvent & event);
   void OnChoice_FromVinyl(wxCommandEvent & event);
   void OnChoice_ToVinyl(wxCommandEvent & event);

   void OnPreview(wxCommandEvent &event);

   // helper fns
   void Update_Text_PercentChange();   // Update control per current m_PercentChange.
   void Update_Slider_PercentChange(); // Update control per current m_PercentChange.
   void Update_Vinyl();                // Update Vinyl controls for new percent change.
   void Update_PercentChange();        // Update percent change controls for new Vinyl values.

 private:
   EffectChangeSpeed * mEffect;
   bool mbLoopDetect;

   // controls
   wxTextCtrl *	mpTextCtrl_PercentChange;
   wxSlider *		mpSlider_PercentChange;
   wxChoice *		mpChoice_FromVinyl;
   wxChoice *		mpChoice_ToVinyl;

 public:
   // effect parameters
   double   m_PercentChange;   // percent change to apply to tempo
                              // -100% is meaningless, but sky's the upper limit.
                              // Slider is (-100, 200], but textCtrls can set higher.
   int      mFromVinyl;       // from standard vinyl speed (RPM)
   int      mToVinyl;         // to standard vinyl speed (RPM)

 private:
   DECLARE_EVENT_TABLE()
};


#endif // __AUDACITY_EFFECT_CHANGESPEED__

