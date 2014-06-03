/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2013 Audacity Team.
   License: GPL v2.  See License.txt.

   BassTreble.h (two shelf filters)
   Steve Daulton

**********************************************************************/

#ifndef __AUDACITY_EFFECT_BASS_TREBLE__
#define __AUDACITY_EFFECT_BASS_TREBLE__

#include "TwoPassSimpleMono.h"

class wxSizer;
class wxTextCtrl;
class WaveTrack;

class EffectBassTreble: public EffectTwoPassSimpleMono {

public:
   EffectBassTreble();
   virtual ~EffectBassTreble() {};

   virtual wxString GetEffectName() {
      return wxString(_("Bass and Treble..."));
   }

   virtual std::set<wxString> GetEffectCategories() {
      std::set<wxString> result;
      result.insert(wxT("http://lv2plug.in/ns/lv2core#EQPlugin"));
      return result;
   }

   virtual wxString GetEffectIdentifier() {
      return wxString(wxT("Bass and Treble"));
   }

   virtual wxString GetEffectAction() {
      return wxString(_("Adjusting Bass and Treble"));
   }

   // Useful only after PromptUser values have been set.
   virtual wxString GetEffectDescription();

protected:
   virtual bool PromptUser();
   virtual bool TransferParameters( Shuttle & shuttle );
   virtual bool Init();

   virtual bool ProcessPass1(float *buffer, sampleCount len);
   virtual bool ProcessPass2(float *buffer, sampleCount len);

   void Coefficents(double hz, float slope, double gain, int type,
                    float& a0, float& a1, float& a2, float& b0, float& b1, float& b2);

private:
   virtual bool NewTrackPass1();
   virtual bool InitPass1();
   virtual bool InitPass2();
   float  DoFilter(float in);

   float xn1Bass, xn2Bass, yn1Bass, yn2Bass,
         wBass, swBass, cwBass, aBass, bBass,
         a0Bass, a1Bass, a2Bass, b0Bass, b1Bass, b2Bass;
   // High shelf
   float xn1Treble, xn2Treble, yn1Treble, yn2Treble,
         wTreble, swTreble, cwTreble, aTreble, bTreble,
         b0Treble, b1Treble, b2Treble, a0Treble, a1Treble, a2Treble;

   double dB_bass, dB_treble, dB_level;
   double mMax;
   bool   mbNormalize;
   double mPreGain;

   friend class BassTrebleDialog;
};

//----------------------------------------------------------------------------
// BassTrebleDialog
//----------------------------------------------------------------------------
class BassTrebleDialog:public EffectDialog {
public:
   BassTrebleDialog(EffectBassTreble *effect, wxWindow * parent);
   virtual ~BassTrebleDialog() {};

   // method declarations for BassTrebleDialog
   void PopulateOrExchange(ShuttleGui & S);
   bool TransferDataToWindow();
   bool TransferDataFromWindow();

private:
   // handler declarations for BassTrebleDialog
   void OnBassText(wxCommandEvent & event);
   void OnTrebleText(wxCommandEvent & event);
   void OnLevelText(wxCommandEvent & event);
   void OnBassSlider(wxCommandEvent & event);
   void OnTrebleSlider(wxCommandEvent & event);
   void OnLevelSlider(wxCommandEvent & event);
   void OnNormalize(wxCommandEvent& evt);
   void UpdateUI();
   void OnPreview(wxCommandEvent & event);
   void set_properties();

private:
   wxSlider *mBassS;
   wxSlider *mTrebleS;
   wxSlider *mLevelS;
   wxTextCtrl *mBassT;
   wxTextCtrl *mTrebleT;
   wxTextCtrl *mLevelT;
   wxCheckBox *mNormalizeCheckBox;
   wxStaticText* mWarning;

public:
   EffectBassTreble *mEffect;

   double bass;
   double treble;
   double level;
   bool mbNormalize;

   DECLARE_EVENT_TABLE()
};

#endif
