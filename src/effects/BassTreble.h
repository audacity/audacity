/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2013 Audacity Team.
   License: GPL v2.  See License.txt.

   BassTreble.h (two shelf filters)
   Steve Daulton

**********************************************************************/

#ifndef __AUDACITY_EFFECT_BASS_TREBLE__
#define __AUDACITY_EFFECT_BASS_TREBLE__

#include "SimpleMono.h"

#include <wx/dialog.h>
#include <wx/intl.h>
#include <wx/slider.h>

class wxSizer;
class wxTextCtrl;

class WaveTrack;

class EffectBassTreble:public EffectSimpleMono {

 public:
   EffectBassTreble();

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

   virtual bool PromptUser();
   virtual bool TransferParameters( Shuttle & shuttle );
   
 protected:

   virtual bool NewTrackSimpleMono();
   virtual bool ProcessSimpleMono(float *buffer, sampleCount len);

 private: 
   /* filter co-efficent values */
   // Low shelf
   float xn1Bass, xn2Bass, yn1Bass, yn2Bass,
         wBass, swBass, cwBass, aBass, bBass,
         a0Bass, a1Bass, a2Bass, b0Bass, b1Bass, b2Bass;
   // High shelf
   float xn1Treble, xn2Treble, yn1Treble, yn2Treble,
         wTreble, swTreble, cwTreble, aTreble, bTreble,
         b0Treble, b1Treble, b2Treble, a0Treble, a1Treble, a2Treble;

   double dB_bass, dB_treble, dB_gain;

   friend class BassTrebleDialog;
};

// class declarations

//----------------------------------------------------------------------------
// BassTrebleDialog
//----------------------------------------------------------------------------
class BassTrebleDialog:public EffectDialog {
 public:
   // constructors and destructors
   BassTrebleDialog(EffectBassTreble *effect, wxWindow * parent);

   // method declarations for BassTrebleDialog
   void PopulateOrExchange(ShuttleGui & S);
   bool TransferDataToWindow();
   bool TransferDataFromWindow();

 private:
   // handler declarations for BassTrebleDialog
   void OnBassText(wxCommandEvent & event);
   void OnTrebleText(wxCommandEvent & event);
   void OnGainText(wxCommandEvent & event);
   void OnBassSlider(wxCommandEvent & event);
   void OnTrebleSlider(wxCommandEvent & event);
   void OnGainSlider(wxCommandEvent & event);
   void OnPreview(wxCommandEvent & event);

 private:
   wxSlider *mBassS;
   wxSlider *mTrebleS;
   wxSlider *mGainS;
   wxTextCtrl *mBassT;
   wxTextCtrl *mTrebleT;
   wxTextCtrl *mGainT;

   DECLARE_EVENT_TABLE()

 public:
   EffectBassTreble *mEffect;

   float bass;
   float treble;
   float gain;

};

#endif
