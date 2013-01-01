/**********************************************************************

  Audacity: A Digital Audio Editor

  Bass Boost
  <As of 2012-12-31, this is deprecated in favor of BassTreble.*. 
   Retained in code repository for reference.>

  Effect programming:
  Nasca Octavian Paul

  UI programming:
  Dominic Mazzoni (with the help of wxDesigner)

**********************************************************************/

#ifndef __AUDACITY_EFFECT_BASS_BOOST__
#define __AUDACITY_EFFECT_BASS_BOOST__

#include "SimpleMono.h"

#include <wx/dialog.h>
#include <wx/intl.h>
#include <wx/slider.h>

class wxSizer;
class wxTextCtrl;

class WaveTrack;

class EffectBassBoost:public EffectSimpleMono {

 public:
   EffectBassBoost();

   virtual wxString GetEffectName() {
      return wxString(_("BassBoost..."));
   }
   
   virtual std::set<wxString> GetEffectCategories() {
      std::set<wxString> result;
      result.insert(wxT("http://lv2plug.in/ns/lv2core#LowpassPlugin"));
      return result;
   }

   virtual wxString GetEffectIdentifier() {
      return wxString(wxT("BassBoost"));
   }
   
   virtual wxString GetEffectAction() {
      return wxString(_("Boosting Bass Frequencies"));
   }
   
   // Useful only after PromptUser values have been set. 
   virtual wxString GetEffectDescription(); 

   virtual bool PromptUser();
   virtual bool TransferParameters( Shuttle & shuttle );
   
 protected:

   virtual bool NewTrackSimpleMono();

   virtual bool ProcessSimpleMono(float *buffer, sampleCount len);
   
   float frequency, dB_boost;
   //filter parameters
   float xn1,xn2,yn1,yn2;
   float omega, sn, cs, a, shape, beta, b0, b1, b2, a0, a1, a2;

   friend class BassBoostDialog;
};

// WDR: class declarations

//----------------------------------------------------------------------------
// BassBoostDialog
//----------------------------------------------------------------------------
class BassBoostDialog:public EffectDialog {
 public:
   // constructors and destructors
   BassBoostDialog(EffectBassBoost *effect, wxWindow * parent);

   // WDR: method declarations for BassBoostDialog
   void PopulateOrExchange(ShuttleGui & S);
   bool TransferDataToWindow();
   bool TransferDataFromWindow();

 private:
   // WDR: handler declarations for BassBoostDialog
   void OnBoostText(wxCommandEvent & event);
   void OnFreqText(wxCommandEvent & event);
   void OnBoostSlider(wxCommandEvent & event);
   void OnFreqSlider(wxCommandEvent & event);
   void OnPreview(wxCommandEvent & event);

 private:
   wxSlider *mBoostS;
   wxSlider *mFreqS;
   wxTextCtrl *mBoostT;
   wxTextCtrl *mFreqT;

   DECLARE_EVENT_TABLE()

 public:
   EffectBassBoost *mEffect;
   
   float freq;
   float boost;

};



#endif

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: b45174f7-ee03-4245-8de5-d75e16bd7009

