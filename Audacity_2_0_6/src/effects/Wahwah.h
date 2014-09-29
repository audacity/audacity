/**********************************************************************

  Audacity: A Digital Audio Editor

  Wahwah

  Effect programming:
  Nasca Octavian Paul (Paul Nasca)

  UI programming:
  Dominic Mazzoni (with the help of wxDesigner)
  Vaughan Johnson (Preview)

**********************************************************************/

#ifndef __AUDACITY_EFFECT_WAHWAH__
#define __AUDACITY_EFFECT_WAHWAH__

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#ifndef WX_PRECOMP
#include <wx/window.h>
#endif

#include <wx/dialog.h>
#include <wx/slider.h>

class wxString;
class wxSizer;
class wxTextCtrl;

#include "SimpleMono.h"

class EffectWahwah:public EffectSimpleMono {

 public:
   EffectWahwah();

   virtual wxString GetEffectName() {
      return wxString(_("Wahwah..."));
   }

   virtual std::set<wxString> GetEffectCategories() {
      std::set<wxString> result;
      result.insert(wxT("http://lv2plug.in/ns/lv2core#ModulatorPlugin"));
      return result;
   }

   virtual wxString GetEffectIdentifier() {
      return wxString(wxT("Wahwah"));
   }

   virtual wxString GetEffectAction() {
      return wxString(_("Applying Wahwah"));
   }

   // Useful only after PromptUser values have been set.
   virtual wxString GetEffectDescription();

   virtual bool PromptUser();
   virtual bool TransferParameters( Shuttle & shuttle );

 protected:
   virtual bool NewTrackSimpleMono();

   virtual bool ProcessSimpleMono(float *buffer, sampleCount len);

   float phase;
   float lfoskip;
   unsigned long skipcount;
   float xn1, xn2, yn1, yn2;
   float b0, b1, b2, a0, a1, a2;

/* Parameters:
   freq - LFO frequency
   startphase - LFO startphase in RADIANS - usefull for stereo WahWah
   depth - Wah depth
   freqofs - Wah frequency offset
   res - Resonance

   !!!!!!!!!!!!! IMPORTANT!!!!!!!!! :
   depth and freqofs should be from 0(min) to 1(max) !
   res should be greater than 0 !  */

 private:
   float freq, startphase;
   float depth, freqofs, res;

friend class WahwahDialog;
};

// Declare window functions

#define ID_FREQTEXT 10003
#define ID_FREQSLIDER 10004
#define ID_PHASETEXT 10005
#define ID_PHASESLIDER 10006
#define ID_DEPTHTEXT 10007
#define ID_DEPTHSLIDER 10008
#define ID_RESONANCETEXT 10009
#define ID_RESONANCESLIDER 10010
#define ID_FREQOFFTEXT 10011
#define ID_FREQOFFSLIDER 10012

// WDR: class declarations

//----------------------------------------------------------------------------
// WahwahDialog
//----------------------------------------------------------------------------

class WahwahDialog:public EffectDialog {
 public:
   // constructors and destructors
   WahwahDialog(EffectWahwah * effect, wxWindow * parent);

   void PopulateOrExchange(ShuttleGui & S);
   bool TransferDataToWindow();
   bool TransferDataFromWindow();

   // WDR: method declarations for WahwahDialog
   wxSlider *GetResonanceSlider() {
      return (wxSlider *) FindWindow(ID_RESONANCESLIDER);
   } wxSlider *GetDepthSlider() {
      return (wxSlider *) FindWindow(ID_DEPTHSLIDER);
   }
   wxSlider *GetPhaseSlider() {
      return (wxSlider *) FindWindow(ID_PHASESLIDER);
   }
   wxSlider *GetFreqSlider() {
      return (wxSlider *) FindWindow(ID_FREQSLIDER);
   }
   wxSlider *GetFreqOffSlider() {
      return (wxSlider *) FindWindow(ID_FREQOFFSLIDER);
   }
   wxTextCtrl *GetResonanceText() {
      return (wxTextCtrl *) FindWindow(ID_RESONANCETEXT);
   }
   wxTextCtrl *GetDepthText() {
      return (wxTextCtrl *) FindWindow(ID_DEPTHTEXT);
   }
   wxTextCtrl *GetPhaseText() {
      return (wxTextCtrl *) FindWindow(ID_PHASETEXT);
   }
   wxTextCtrl *GetFreqText() {
      return (wxTextCtrl *) FindWindow(ID_FREQTEXT);
   }
   wxTextCtrl *GetFreqOffText() {
      return (wxTextCtrl *) FindWindow(ID_FREQOFFTEXT);
   }

 private:
   // WDR: member variable declarations for WahwahDialog

 private:
   // WDR: handler declarations for WahwahDialog
   void OnResonanceSlider(wxCommandEvent & event);
   void OnDepthSlider(wxCommandEvent & event);
   void OnPhaseSlider(wxCommandEvent & event);
   void OnFreqSlider(wxCommandEvent & event);
   void OnFreqOffSlider(wxCommandEvent & event);
   void OnResonanceText(wxCommandEvent & event);
   void OnDepthText(wxCommandEvent & event);
   void OnPhaseText(wxCommandEvent & event);
   void OnFreqText(wxCommandEvent & event);
   void OnFreqOffText(wxCommandEvent & event);
   void OnPreview(wxCommandEvent &event);

 private:
   EffectWahwah * mEffect;

 public:
   float freq;
   float freqoff;
   float startphase;
   float res;
   float depth;

 private:
   DECLARE_EVENT_TABLE()
};

#endif

