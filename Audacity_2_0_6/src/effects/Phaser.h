/**********************************************************************

  Audacity: A Digital Audio Editor

  Phaser

  Effect programming:
  Nasca Octavian Paul (Paul Nasca)

  UI programming:
  Dominic Mazzoni (with the help of wxDesigner)
  Vaughan Johnson (Preview)

**********************************************************************/

#ifndef __AUDACITY_EFFECT_PHASER__
#define __AUDACITY_EFFECT_PHASER__

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#ifndef WX_PRECOMP
#include <wx/window.h>
#endif

#include <wx/dialog.h>
#include <wx/intl.h>
#include <wx/slider.h>

class wxString;
class wxTextCtrl;
class wxSizer;
class wxSpinCtrl;

#include "SimpleMono.h"

class WaveTrack;

class EffectPhaser:public EffectSimpleMono {

 public:
   EffectPhaser();

   virtual wxString GetEffectName() {
      return wxString(_("Phaser..."));
   }

   virtual std::set<wxString> GetEffectCategories() {
      std::set<wxString> result;
      result.insert(wxT("http://lv2plug.in/ns/lv2core#PhaserPlugin"));
      return result;
   }

   virtual wxString GetEffectIdentifier() {
      return wxString(wxT("Phaser"));
   }

   virtual wxString GetEffectAction() {
      return wxString(_("Applying Phaser"));
   }

   // Useful only after PromptUser values have been set.
   virtual wxString GetEffectDescription();

   virtual bool PromptUser();
   virtual bool TransferParameters( Shuttle & shuttle );

 protected:
   virtual bool NewTrackSimpleMono();

   virtual bool ProcessSimpleMono(float *buffer, sampleCount len);

/*
    Phaser Parameters

 freq       - Phaser's LFO frequency
 startphase - Phaser's LFO startphase (radians), needed for stereo Phasers
 depth      - Phaser depth (0 - no depth, 255 - max depth)
 stages     - Phaser stages (recomanded from 2 to 16-24, and EVEN NUMBER)
 drywet     - Dry/wet mix, (0 - dry, 128 - dry=wet, 255 - wet)
 fb         - Phaser FeedBack (0 - no feedback, 100 = 100% Feedback,
                               -100 = -100% FeedBack)
*/

 private:

   // parameters
   float freq;
   float startphase;
   float fb;
   int depth;
   int stages;
   int drywet;

   // state variables
   unsigned long skipcount;
   float old[24]; // must be as large as MAX_STAGES
   float gain;
   float fbout;
   float lfoskip;
   float phase;

friend class PhaserDialog;
};

// Declare window functions

#define ID_PHASER_STAGESTEXT 12001
#define ID_PHASER_STAGESSLIDER 12002
#define ID_PHASER_DRYWETTEXT 12003
#define ID_PHASER_DRYWETSLIDER 12004
#define ID_PHASER_FREQTEXT 12005
#define ID_PHASER_FREQSLIDER 12006
#define ID_PHASER_PHASETEXT 12007
#define ID_PHASER_PHASESLIDER 12008
#define ID_PHASER_DEPTHTEXT 12009
#define ID_PHASER_DEPTHSLIDER 12010
#define ID_PHASER_FEEDBACKTEXT 12011
#define ID_PHASER_FEEDBACKSLIDER 12012

//----------------------------------------------------------------------------
// PhaserDialog
//----------------------------------------------------------------------------

class PhaserDialog:public EffectDialog {
 public:
   // constructors and destructors
   PhaserDialog(EffectPhaser * effect, wxWindow * parent);

   void PopulateOrExchange(ShuttleGui & S);
   bool TransferDataToWindow();
   bool TransferDataFromWindow();

   wxSlider *GetStagesSlider() {
      return (wxSlider *) FindWindow(ID_PHASER_STAGESSLIDER);
   }
   wxSlider *GetDryWetSlider() {
      return (wxSlider *) FindWindow(ID_PHASER_DRYWETSLIDER);
   }
   wxSlider *GetFeedbackSlider() {
      return (wxSlider *) FindWindow(ID_PHASER_FEEDBACKSLIDER);
   }
   wxSlider *GetDepthSlider() {
      return (wxSlider *) FindWindow(ID_PHASER_DEPTHSLIDER);
   }
   wxSlider *GetPhaseSlider() {
      return (wxSlider *) FindWindow(ID_PHASER_PHASESLIDER);
   }
   wxSlider *GetFreqSlider() {
      return (wxSlider *) FindWindow(ID_PHASER_FREQSLIDER);
   }
   wxTextCtrl *GetStagesText() {
      return (wxTextCtrl *) FindWindow(ID_PHASER_STAGESTEXT);
   }
   wxTextCtrl *GetDryWetText() {
      return (wxTextCtrl *) FindWindow(ID_PHASER_DRYWETTEXT);
   }
   wxTextCtrl *GetFeedbackText() {
      return (wxTextCtrl *) FindWindow(ID_PHASER_FEEDBACKTEXT);
   }
   wxTextCtrl *GetDepthText() {
      return (wxTextCtrl *) FindWindow(ID_PHASER_DEPTHTEXT);
   }
   wxTextCtrl *GetPhaseText() {
      return (wxTextCtrl *) FindWindow(ID_PHASER_PHASETEXT);
   }
   wxTextCtrl *GetFreqText() {
      return (wxTextCtrl *) FindWindow(ID_PHASER_FREQTEXT);
   }

 private:
   // WDR: handler declarations for PhaserDialog
   void OnStagesSlider(wxCommandEvent & event);
   void OnDryWetSlider(wxCommandEvent & event);
   void OnFeedbackSlider(wxCommandEvent & event);
   void OnDepthSlider(wxCommandEvent & event);
   void OnPhaseSlider(wxCommandEvent & event);
   void OnFreqSlider(wxCommandEvent & event);
   void OnStagesText(wxCommandEvent & event);
   void OnDryWetText(wxCommandEvent & event);
   void OnFeedbackText(wxCommandEvent & event);
   void OnDepthText(wxCommandEvent & event);
   void OnPhaseText(wxCommandEvent & event);
   void OnFreqText(wxCommandEvent & event);
   void OnPreview(wxCommandEvent &event);

 private:
   EffectPhaser * mEffect;

 public:
   float freq;
   float startphase;
   float fb;

   int depth;
   int stages;
   int drywet;

 private:
   DECLARE_EVENT_TABLE()
};

#endif
