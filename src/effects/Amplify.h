/**********************************************************************

  Audacity: A Digital Audio Editor

  Amplify.h

  Dominic Mazzoni

  This rewritten class supports a smart Amplify effect - it calculates
  the maximum amount of gain that can be applied to all tracks without
  causing clipping and selects this as the default parameter.

**********************************************************************/

#ifndef __AUDACITY_EFFECT_AMPLIFY__
#define __AUDACITY_EFFECT_AMPLIFY__

#include <wx/button.h>
#include <wx/checkbox.h>
#include <wx/dialog.h>
#include <wx/intl.h>
#include <wx/slider.h>
#include <wx/textctrl.h>

#include "SimpleMono.h"

class WaveTrack;

class EffectAmplify:public EffectSimpleMono
{
 friend class AmplifyDialog;

 public:
   EffectAmplify();

   virtual wxString GetEffectName() {
      return wxString(_("Amplify..."));
   }

   virtual std::set<wxString> GetEffectCategories() {
      std::set<wxString> result;
      result.insert(wxT("http://lv2plug.in/ns/lv2core#AmplifierPlugin"));
      return result;
   }

   virtual wxString GetEffectIdentifier() {
      return wxString(wxT("Amplify"));
   }

   virtual wxString GetEffectAction() {
      return wxString(_("Amplifying"));
   }

   // Useful only after PromptUser values have been set.
   virtual wxString GetEffectDescription();

   virtual bool Init();

   virtual bool PromptUser();
   virtual bool TransferParameters( Shuttle & shuttle );

 protected:
   virtual bool ProcessSimpleMono(float *buffer, sampleCount len);

 private:
   float ratio;
   float peak;
};

//----------------------------------------------------------------------------
// AmplifyDialog
//----------------------------------------------------------------------------

class AmplifyDialog:public EffectDialog
{
 public:
   // constructors and destructors
   AmplifyDialog(EffectAmplify *effect, wxWindow * parent);

   // method declarations
   void PopulateOrExchange(ShuttleGui & S);
   bool TransferDataToWindow();
   bool TransferDataFromWindow();
   bool Validate();

private:
   // handlers
   void OnAmpText(wxCommandEvent & event);
   void OnPeakText(wxCommandEvent & event);
   void OnAmpSlider(wxCommandEvent & event);
   void OnClipCheckBox(wxCommandEvent & event);
   void OnPreview( wxCommandEvent &event );

   void CheckClip();

 private:
   wxSlider *mAmpS;
   wxTextCtrl *mAmpT;
   wxTextCtrl *mPeakT;
   wxCheckBox *mClip;

   DECLARE_EVENT_TABLE();

 public:
   EffectAmplify *mEffect;

   float ratio;
   float peak;
   bool noclip;
};

#endif // __AUDACITY_EFFECT_AMPLIFY__
