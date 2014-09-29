/**********************************************************************

  Audacity: A Digital Audio Editor

  Echo.h

  Dominic Mazzoni
  Vaughan Johnson (dialog)

**********************************************************************/

#ifndef __AUDACITY_EFFECT_ECHO__
#define __AUDACITY_EFFECT_ECHO__

class wxString;

#include <wx/dialog.h>

#include <wx/intl.h>

#include "Effect.h"

class wxStaticText;

class WaveTrack;

class EffectEcho:public Effect {

 public:

   EffectEcho();

   virtual wxString GetEffectName() {
      return wxString(_("Echo..."));
   }

   virtual std::set<wxString> GetEffectCategories() {
      std::set<wxString> result;
      result.insert(wxT("http://lv2plug.in/ns/lv2core#DelayPlugin"));
      return result;
   }

   virtual wxString GetEffectIdentifier() {
      return wxString(wxT("Echo"));
   }

   virtual wxString GetEffectAction() {
      return wxString(_("Performing Echo"));
   }

   // Useful only after PromptUser values have been set.
   virtual wxString GetEffectDescription();

   virtual bool PromptUser();
   virtual bool TransferParameters( Shuttle & shuttle );

   virtual bool Process();

 private:
   bool ProcessOne(int count, WaveTrack * t,
                   sampleCount start, sampleCount len);

   float delay;
   float decay;

friend class EchoDialog;
};

//----------------------------------------------------------------------------
// EchoDialog
//----------------------------------------------------------------------------

class EchoDialog:public EffectDialog {
 public:
   EchoDialog(EffectEcho * effect, wxWindow * parent);

   void PopulateOrExchange(ShuttleGui & S);
   bool TransferDataToWindow();
   bool TransferDataFromWindow();

 private:
   // handlers
   void OnPreview( wxCommandEvent &event );

 private:
   bool           m_bLoopDetect;
   EffectEcho *   m_pEffect;

   // controls
   wxTextCtrl *   m_pTextCtrl_Delay;
   wxTextCtrl *   m_pTextCtrl_Decay;

 public:
   // effect parameters
   float delay;
   float decay;

 private:
   DECLARE_EVENT_TABLE()
};

#endif // __AUDACITY_EFFECT_ECHO__
