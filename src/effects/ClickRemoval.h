/**********************************************************************

  Audacity: A Digital Audio Editor

  ClickRemoval.h

  Craig DeForest

  (Structure largely stolen from NoiseRemoval.h by Dominic Mazzoni)

  This file is intended to become part of Audacity.  You may modify and/or
  distribute it under the same terms as Audacity itself.

**********************************************************************/

#ifndef __AUDACITY_EFFECT_CLICK_REMOVAL__
#define __AUDACITY_EFFECT_CLICK_REMOVAL__

#include <wx/bitmap.h>
#include <wx/button.h>
#include <wx/panel.h>
#include <wx/sizer.h>
#include <wx/slider.h>
#include <wx/stattext.h>
#include <wx/intl.h>

class wxString;

#include "Effect.h"

class Envelope;
class WaveTrack;

class EffectClickRemoval: public Effect {

public:

   EffectClickRemoval();
   virtual ~EffectClickRemoval();

   virtual wxString GetEffectName() {
      return wxString(_("Click Removal..."));
   }

   virtual std::set<wxString> GetEffectCategories() {
     std::set<wxString> result;
     result.insert(wxT("http://audacityteam.org/namespace#NoiseRemoval"));
     return result;
   }

   virtual wxString GetEffectIdentifier() {
      return wxString(wxT("ClickRemoval"));
   }

   virtual wxString GetEffectAction() {
         return wxString(_("Removing clicks and pops..."));
   }

   virtual bool PromptUser();
   virtual bool TransferParameters( Shuttle & shuttle );

   virtual bool Init();

   virtual bool CheckWhetherSkipEffect();

   virtual bool Process();

private:
   bool ProcessOne(int count, WaveTrack * track,
                   sampleCount start, sampleCount len);

   bool RemoveClicks(sampleCount len, float *buffer);

   Envelope *mEnvelope;

   bool mbDidSomething; // This effect usually does nothing on real-world data.
   int       windowSize;
   int       mThresholdLevel;
   int       mClickWidth;
   int  sep;

friend class ClickRemovalDialog;
};

// WDR: class declarations

//----------------------------------------------------------------------------
// BassWidthDialog
//----------------------------------------------------------------------------
class ClickRemovalDialog:public EffectDialog {
 public:
   // constructors and destructors
   ClickRemovalDialog(EffectClickRemoval *effect, wxWindow *parent);

   // WDR: method declarations for BassWidthDialog
   void PopulateOrExchange(ShuttleGui & S);
   bool TransferDataToWindow();
   bool TransferDataFromWindow();

 private:
   // WDR: handler declarations for BassWidthDialog
   void OnWidthText(wxCommandEvent & event);
   void OnThreshText(wxCommandEvent & event);
   void OnWidthSlider(wxCommandEvent & event);
   void OnThreshSlider(wxCommandEvent & event);
   void OnPreview(wxCommandEvent & event);

 private:
   wxSlider *mWidthS;
   wxSlider *mThreshS;
   wxTextCtrl *mWidthT;
   wxTextCtrl *mThreshT;

   DECLARE_EVENT_TABLE()

 public:
   EffectClickRemoval *mEffect;

   int mThresh;
   int mWidth;

};

#endif
