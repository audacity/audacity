/**********************************************************************

  Audacity: A Digital Audio Editor

  DtmfGen.h

  Salvo Ventura
  Dec 2006

  An effect for the "Generator" menu to generate DTMF tones

**********************************************************************/

#ifndef __AUDACITY_EFFECT_DTMF__
#define __AUDACITY_EFFECT_DTMF__

#include <wx/choice.h>
#include <wx/defs.h>
#include <wx/intl.h>
#include <wx/sizer.h>
#include <wx/string.h>
#include <wx/textctrl.h>

#include "../ShuttleGui.h"
#include "../WaveTrack.h"
#include "../widgets/TimeTextCtrl.h"

#include "Generator.h"

#define __UNINITIALIZED__ (-1)

class EffectDtmf : public Generator {

 public:
   EffectDtmf() : mIsSelection(false) {
      SetEffectFlags(BUILTIN_EFFECT | INSERT_EFFECT);
   }

   virtual wxString GetEffectName() {
      return wxString(_("DTMF Tones..."));
   }

   virtual std::set<wxString> GetEffectCategories() {
     std::set<wxString> result;
     result.insert(wxT("http://lv2plug.in/ns/lv2core#GeneratorPlugin"));
     return result;
   }

   virtual wxString GetEffectIdentifier() {
      return wxString(wxT("DTMFTone"));
   }

   virtual wxString GetEffectDescription() {
      return wxString::Format(_("Applied effect: Generate DTMF tones, %.6lf seconds"), mDuration);
   }

   virtual wxString GetEffectAction() {
      return wxString(_("Generating DTMF tones"));
   }

   virtual bool Init();
   virtual bool PromptUser();
   virtual bool TransferParameters( Shuttle & shuttle );

 private:
   sampleCount numSamplesSequence, numSamplesTone, numSamplesSilence;

   wxString dtmfString;       // dtmf tone string
   int    dtmfNTones;         // total number of tones to generate
   double dtmfTone;           // duration of a single tone in ms
   double dtmfSilence;        // duration of silence between tones in ms
   double dtmfDutyCycle;      // ratio of dtmfTone/(dtmfTone+dtmfSilence)
   double dtmfAmplitude;      // amplitude of dtmf tone sequence, restricted to (0-1)
   bool mIsSelection;

 protected:
   virtual bool MakeDtmfTone(float *buffer, sampleCount len, float fs,
                             wxChar tone, sampleCount last,
                             sampleCount total, float amplitude);
   bool GenerateTrack(WaveTrack *tmp, const WaveTrack &track, int ntrack);
   void Success();

 // friendship ...
 friend class DtmfDialog;

};

//----------------------------------------------------------------------------
// DtmfDialog
//----------------------------------------------------------------------------

// Declare window functions

class DtmfDialog:public EffectDialog {
 public:
   // constructors and destructors
   DtmfDialog(EffectDtmf * effect, wxWindow * parent, const wxString & title);

   // method declarations
   void PopulateOrExchange(ShuttleGui & S);
   bool TransferDataToWindow();
   bool TransferDataFromWindow();

 private:
   void OnDtmfStringText(wxCommandEvent & event);
   void OnDtmfDurationText(wxCommandEvent & event);
   void OnDutyCycleSlider(wxCommandEvent & event);
   void OnTimeCtrlUpdate(wxCommandEvent & event);
   void Recalculate(void);

 private:
   EffectDtmf *mEffect;
   wxSlider   *mDtmfDutyS;
   wxTextCtrl *mDtmfStringT;
   TimeTextCtrl *mDtmfDurationT;
   wxStaticText *mDtmfToneT;
   wxStaticText *mDtmfSilenceT;
   wxStaticText *mDtmfDutyT;

   DECLARE_EVENT_TABLE()

 public:
   wxString dString;       // dtmf tone string
   int    dNTones;         // total number of tones to generate
   double dTone;           // duration of a single tone
   double dSilence;        // duration of silence between tones
   double dDuration;       // duration of the whole dtmf tone sequence
   double dDutyCycle;      // ratio of dTone/(dTone+dSilence)
   double dAmplitude;      // amplitude of dtmf tone sequence, restricted to (0-1)
   bool   dIsSelection;    // true if duration comes from selection

};

#endif
