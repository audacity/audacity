/**********************************************************************

  Audacity: A Digital Audio Editor

  Noise.h

  Dominic Mazzoni

  An effect for the "Generator" menu to add white noise.

**********************************************************************/

#ifndef __AUDACITY_EFFECT_NOISE__
#define __AUDACITY_EFFECT_NOISE__

#include <wx/defs.h>
#include <wx/intl.h>

#include "Generator.h"
#include "../widgets/TimeTextCtrl.h"

class wxString;
class wxChoice;
class wxTextCtrl;
class ShuttleGui;

#define __UNINITIALIZED__ (-1)

class WaveTrack;


class EffectNoise : public BlockGenerator {

 public:
   EffectNoise() {
      SetEffectFlags(BUILTIN_EFFECT | INSERT_EFFECT);
      noiseType = 0;
      noiseAmplitude = 1.0;
   }
   virtual bool Init();

   virtual wxString GetEffectName() {
      return wxString(_("Noise..."));
   }

   virtual std::set<wxString> GetEffectCategories() {
      std::set<wxString> result;
      result.insert(wxT("http://lv2plug.in/ns/lv2core#GeneratorPlugin"));
      return result;
   }

   virtual wxString GetEffectIdentifier() {
      return wxString(wxT("Noise"));
   }

   virtual wxString GetEffectDescription() {
      return wxString::Format(_("Applied effect: Generate Noise, %.6lf seconds"), mDuration);
   }

   virtual wxString GetEffectAction() {
      return wxString(_("Generating Noise"));
   }

   virtual bool PromptUser();
   virtual bool TransferParameters( Shuttle & shuttle );

   void GenerateBlock(float *data, const WaveTrack &track, sampleCount block);
   void Success();

 private:
   sampleCount numSamples;
   int noiseType;
   double noiseAmplitude;

 protected:
   virtual bool MakeNoise(float *buffer, sampleCount len, float fs, float amplitude);

 // friendship ...
 friend class NoiseDialog;

};

//----------------------------------------------------------------------------
// NoiseDialog
//----------------------------------------------------------------------------

// Declare window functions

class NoiseDialog:public EffectDialog {
 public:
   // constructors and destructors
   NoiseDialog(EffectNoise * effect, wxWindow * parent, const wxString & title);

   // method declarations
   void PopulateOrExchange(ShuttleGui & S);
   bool TransferDataToWindow();
   bool TransferDataFromWindow();

 private:
   void OnTimeCtrlUpdate(wxCommandEvent & event);
   DECLARE_EVENT_TABLE()

 public:
   double nRate;
   double nTime;
   wxArrayString *nTypeList;
   double nDuration;
   int nType;
   double nAmplitude;
   bool nIsSelection;

 private:
   EffectNoise  *mEffect;
   TimeTextCtrl *mNoiseDurationT;
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
// arch-tag: 3d52765e-51bb-4f53-8ed8-4239f7b42d16

