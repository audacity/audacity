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
      y = z = buf0 = buf1 = buf2 = buf3 = buf4 = buf5 = buf6 = 0;
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
   float y, z, buf0, buf1, buf2, buf3, buf4, buf5, buf6;

 protected:
   virtual bool MakeNoise(float *buffer, sampleCount len, float fs, float amplitude);
   //double mCurRate;

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
