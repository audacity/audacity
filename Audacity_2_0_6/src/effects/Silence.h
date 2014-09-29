/**********************************************************************

  Audacity: A Digital Audio Editor

  Silence.h

  Dominic Mazzoni

  An effect for the "Generator" menu to add silence.

**********************************************************************/

#ifndef __AUDACITY_EFFECT_SILENCE__
#define __AUDACITY_EFFECT_SILENCE__

#include <wx/defs.h>
#include <wx/dialog.h>
#include <wx/intl.h>

#include "Generator.h"
#include "../widgets/TimeTextCtrl.h"

class wxSizer;
class wxTextCtrl;

class EffectSilence : public Generator {

 public:
   EffectSilence() {
      SetEffectFlags(BUILTIN_EFFECT | INSERT_EFFECT);
   }

   virtual wxString GetEffectName() {
      return wxString(_("Silence..."));
   }

   virtual std::set<wxString> GetEffectCategories() {
      std::set<wxString> result;
      result.insert(wxT("http://lv2plug.in/ns/lv2core#GeneratorPlugin"));
      return result;
   }

   virtual wxString GetEffectIdentifier() {
      return wxString(wxT("Silence"));
   }

   virtual wxString GetEffectAction() {
      return wxString(_("Generating Silence"));
   }

   // Useful only after PromptUser values have been set.
   virtual wxString GetEffectDescription() {
      return wxString::Format(_("Applied effect: Generate Silence, %.6lf seconds"), mDuration);
   }

   virtual bool PromptUser();
 protected:
   bool GenerateTrack(WaveTrack *tmp, const WaveTrack &track, int ntrack);
};

#endif
