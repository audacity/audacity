/**********************************************************************

  Audacity: A Digital Audio Editor

  Invert.h

  Mark Phillips
  
  This class inverts the selected audio.

**********************************************************************/

#ifndef __AUDACITY_EFFECT_INVERT__
#define __AUDACITY_EFFECT_INVERT__

#include <wx/intl.h>
#include <wx/string.h>

#include "SimpleMono.h"

class WaveTrack;

class EffectInvert:public EffectSimpleMono {

 public:
   virtual wxString GetEffectName() {
      return wxString(_("Invert"));
   }
   
   virtual std::set<wxString> GetEffectCategories() {
      std::set<wxString> result;
      result.insert(wxT("http://lv2plug.in/ns/lv2core#UtilityPlugin"));
      return result;
   }

   virtual wxString GetEffectIdentifier() {
      return wxString(wxT("Invert"));
   }

   virtual wxString GetEffectAction() {
      return wxString(_("Inverting"));
   }
   
 protected:
   virtual bool ProcessSimpleMono(float *buffer, sampleCount len);
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
// arch-tag: 12a3ac5a-4cb3-40f2-811a-595716cabd49

