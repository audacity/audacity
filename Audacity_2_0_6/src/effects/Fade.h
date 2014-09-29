/**********************************************************************

  Audacity: A Digital Audio Editor

  Fade.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EFFECT_FADE__
#define __AUDACITY_EFFECT_FADE__

#include "SimpleMono.h"

#include <wx/intl.h>

class wxString;

class EffectFadeIn: public EffectSimpleMono {

 public:
   virtual wxString GetEffectName() {
      return wxString(_("Fade In"));
   }

   virtual std::set<wxString> GetEffectCategories() {
      std::set<wxString> result;
      result.insert(wxT("http://lv2plug.in/ns/lv2core#UtilityPlugin"));
      return result;
   }

   virtual wxString GetEffectIdentifier() {
      return wxString(wxT("FadeIn"));
   }

   virtual wxString GetEffectAction() {
      return wxString(_("Fading In"));
   }

 protected:
   sampleCount mSample;
   sampleCount mLen;

   virtual bool NewTrackSimpleMono();

   virtual bool ProcessSimpleMono(float *buffer, sampleCount len);
};

class EffectFadeOut:public EffectSimpleMono {

 public:
   virtual wxString GetEffectName() {
      return wxString(_("Fade Out"));
   }

   virtual std::set<wxString> GetEffectCategories() {
      std::set<wxString> result;
      result.insert(wxT("http://lv2plug.in/ns/lv2core#UtilityPlugin"));
      return result;
   }

   virtual wxString GetEffectIdentifier() {
      return wxString(wxT("FadeOut"));
   }

   virtual wxString GetEffectAction() {
      return wxString(_("Fading Out"));
   }

 protected:
   sampleCount mSample;
   sampleCount mLen;

   virtual bool NewTrackSimpleMono();

   virtual bool ProcessSimpleMono(float *buffer, sampleCount len);
};

#endif
