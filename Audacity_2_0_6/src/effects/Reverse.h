/**********************************************************************

  Audacity: A Digital Audio Editor

  Reverse.h

  Mark Phillips

  This class reverses the selected audio.

**********************************************************************/

#ifndef __AUDACITY_EFFECT_REVERSE__
#define __AUDACITY_EFFECT_REVERSE__

#include <wx/intl.h>

#include "Effect.h"

#define __UNINITIALIZED__ (-1)

class WaveTrack;

class EffectReverse:public Effect {

 public:
   EffectReverse();

   virtual wxString GetEffectName() {
      return wxString(_("Reverse"));
   }

   virtual std::set<wxString> GetEffectCategories() {
      std::set<wxString> result;
      result.insert(wxT("http://audacityteam.org/namespace#TimelineChanger"));
      return result;
   }

   virtual wxString GetEffectIdentifier() {
      return wxString(wxT("Reverse"));
   }

   virtual wxString GetEffectAction() {
      return wxString(_("Reversing"));
   }

   virtual bool Process();

 private:
   bool ProcessOneClip(int count, WaveTrack* track,
                   sampleCount start, sampleCount len, sampleCount originalStart, sampleCount originalEnd);
   bool ProcessOneWave(int count, WaveTrack* track, sampleCount start, sampleCount len);
 };

#endif

