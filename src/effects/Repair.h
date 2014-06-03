/**********************************************************************

  Audacity: A Digital Audio Editor

  Repair.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EFFECT_REPAIR__
#define __AUDACITY_EFFECT_REPAIR__

#include <wx/intl.h>
#include <wx/string.h>

#include "SimpleMono.h"

class WaveTrack;

class EffectRepair: public Effect {

public:

   EffectRepair();
   virtual ~EffectRepair();

   virtual wxString GetEffectName() {
      return wxString(_("Repair"));
   }

   virtual std::set<wxString> GetEffectCategories() {
      std::set<wxString> result;
      result.insert(wxT("http://audacityteam.org/namespace#NoiseRemoval"));
      return result;
   }

   virtual wxString GetEffectIdentifier() {
      return wxString(wxT("Repair"));
   }

   virtual wxString GetEffectAction() {
      return wxString(_("Repairing damaged audio"));
   }

   virtual bool PromptUser();
   virtual bool TransferParameters( Shuttle & shuttle );

   virtual bool Process();

private:
   bool ProcessOne(int count, WaveTrack * track,
                   sampleCount start,
                   sampleCount len,
                   sampleCount repairStart, sampleCount repairLen);
};

#endif // __AUDACITY_EFFECT_REPAIT__
