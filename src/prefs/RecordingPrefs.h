/**********************************************************************

  Audacity: A Digital Audio Editor

  RecordingPrefs.h

  Joshua Haberman
  James Crook

**********************************************************************/

#ifndef __AUDACITY_RECORDING_PREFS__
#define __AUDACITY_RECORDING_PREFS__

#include <wx/defs.h>

#include <wx/window.h>

#include "../ShuttleGui.h"

#include "PrefsPanel.h"

class RecordingPrefs:public PrefsPanel
{
 public:
   RecordingPrefs(wxWindow * parent);
   virtual ~RecordingPrefs();
   virtual bool Apply();

 private:
   void Populate();
   void PopulateOrExchange(ShuttleGui & S);
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
// arch-tag: df22b108-e989-4ec4-a8b6-dddbcc7be6a7
