/**********************************************************************

  Audacity: A Digital Audio Editor

  PlaybackPrefs.h

  Joshua Haberman
  James Crook

**********************************************************************/

#ifndef __AUDACITY_PLAYBACK_PREFS__
#define __AUDACITY_PLAYBACK_PREFS__

#include <wx/defs.h>

#include <wx/window.h>

#include "../ShuttleGui.h"

#include "PrefsPanel.h"

class PlaybackPrefs:public PrefsPanel
{
 public:
   PlaybackPrefs(wxWindow * parent);
   virtual ~PlaybackPrefs();
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
