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
