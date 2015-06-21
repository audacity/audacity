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

#include "PrefsPanel.h"

class ShuttleGui;

class PlaybackPrefs :public PrefsPanel
{
 public:
   PlaybackPrefs(wxWindow * parent);
   virtual ~PlaybackPrefs();
   virtual bool Apply();

 private:
   void Populate();
   void PopulateOrExchange(ShuttleGui & S);
};

class PlaybackPrefsFactory : public PrefsPanelFactory
{
public:
   virtual PrefsPanel *Create(wxWindow *parent);
};

#endif
