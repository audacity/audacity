/**********************************************************************

  Audacity: A Digital Audio Editor

  PlaybackPrefs.h

  Joshua Haberman
  James Crook

**********************************************************************/

#ifndef __AUDACITY_PLAYBACK_PREFS__
#define __AUDACITY_PLAYBACK_PREFS__

#include <wx/defs.h>

#include "PrefsPanel.h"

class ShuttleGui;

#define PLAYBACK_PREFS_PLUGIN_SYMBOL ComponentInterfaceSymbol{ XO("Playback") }

class PlaybackPrefs final : public PrefsPanel
{
 public:
   PlaybackPrefs(wxWindow * parent, wxWindowID winid);
   virtual ~PlaybackPrefs();
   ComponentInterfaceSymbol GetSymbol() override;
   TranslatableString GetDescription() override;

   bool Commit() override;
   wxString HelpPageName() override;
   void PopulateOrExchange(ShuttleGui & S) override;
   
   static bool GetUnpinnedScrubbingPreference();

 private:
   void Populate();
};

#endif
