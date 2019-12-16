/**********************************************************************

  Audacity: A Digital Audio Editor

  TracksBehaviorsPrefs.h

  Brian Gunlogson
  Joshua Haberman
  James Crook
  Steve Daulton

**********************************************************************/

#ifndef __AUDACITY_TRACKSBEHAVIORS_PREFS__
#define __AUDACITY_TRACKSBEHAVIORS_PREFS__

#include "PrefsPanel.h"

class ChoiceSetting;
class ShuttleGui;
class wxArrayStringEx;

#define TRACKS_BEHAVIORS_PREFS_PLUGIN_SYMBOL ComponentInterfaceSymbol{ XO("Tracks Behaviors") }

class TracksBehaviorsPrefs final : public PrefsPanel
{
 public:
   TracksBehaviorsPrefs(wxWindow * parent, wxWindowID winid);
   ~TracksBehaviorsPrefs();
   ComponentInterfaceSymbol GetSymbol() override;
   TranslatableString GetDescription() override;

   bool Commit() override;
   wxString HelpPageName() override;

   static const wxChar *ScrollingPreferenceKey();
   static inline bool ScrollingPreferenceDefault() { return false; }

 private:
   void Populate();
   void PopulateOrExchange(ShuttleGui & S) override;
};

/// A PrefsPanel::Factory that creates one TracksBehaviorsPrefs panel.
extern PrefsPanel::Factory TracksBehaviorsPrefsFactory;

extern ChoiceSetting TracksBehaviorsSolo;

#endif
