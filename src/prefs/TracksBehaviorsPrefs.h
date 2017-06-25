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

class ShuttleGui;

class TracksBehaviorsPrefs final : public PrefsPanel
{
 public:
   TracksBehaviorsPrefs(wxWindow * parent);
   ~TracksBehaviorsPrefs();
   bool Commit() override;
   wxString HelpPageName() override;

   static const wxChar *ScrollingPreferenceKey();
   static inline bool ScrollingPreferenceDefault() { return false; }

 private:
   void Populate();
   void PopulateOrExchange(ShuttleGui & S);

   wxArrayString mSoloCodes;
   wxArrayString mSoloChoices;
};

class TracksBehaviorsPrefsFactory final : public PrefsPanelFactory
{
public:
   explicit TracksBehaviorsPrefsFactory();
   PrefsPanel *Create(wxWindow *parent) override;

};
#endif
