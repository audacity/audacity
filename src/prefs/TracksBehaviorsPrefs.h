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
class wxArrayStringEx;

class TracksBehaviorsPrefs final : public PrefsPanel
{
 public:
   TracksBehaviorsPrefs(wxWindow * parent, wxWindowID winid);
   ~TracksBehaviorsPrefs();
   bool Commit() override;
   wxString HelpPageName() override;

   static const wxChar *ScrollingPreferenceKey();
   static inline bool ScrollingPreferenceDefault() { return false; }

 private:
   void Populate();
   void PopulateOrExchange(ShuttleGui & S) override;

   wxArrayStringEx mSoloCodes;
   wxArrayStringEx mSoloChoices;
};

/// A PrefsPanelFactory that creates one TracksBehaviorsPrefs panel.
class TracksBehaviorsPrefsFactory final : public PrefsPanelFactory
{
public:
   explicit TracksBehaviorsPrefsFactory();
   PrefsPanel *operator () (wxWindow *parent, wxWindowID winid) override;

};
#endif
