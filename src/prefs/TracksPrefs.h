/**********************************************************************

  Audacity: A Digital Audio Editor

  TracksPrefs.h

  Brian Gunlogson
  Joshua Haberman
  James Crook

**********************************************************************/

#ifndef __AUDACITY_TRACKS_PREFS__
#define __AUDACITY_TRACKS_PREFS__

//#include <wx/defs.h>

//#include <wx/arrstr.h>
//#include <wx/window.h>

#include "PrefsPanel.h"

class ShuttleGui;

class TracksPrefs final : public PrefsPanel
{
 public:
   TracksPrefs(wxWindow * parent);
   ~TracksPrefs();
   bool Commit() override;
   wxString HelpPageName() override;

   static bool GetPinnedHeadPreference();
   static void SetPinnedHeadPreference(bool value, bool flush = false);

   static wxString GetDefaultAudioTrackNamePreference();

 private:
   void Populate();
   void PopulateOrExchange(ShuttleGui & S);

   static int iPreferencePinned;

   wxArrayInt    mViewCodes;
   wxArrayString mViewChoices;
   wxArrayInt    mSampleDisplayCodes;
   wxArrayString mSampleDisplayChoice;
};

class TracksPrefsFactory final : public PrefsPanelFactory
{
public:
   PrefsPanel *Create(wxWindow *parent) override;
};
#endif
