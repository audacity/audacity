/**********************************************************************

  Audacity: A Digital Audio Editor

  TracksPrefs.h

  Brian Gunlogson
  Joshua Haberman
  James Crook

**********************************************************************/

#ifndef __AUDACITY_TRACKS_PREFS__
#define __AUDACITY_TRACKS_PREFS__

#include <wx/defs.h>

#include <wx/arrstr.h>
#include <wx/window.h>

#include "PrefsPanel.h"

class ShuttleGui;

class TracksPrefs final : public PrefsPanel
{
 public:
   TracksPrefs(wxWindow * parent);
   ~TracksPrefs();
   bool Apply() override;

   static const wxChar *ScrollingPreferenceKey();
   static inline bool ScrollingPreferenceDefault() { return false; }

 private:
   void Populate();
   void PopulateOrExchange(ShuttleGui & S);

   wxArrayString mSoloCodes;
   wxArrayString mSoloChoices;
   wxArrayInt    mViewCodes;
   wxArrayString mViewChoices;
};

class TracksPrefsFactory final : public PrefsPanelFactory
{
public:
   PrefsPanel *Create(wxWindow *parent) override;
};
#endif
