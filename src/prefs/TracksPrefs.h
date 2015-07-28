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

class TracksPrefs :public PrefsPanel
{
 public:
   TracksPrefs(wxWindow * parent);
   ~TracksPrefs();
   virtual bool Apply();

 private:
   void Populate();
   void PopulateOrExchange(ShuttleGui & S);

   wxArrayString mSoloCodes;
   wxArrayString mSoloChoices;
   wxArrayInt    mViewCodes;
   wxArrayString mViewChoices;
};

class TracksPrefsFactory : public PrefsPanelFactory
{
public:
   virtual PrefsPanel *Create(wxWindow *parent);
};
#endif
