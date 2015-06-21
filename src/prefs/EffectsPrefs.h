/**********************************************************************

  Audacity: A Digital Audio Editor

  EffectsPrefs.h

  Brian Gunlogson
  Joshua Haberman
  James Crook

**********************************************************************/

#ifndef __AUDACITY_EFFECTS_PREFS__
#define __AUDACITY_EFFECTS_PREFS__

#include <wx/defs.h>

#include <wx/arrstr.h>
#include <wx/window.h>

#include "PrefsPanel.h"

class ShuttleGui;

class EffectsPrefs :public PrefsPanel
{
 public:
   EffectsPrefs(wxWindow * parent);
   ~EffectsPrefs();
   virtual bool Apply();

 private:
   void Populate();
   void PopulateOrExchange(ShuttleGui & S);
};

class EffectsPrefsFactory : public PrefsPanelFactory
{
public:
   virtual PrefsPanel *Create(wxWindow *parent);
};
#endif
