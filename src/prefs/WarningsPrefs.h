/**********************************************************************

  Audacity: A Digital Audio Editor

  WarningsPrefs.h

  Brian Gunlogson
  Joshua Haberman
  James Crook

**********************************************************************/

#ifndef __AUDACITY_WARNINGS_PREFS__
#define __AUDACITY_WARNINGS_PREFS__

#include <wx/defs.h>

#include <wx/window.h>

#include "PrefsPanel.h"

class ShuttleGui;

class WarningsPrefs final : public PrefsPanel
{
 public:
   WarningsPrefs(wxWindow * parent);
   ~WarningsPrefs();
   bool Apply() override;

 private:
   void Populate();
   void PopulateOrExchange(ShuttleGui & S);
};

class WarningsPrefsFactory final : public PrefsPanelFactory
{
public:
   PrefsPanel *Create(wxWindow *parent) override;
};
#endif
