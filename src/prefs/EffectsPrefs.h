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

class EffectsPrefs final : public PrefsPanel
{
 public:
   EffectsPrefs(wxWindow * parent);
   ~EffectsPrefs();
   bool Apply() override;

 private:
   void Populate();
   void PopulateOrExchange(ShuttleGui & S);
};

class EffectsPrefsFactory final : public PrefsPanelFactory
{
public:
   PrefsPanel *Create(wxWindow *parent) override;
};
#endif
