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

#include "PrefsPanel.h"

class ShuttleGui;

class EffectsPrefs final : public PrefsPanel
{
 public:
   EffectsPrefs(wxWindow * parent, wxWindowID winid);
   ~EffectsPrefs();
   bool Commit() override;
   wxString HelpPageName() override;
   void PopulateOrExchange(ShuttleGui & S) override;

 private:
   void Populate();
};

/// A PrefsPanelFactory that creates one EffectsPrefs panel.
class EffectsPrefsFactory final : public PrefsPanelFactory
{
public:
   PrefsPanel *operator () (wxWindow *parent, wxWindowID winid) override;
};
#endif
