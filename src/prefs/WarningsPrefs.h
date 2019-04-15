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

#include "PrefsPanel.h"

class ShuttleGui;

#define WARNINGS_PREFS_PLUGIN_SYMBOL ComponentInterfaceSymbol{ XO("Warnings") }

class WarningsPrefs final : public PrefsPanel
{
 public:
   WarningsPrefs(wxWindow * parent, wxWindowID winid);
   ~WarningsPrefs();
   virtual ComponentInterfaceSymbol GetSymbol();
   virtual wxString GetDescription();

   bool Commit() override;
   wxString HelpPageName() override;

 private:
   void Populate();
   void PopulateOrExchange(ShuttleGui & S) override;
};

/// A PrefsPanelFactory that creates one WarningPrefs panel.
class WarningsPrefsFactory final : public PrefsPanelFactory
{
public:
   PrefsPanel *operator () (wxWindow *parent, wxWindowID winid) override;
};
#endif
