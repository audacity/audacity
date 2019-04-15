/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportExportPrefs.h

  Joshua Haberman
  Dominic Mazzoni
  James Crook

**********************************************************************/

#ifndef __AUDACITY_IMPORT_EXPORT_PREFS__
#define __AUDACITY_IMPORT_EXPORT_PREFS__

#include <wx/defs.h>

#include "PrefsPanel.h"

class ShuttleGui;

#define IMPORT_EXPORT_PREFS_PLUGIN_SYMBOL ComponentInterfaceSymbol{ XO("IMPORT EXPORT") }

class ImportExportPrefs final : public PrefsPanel
{
 public:
   ImportExportPrefs(wxWindow * parent, wxWindowID winid);
   ~ImportExportPrefs();
   virtual ComponentInterfaceSymbol GetSymbol();
   virtual wxString GetDescription();

   bool Commit() override;
   wxString HelpPageName() override;
   void PopulateOrExchange(ShuttleGui & S) override;

 private:
   void Populate();
};

/// A PrefsPanelFactory that creates one ImportExportPrefs panel.
class ImportExportPrefsFactory final : public PrefsPanelFactory
{
public:
   PrefsPanel *operator () (wxWindow *parent, wxWindowID winid) override;
};
#endif
