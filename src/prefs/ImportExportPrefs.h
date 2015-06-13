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

#include <wx/window.h>

#include "PrefsPanel.h"

class ShuttleGui;

class ImportExportPrefs :public PrefsPanel
{
 public:
   ImportExportPrefs(wxWindow * parent);
   ~ImportExportPrefs();
   virtual bool Apply();

 private:
   void Populate();
   void PopulateOrExchange(ShuttleGui & S);
};

class ImportExportPrefsFactory : public PrefsPanelFactory
{
public:
   virtual PrefsPanel *Create(wxWindow *parent);
};
#endif
