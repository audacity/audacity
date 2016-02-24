/**********************************************************************

  Audacity: A Digital Audio Editor

  ModulePrefs.h

  Brian Gunlogson
  Joshua Haberman
  James Crook

**********************************************************************/

#ifndef __AUDACITY_MODULE_PREFS__
#define __AUDACITY_MODULE_PREFS__

#include <wx/defs.h>

#include <wx/window.h>

#include "PrefsPanel.h"


class ShuttleGui;

enum {
   kModuleDisabled = 0,
   kModuleEnabled = 1,
   kModuleAsk = 2,     // Will ask, each time, when audacity starts.
   kModuleFailed = 3,  // Audacity thinks this is a bad module.
   kModuleNew = 4      // Audacity will ask once, and remember the answer.
};


class ModulePrefs final : public PrefsPanel
{
 public:
   ModulePrefs(wxWindow * parent);
   ~ModulePrefs();
   bool Apply() override;

   static int GetModuleStatus( const wxString &fname );
   static void SetModuleStatus( const wxString &fname, int iStatus );

 private:
   void GetAllModuleStatuses();
   void Populate();
   void PopulateOrExchange(ShuttleGui & S);
   wxArrayString mModules;
   wxArrayInt    mStatuses;
   wxArrayString mPaths;
};

class ModulePrefsFactory final : public PrefsPanelFactory
{
public:
   PrefsPanel *Create(wxWindow *parent) override;
};
#endif
