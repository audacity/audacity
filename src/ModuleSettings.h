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

#include "PrefsPanel.h"


class wxArrayString;
class ShuttleGui;

enum {
   kModuleDisabled = 0,
   kModuleEnabled = 1,
   kModuleAsk = 2,     // Will ask, each time, when audacity starts.
   kModuleFailed = 3,  // Audacity thinks this is a bad module.
   kModuleNew = 4      // Audacity will ask once, and remember the answer.
};


#define MODULE_PREFS_PLUGIN_SYMBOL ComponentInterfaceSymbol{ XO("Module") }

class ModulePrefs final : public PrefsPanel
{
 public:
   ModulePrefs(wxWindow * parent, wxWindowID winid);
   ~ModulePrefs();
   ComponentInterfaceSymbol GetSymbol() override;
   TranslatableString GetDescription() override;

   bool Commit() override;
   wxString HelpPageName() override;
   void PopulateOrExchange(ShuttleGui & S) override;

   static int GetModuleStatus( const FilePath &fname );
   static void SetModuleStatus( const FilePath &fname, int iStatus );

 private:
   void GetAllModuleStatuses();
   void Populate();
   wxArrayString mModules;
   std::vector<int> mStatuses;
   FilePaths mPaths;
};

#endif
