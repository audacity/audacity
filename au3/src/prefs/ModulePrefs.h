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

#define MODULE_PREFS_PLUGIN_SYMBOL ComponentInterfaceSymbol { XO("Module") }

class ModulePrefs final : public PrefsPanel
{
public:
    ModulePrefs(wxWindow* parent, wxWindowID winid);
    ~ModulePrefs();
    ComponentInterfaceSymbol GetSymbol() const override;
    TranslatableString GetDescription() const override;

    bool Commit() override;
    ManualPageID HelpPageName() override;
    void PopulateOrExchange(ShuttleGui& S) override;

private:
    void GetAllModuleStatuses();
    void Populate();
    wxArrayString mModules;
    std::vector<int> mStatuses;
    FilePaths mPaths;
};

#endif
