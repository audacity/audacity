/**********************************************************************

  Audacity: A Digital Audio Editor

  ModulePrefs.cpp

  James Crook

*******************************************************************//**

\class ModulePrefs
\brief A PrefsPanel to enable/disable certain modules.  'Modules' are
dynamically linked libraries that modify Audacity.  They are plug-ins
with names like mod-script-pipe that add NEW features.

*//*******************************************************************/

#include "ModulePrefs.h"

#include <wx/defs.h>
#include <wx/filename.h>

#include "ShuttleGui.h"

#include "Prefs.h"
#include "ModuleSettings.h"

////////////////////////////////////////////////////////////////////////////////

ModulePrefs::ModulePrefs(wxWindow* parent, wxWindowID winid)
/* i18n-hint: Modules are optional extensions to Audacity that add NEW features.*/
    :  PrefsPanel(parent, winid, XO("Modules"))
{
    Populate();
}

ModulePrefs::~ModulePrefs()
{
}

ComponentInterfaceSymbol ModulePrefs::GetSymbol() const
{
    return MODULE_PREFS_PLUGIN_SYMBOL;
}

TranslatableString ModulePrefs::GetDescription() const
{
    return XO("Preferences for Module");
}

ManualPageID ModulePrefs::HelpPageName()
{
    return "Modules_Preferences";
}

void ModulePrefs::GetAllModuleStatuses()
{
    // Modules could for example be:
    //    mod-script-pipe
    //    mod-nyq-bench
    //    mod-menu-munger
    //    mod-theming

    // TODO: On an Audacity upgrade we should (?) actually untick modules.
    // The old modules might be still around, and we do not want to use them.
    mModules.clear();
    mStatuses.clear();
    mPaths.clear();

    // Iterate through all Modules listed in prefs.
    // Get their names and values.
    auto moduleGroup = gPrefs->BeginGroup("Module");
    for (const auto& key : gPrefs->GetChildKeys()) {
        int iStatus;
        gPrefs->Read(key, &iStatus, static_cast<decltype(iStatus)>(kModuleDisabled));
        wxString fname;
        gPrefs->Read(wxT("/ModulePath/") + key, &fname, {});
        if (!fname.empty() && wxFileExists(fname)) {
            if (iStatus > kModuleNew) {
                iStatus = kModuleNew;
                gPrefs->Write(key, iStatus);
            }
            //wxLogDebug( wxT("Entry: %s Value: %i"), str, iStatus );
            mModules.push_back(key);
            mStatuses.push_back(iStatus);
            mPaths.push_back(fname);
        }
    }
}

void ModulePrefs::Populate()
{
    GetAllModuleStatuses();
    //------------------------- Main section --------------------
    // Now construct the GUI itself.
    // Use 'eIsCreatingFromPrefs' so that the GUI is
    // initialised with values from gPrefs.
    ShuttleGui S(this, eIsCreatingFromPrefs);
    PopulateOrExchange(S);
    // ----------------------- End of main section --------------
}

void ModulePrefs::PopulateOrExchange(ShuttleGui& S)
{
    S.SetBorder(2);
    S.StartScroller();

    S.StartStatic({});
    {
        S.AddFixedText(XO(
                           "Modules are optional components of Audacity that enable some functionality, such as importing and exporting. \nIt is generally not necessary to change these settings."));
        S.AddFixedText(XO(
                           "Changes to these settings only take effect when restarting Audacity.\n"));
        {
            S.StartMultiColumn(2);
            int i;
            for (i=0; i < (int)mModules.size(); i++) {
                S.TieChoice(Verbatim(mModules[i]),
                            mStatuses[i],
                {
                    XO("Disabled"),
                    XO("Enabled"),
                    XO("Always ask"),
                    XO("Failed"),
                    XO("No choice made"),
                }
                            );
            }
            S.EndMultiColumn();
        }
        if (mModules.size() < 1) {
            S.AddFixedText(XO("Error: No modules were found. This may indicate a faulty installation."));
        }
    }
    S.EndStatic();
    S.EndScroller();
}

bool ModulePrefs::Commit()
{
    ShuttleGui S(this, eIsSavingToPrefs);
    PopulateOrExchange(S);
    int i;
    for (i=0; i < (int)mPaths.size(); i++) {
        ModuleSettings::SetModuleStatus(mPaths[i], mStatuses[i]);
    }
    return true;
}

namespace {
PrefsPanel::Registration sAttachment{ "Module",
                                      [](wxWindow* parent, wxWindowID winid, AudacityProject*)
    {
        wxASSERT(parent); // to justify safenew
        return safenew ModulePrefs(parent, winid);
    },
                                      false,
                                      // Register with an explicit ordering hint because this one is
                                      // only conditionally compiled
                                      { "", { Registry::OrderingHint::After, "Mouse" } }
};
}
