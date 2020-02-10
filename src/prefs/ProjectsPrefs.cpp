/**********************************************************************

  Audacity: A Digital Audio Editor

  ProjectsPrefs.cpp

  Joshua Haberman
  Dominic Mazzoni
  James Crook

*******************************************************************//**

\class ProjectsPrefs
\brief A PrefsPanel used to select options related to Audacity Project
handling.

*//*******************************************************************/

#include "../Audacity.h"
#include "ProjectsPrefs.h"

#include "../Experimental.h"

#include <wx/defs.h>
#include <wx/textctrl.h>

#include "../FileFormats.h"
#include "../Prefs.h"
#include "../ShuttleGui.h"

////////////////////////////////////////////////////////////////////////////////

ProjectsPrefs::ProjectsPrefs(wxWindow * parent, wxWindowID winid)
:   PrefsPanel(parent, winid,
   /* i18n-hint: (noun) i.e Audacity projects. */
               XO("Projects"))
{
   Populate();
}

ProjectsPrefs::~ProjectsPrefs()
{
}

ComponentInterfaceSymbol ProjectsPrefs::GetSymbol()
{
   return PROJECTS_PREFS_PLUGIN_SYMBOL;
}

TranslatableString ProjectsPrefs::GetDescription()
{
   return XO("Preferences for Projects");
}

wxString ProjectsPrefs::HelpPageName()
{
   return "Projects_Preferences";
}

/// Creates the dialog and its contents.
void ProjectsPrefs::Populate()
{
   //------------------------- Main section --------------------
   // Now construct the GUI itself.
   // Use 'eIsCreatingFromPrefs' so that the GUI is
   // initialised with values from gPrefs.
   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);
   // ----------------------- End of main section --------------
}

void ProjectsPrefs::PopulateOrExchange(ShuttleGui & S)
{
   S.SetBorder(2);
   S.StartScroller();

   S.StartStatic(XO("When saving a project that depends on other audio files"));
   {
      S.StartRadioButtonGroup(FileFormatsSaveWithDependenciesSetting);
      {
         S.TieRadioButton();
         S.TieRadioButton();
         S.TieRadioButton();
      }
      S.EndRadioButtonGroup();
   }
   S.EndStatic();
   S.EndScroller();

}

bool ProjectsPrefs::Commit()
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   return true;
}

#ifdef EXPERIMENTAL_OD_DATA
namespace{
PrefsPanel::Registration sAttachment{ "Projects",
   [](wxWindow *parent, wxWindowID winid, AudacityProject *)
   {
      wxASSERT(parent); // to justify safenew
      return safenew ProjectsPrefs(parent, winid);
   },
   false,
   // Register with an explicit ordering hint because this one is
   // only conditionally compiled
   { "", { Registry::OrderingHint::After, "ImportExport" } }
};
}
#endif
