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

#include <wx/defs.h>
#include <wx/textctrl.h>

#include "../Prefs.h"
#include "../ShuttleGui.h"

#include "ProjectsPrefs.h"

////////////////////////////////////////////////////////////////////////////////

ProjectsPrefs::ProjectsPrefs(wxWindow * parent)
:   PrefsPanel(parent, _("Projects"))
{
   Populate();
}

ProjectsPrefs::~ProjectsPrefs()
{
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
   wxTextCtrl *w;

   S.SetBorder(2);

   S.StartStatic(_("When saving a project that depends on other audio files"));
   {
      S.StartRadioButtonGroup(wxT("/FileFormats/SaveProjectWithDependencies"), wxT("ask"));
      {
         S.TieRadioButton(_("&Always copy all audio into project (safest)"),
                          wxT("copy"));
         S.TieRadioButton(_("Do &not copy any audio"),
                          wxT("never"));
         S.TieRadioButton(_("As&k user"),
                          wxT("ask"));
      }
      S.EndRadioButtonGroup();
   }
   S.EndStatic();

   S.StartStatic(_("Auto save"));
   {
      S.TieCheckBox(_("Auto &save a copy of the project in a separate folder"),
                    wxT("/Directories/AutoSaveEnabled"),
                    true);

      S.StartThreeColumn();
      {
         w = S.TieTextBox(_("Auto save in&terval:"),
                          wxT("/Directories/AutoSaveMinutes"),
                          2.0,
                          9);
         S.AddUnits(_("minutes"));
         w->SetName(w->GetName() + wxT(" ") + _("minutes"));
      }
      S.EndThreeColumn();
   }
   S.EndStatic();
}

bool ProjectsPrefs::Apply()
{  
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);    
   
   return true;
}


// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 427b9e64-3fc6-40ef-bbf8-e6fff1d442f0
