/**********************************************************************

  Audacity: A Digital Audio Editor

  ModulePrefs.cpp

  Brian Gunlogson
  Joshua Haberman
  Dominic Mazzoni
  James Crook


*******************************************************************//**

\class ModulePrefs
\brief A PrefsPanel to enable/disable certain modules.

*//*******************************************************************/

#include "../Audacity.h"

#include <wx/defs.h>

#include "../ShuttleGui.h"

#include "ModulePrefs.h"

////////////////////////////////////////////////////////////////////////////////

ModulePrefs::ModulePrefs(wxWindow * parent)
:  PrefsPanel(parent, _("Modules"))
{
   Populate();
}

ModulePrefs::~ModulePrefs()
{
}

void ModulePrefs::Populate()
{
   //------------------------- Main section --------------------
   // Now construct the GUI itself.
   // Use 'eIsCreatingFromPrefs' so that the GUI is
   // initialised with values from gPrefs.
   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);
   // ----------------------- End of main section --------------
}

void ModulePrefs::PopulateOrExchange(ShuttleGui & S)
{
   S.SetBorder(2);

   S.StartStatic(_("Enable these Modules (if present), next time Audacity is started"));
   {
      S.AddFixedText(_("These are experimental. Enable them only if you've read the manual\nand know what you are doing.") );
      S.TieCheckBox(_("mod-&script-pipe"),
                    wxT("/Module/mod-script-pipe"),
                    false);
      S.TieCheckBox(_("mod-&nyq-bench"),
                    wxT("/Module/mod-nyq-bench"),
                    false);
      S.TieCheckBox(_("mod-&track-panel"),
                    wxT("/Module/mod-track-panel"),
                    false);
   }
   S.EndStatic();
}

bool ModulePrefs::Apply()
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   return true;
}
