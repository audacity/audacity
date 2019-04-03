/**********************************************************************

  Audacity: A Digital Audio Editor

  WarningsPrefs.cpp

  Brian Gunlogson
  Joshua Haberman
  Dominic Mazzoni
  James Crook


*******************************************************************//**

\class WarningsPrefs
\brief A PrefsPanel to enable/disable certain warning messages.

*//*******************************************************************/

#include "../Audacity.h"
#include "WarningsPrefs.h"

#include <wx/defs.h>

#include "../ShuttleGui.h"

////////////////////////////////////////////////////////////////////////////////

WarningsPrefs::WarningsPrefs(wxWindow * parent, wxWindowID winid)
:  PrefsPanel(parent, winid, _("Warnings"))
{
   Populate();
}

WarningsPrefs::~WarningsPrefs()
{
}

ComponentInterfaceSymbol WarningsPrefs::GetSymbol()
{
   return WARNINGS_PREFS_PLUGIN_SYMBOL;
}

wxString WarningsPrefs::GetDescription()
{
   return _("Preferences for Warnings");
}

wxString WarningsPrefs::HelpPageName()
{
   return "Warnings_Preferences";
}

void WarningsPrefs::Populate()
{
   //------------------------- Main section --------------------
   // Now construct the GUI itself.
   // Use 'eIsCreatingFromPrefs' so that the GUI is
   // initialised with values from gPrefs.
   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);
   // ----------------------- End of main section --------------
}

void WarningsPrefs::PopulateOrExchange(ShuttleGui & S)
{
   S.SetBorder(2);
   S.StartScroller();

   S.StartStatic(_("Show Warnings/Prompts for"));
   {
      S.TieCheckBox(_("Saving &projects"),
                    {wxT("/Warnings/FirstProjectSave"),
                     true});
      S.TieCheckBox(_("Saving &empty project"),
                    {wxT("/GUI/EmptyCanBeDirty"),
                     true});
      S.TieCheckBox(_("&Low disk space at launch or new project"),
                    {wxT("/Warnings/DiskSpaceWarning"),
                     true});
      S.TieCheckBox(_("Mixing down to &mono during export"),
                    {wxT("/Warnings/MixMono"),
                     true});
      S.TieCheckBox(_("Mixing down to &stereo during export"),
                    {wxT("/Warnings/MixStereo"),
                     true});
      S.TieCheckBox(_("Mixing down on export (&Custom FFmpeg or external program)"),
                    {wxT("/Warnings/MixUnknownChannels"),
                     true});
#ifdef EXPERIMENTAL_OD_DATA
      S.TieCheckBox(_("&Importing uncompressed audio files"),
                    {wxT("/Warnings/CopyOrEditUncompressedDataAsk"),
                     true});
#endif
   }
   S.EndStatic();
   S.EndScroller();

}

bool WarningsPrefs::Commit()
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   return true;
}

PrefsPanel::Factory
WarningsPrefsFactory = [](wxWindow *parent, wxWindowID winid)
{
   wxASSERT(parent); // to justify safenew
   return safenew WarningsPrefs(parent, winid);
};
