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


#include "WarningsPrefs.h"

#include <wx/defs.h>

#include "Prefs.h"
#include "../ShuttleGui.h"

////////////////////////////////////////////////////////////////////////////////

WarningsPrefs::WarningsPrefs(wxWindow * parent, wxWindowID winid)
:  PrefsPanel(parent, winid, XO("Warnings"))
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

TranslatableString WarningsPrefs::GetDescription()
{
   return XO("Preferences for Warnings");
}

ManualPageID WarningsPrefs::HelpPageName()
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

   S.StartStatic(XO("Show Warnings/Prompts for"));
   {
      S.TieCheckBox(XXO("Saving &projects"),
                    {wxT("/Warnings/FirstProjectSave"),
                     true});
      S.TieCheckBox(XXO("Saving &empty project"),
                    {wxT("/GUI/EmptyCanBeDirty"),
                     true});
      S.TieCheckBox(XXO("Mixing down to &mono during export"),
                    {wxT("/Warnings/MixMono"),
                     true});
      S.TieCheckBox(XXO("Mixing down to &stereo during export"),
                    {wxT("/Warnings/MixStereo"),
                     true});
      S.TieCheckBox(XXO("Mixing down on export (&Custom FFmpeg or external program)"),
                    {wxT("/Warnings/MixUnknownChannels"),
                     true});
      S.TieCheckBox(XXO("Missing file &name extension during export"),
                    {wxT("/Warnings/MissingExtension"),
                     true});
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

namespace{
PrefsPanel::Registration sAttachment{ "Warnings",
   [](wxWindow *parent, wxWindowID winid, AudacityProject *)
   {
      wxASSERT(parent); // to justify safenew
      return safenew WarningsPrefs(parent, winid);
   }
};
}
