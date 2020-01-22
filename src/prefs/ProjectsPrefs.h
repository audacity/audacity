/**********************************************************************

  Audacity: A Digital Audio Editor

  ProjectsPrefs.h

  Joshua Haberman
  Dominic Mazzoni
  James Crook

**********************************************************************/

#ifndef __AUDACITY_PROJECT_PREFS__
#define __AUDACITY_PROJECT_PREFS__

#include <wx/defs.h>

#include "PrefsPanel.h"

class ShuttleGui;

#define PROJECTS_PREFS_PLUGIN_SYMBOL ComponentInterfaceSymbol{ XO("Projects") }

class ProjectsPrefs final : public PrefsPanel
{
 public:
   ProjectsPrefs(wxWindow * parent, wxWindowID winid);
   ~ProjectsPrefs();
   ComponentInterfaceSymbol GetSymbol() override;
   TranslatableString GetDescription() override;

   bool Commit() override;
   wxString HelpPageName() override;
   void PopulateOrExchange(ShuttleGui & S) override;

 private:
   void Populate();
};

#endif
