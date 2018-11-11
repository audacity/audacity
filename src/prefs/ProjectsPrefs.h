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

class ProjectsPrefs final : public PrefsPanel
{
 public:
   ProjectsPrefs(wxWindow * parent, wxWindowID winid);
   ~ProjectsPrefs();
   bool Commit() override;
   wxString HelpPageName() override;
   void PopulateOrExchange(ShuttleGui & S) override;

 private:
   void Populate();
};

/// A PrefsPanelFactory that creates one ProjectPrefs panel.
class ProjectsPrefsFactory final : public PrefsPanelFactory
{
public:
   PrefsPanel *operator () (wxWindow *parent, wxWindowID winid) override;
};
#endif
