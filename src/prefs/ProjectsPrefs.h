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

#include <wx/window.h>

#include "PrefsPanel.h"

class ShuttleGui;

class ProjectsPrefs final : public PrefsPanel
{
 public:
   ProjectsPrefs(wxWindow * parent);
   ~ProjectsPrefs();
   virtual bool Apply();

 private:
   void Populate();
   void PopulateOrExchange(ShuttleGui & S);
};

class ProjectsPrefsFactory final : public PrefsPanelFactory
{
public:
   virtual PrefsPanel *Create(wxWindow *parent);
};
#endif
