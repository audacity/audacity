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

#include "../ShuttleGui.h"

#include "PrefsPanel.h"

class ProjectsPrefs:public PrefsPanel
{
 public:
   ProjectsPrefs(wxWindow * parent);
   ~ProjectsPrefs();
   virtual bool Apply();

 private:
   void Populate();
   void PopulateOrExchange(ShuttleGui & S);
};

#endif
