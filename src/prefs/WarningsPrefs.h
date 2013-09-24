/**********************************************************************

  Audacity: A Digital Audio Editor

  WarningsPrefs.h

  Brian Gunlogson
  Joshua Haberman
  James Crook

**********************************************************************/

#ifndef __AUDACITY_WARNINGS_PREFS__
#define __AUDACITY_WARNINGS_PREFS__

#include <wx/defs.h>

#include <wx/window.h>

#include "../ShuttleGui.h"

#include "PrefsPanel.h"

class WarningsPrefs:public PrefsPanel
{
 public:
   WarningsPrefs(wxWindow * parent);
   ~WarningsPrefs();
   virtual bool Apply();

 private:
   void Populate();
   void PopulateOrExchange(ShuttleGui & S);
};

#endif
