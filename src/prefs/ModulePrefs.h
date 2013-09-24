/**********************************************************************

  Audacity: A Digital Audio Editor

  ModulePrefs.h

  Brian Gunlogson
  Joshua Haberman
  James Crook

**********************************************************************/

#ifndef __AUDACITY_MODULE_PREFS__
#define __AUDACITY_MODULE_PREFS__

#include <wx/defs.h>

#include <wx/window.h>

#include "../ShuttleGui.h"

#include "PrefsPanel.h"

class ModulePrefs:public PrefsPanel
{
 public:
   ModulePrefs(wxWindow * parent);
   ~ModulePrefs();
   virtual bool Apply();

 private:
   void Populate();
   void PopulateOrExchange(ShuttleGui & S);
};

#endif
