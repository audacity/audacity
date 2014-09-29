/**********************************************************************

  Audacity: A Digital Audio Editor

  GUIPrefs.h

  Brian Gunlogson
  Joshua Haberman
  James Crook

**********************************************************************/

#ifndef __AUDACITY_GUI_PREFS__
#define __AUDACITY_GUI_PREFS__

#include <wx/defs.h>

#include <wx/arrstr.h>
#include <wx/window.h>

#include "../ShuttleGui.h"

#include "PrefsPanel.h"

class GUIPrefs:public PrefsPanel
{
 public:
   GUIPrefs(wxWindow * parent);
   ~GUIPrefs();
   virtual bool Apply();

 private:
   void Populate();
   void PopulateOrExchange(ShuttleGui & S);

   wxArrayString mLangCodes;
   wxArrayString mLangNames;

   wxArrayString mHtmlHelpCodes;
   wxArrayString mHtmlHelpChoices;

   wxArrayString mRangeCodes;
   wxArrayString mRangeChoices;
};

#endif
