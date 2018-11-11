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

#include "PrefsPanel.h"

class ShuttleGui;
class wxArrayStringEx;

class GUIPrefs final : public PrefsPanel
{
 public:
   GUIPrefs(wxWindow * parent, wxWindowID winid);
   ~GUIPrefs();
   bool Commit() override;
   wxString HelpPageName() override;
   void PopulateOrExchange(ShuttleGui & S) override;

   static void GetRangeChoices(
      wxArrayStringEx *pChoices, wxArrayStringEx *pCodes);

 private:
   void Populate();

   wxArrayStringEx mLangCodes;
   wxArrayStringEx mLangNames;

   wxArrayStringEx mHtmlHelpCodes;
   wxArrayStringEx mHtmlHelpChoices;

   wxArrayStringEx mThemeCodes;
   wxArrayStringEx mThemeChoices;

   wxArrayStringEx mRangeCodes;
   wxArrayStringEx mRangeChoices;
};

/// A PrefsPanelFactory that creates one GUIPrefs panel.
class GUIPrefsFactory final : public PrefsPanelFactory
{
public:
   PrefsPanel *operator () (wxWindow *parent, wxWindowID winid) override;
};
#endif
