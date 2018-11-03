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

#include "PrefsPanel.h"

class ShuttleGui;

class GUIPrefs final : public PrefsPanel
{
 public:
   GUIPrefs(wxWindow * parent, wxWindowID winid);
   ~GUIPrefs();
   bool Commit() override;
   wxString HelpPageName() override;
   void PopulateOrExchange(ShuttleGui & S) override;

   static void GetRangeChoices(wxArrayString *pChoices, wxArrayString *pCodes);

 private:
   void Populate();

   wxArrayString mLangCodes;
   wxArrayString mLangNames;

   wxArrayString mHtmlHelpCodes;
   wxArrayString mHtmlHelpChoices;

   wxArrayString mThemeCodes;
   wxArrayString mThemeChoices;

   wxArrayString mRangeCodes;
   wxArrayString mRangeChoices;
};

/// A PrefsPanelFactory that creates one GUIPrefs panel.
class GUIPrefsFactory final : public PrefsPanelFactory
{
public:
   PrefsPanel *operator () (wxWindow *parent, wxWindowID winid) override;
};
#endif
