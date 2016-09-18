/**********************************************************************

  Audacity: A Digital Audio Editor

  ThemePrefs.h

  James Crook

  Audacity is free software.
  This file is licensed under the wxWidgets license, see License.txt

**********************************************************************/

#ifndef __AUDACITY_THEME_PREFS__
#define __AUDACITY_THEME_PREFS__

#include <wx/defs.h>
#include <wx/window.h>

#include "PrefsPanel.h"

class ShuttleGui;

class ThemePrefs final : public PrefsPanel
{
 public:
   ThemePrefs(wxWindow * parent);
   ~ThemePrefs(void);
   bool Apply() override;

 private:
   void Populate();
   void PopulateOrExchange(ShuttleGui & S);
   void OnLoadThemeComponents(wxCommandEvent & e);
   void OnSaveThemeComponents(wxCommandEvent & e);
   void OnLoadThemeCache(wxCommandEvent & e);
   void OnSaveThemeCache(wxCommandEvent & e);
   void OnReadThemeInternal(wxCommandEvent & e);
   void OnSaveThemeAsCode(wxCommandEvent & e);

   DECLARE_EVENT_TABLE()
};

class ThemePrefsFactory final : public PrefsPanelFactory
{
public:
   PrefsPanel *Create(wxWindow *parent) override;
};
#endif
