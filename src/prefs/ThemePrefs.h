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
#include <wx/event.h> // to declare a custom event type

#include "PrefsPanel.h"

class ShuttleGui;

#define THEME_PREFS_PLUGIN_SYMBOL ComponentInterfaceSymbol{ XO("Theme") }

class ThemePrefs final : public PrefsPanel
{
 public:
   ThemePrefs(wxWindow * parent, wxWindowID winid);
   ~ThemePrefs(void);
   ComponentInterfaceSymbol GetSymbol() const override;
   TranslatableString GetDescription() const override;

   bool Commit() override;
   void Cancel() override;
   ManualPageID HelpPageName() override;

 private:
   void Populate();
   void PopulateOrExchange(ShuttleGui & S) override;
   void OnLoadThemeComponents(wxCommandEvent & e);
   void OnSaveThemeComponents(wxCommandEvent & e);
   void OnLoadThemeCache(wxCommandEvent & e);
   void OnSaveThemeCache(wxCommandEvent & e);
   void OnReadThemeInternal(wxCommandEvent & e);
   void OnSaveThemeAsCode(wxCommandEvent & e);

   DECLARE_EVENT_TABLE()
};

#endif
