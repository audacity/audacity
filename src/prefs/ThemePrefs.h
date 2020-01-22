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

// An event sent to the application when the user changes choice of theme
wxDECLARE_EXPORTED_EVENT(AUDACITY_DLL_API,
                         EVT_THEME_CHANGE, wxCommandEvent);

#define THEME_PREFS_PLUGIN_SYMBOL ComponentInterfaceSymbol{ XO("Theme") }

class ThemePrefs final : public PrefsPanel
{
 public:
   ThemePrefs(wxWindow * parent, wxWindowID winid);
   ~ThemePrefs(void);
   ComponentInterfaceSymbol GetSymbol() override;
   TranslatableString GetDescription() override;

   bool Commit() override;
   wxString HelpPageName() override;

   static void ApplyUpdatedImages();

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
