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

class ChoiceSetting;
class ShuttleGui;
class wxArrayStringEx;

#define GUI_PREFS_PLUGIN_SYMBOL ComponentInterfaceSymbol{ XO("GUI") }

class GUIPrefs final : public PrefsPanel
{
 public:
   GUIPrefs(wxWindow * parent, wxWindowID winid);
   ~GUIPrefs();
   ComponentInterfaceSymbol GetSymbol() override;
   wxString GetDescription() override;

   bool Commit() override;
   wxString HelpPageName() override;
   void PopulateOrExchange(ShuttleGui & S) override;

   static void GetRangeChoices(
      TranslatableStrings *pChoicesUntranslated,
      wxArrayStringEx *pChoicesTranslated,
      wxArrayStringEx *pCodes,
      int *pDefaultRangeIndex = nullptr
   );

   // If no input language given, defaults first to choice in preferences, then
   // to system language.
   // Returns the language actually used which is not lang if lang cannot be found.
   static wxString InitLang( wxString lang = {} );

   // If no input language given, defaults to system language.
   // Returns the language actually used which is not lang if lang cannot be found.
   static wxString SetLang( const wxString & lang );

   // Returns the last language code that was set
   static wxString GetLang();

 private:
   void Populate();

   wxArrayStringEx mLangCodes;
   TranslatableStrings mLangNames;

   wxArrayStringEx mRangeCodes;
   TranslatableStrings mRangeChoices;
   int mDefaultRangeIndex;
};

/// A PrefsPanel::Factory that creates one GUIPrefs panel.
extern PrefsPanel::Factory GUIPrefsFactory;

int ShowClippingPrefsID();

extern ChoiceSetting
     GUIManualLocation
   , GUITheme
;

#endif
