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
class BoolSetting;
class ShuttleGui;

#define GUI_PREFS_PLUGIN_SYMBOL ComponentInterfaceSymbol { XO("GUI") }

class AUDACITY_DLL_API GUIPrefs final : public PrefsPanel
{
public:
    GUIPrefs(wxWindow* parent, wxWindowID winid);
    ~GUIPrefs();
    ComponentInterfaceSymbol GetSymbol() const override;
    TranslatableString GetDescription() const override;

    bool Commit() override;
    ManualPageID HelpPageName() override;
    void PopulateOrExchange(ShuttleGui& S) override;

private:
    void Populate();

    wxArrayStringEx mLangCodes;
    TranslatableStrings mLangNames;

    wxArrayStringEx mRangeCodes;
    TranslatableStrings mRangeChoices;
    int mDefaultRangeIndex;
};

AUDACITY_DLL_API BoolSetting& ShowRMSPref();
AUDACITY_DLL_API BoolSetting& ShowClippingPref();

#endif
