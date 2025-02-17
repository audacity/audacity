/**********************************************************************

  Audacity: A Digital Audio Editor

  GUIPrefs.cpp

  Brian Gunlogson
  Joshua Haberman
  Dominic Mazzoni
  James Crook


*******************************************************************//**

\class GUIPrefs
\brief A PrefsPanel for general GUI preferences.

*//*******************************************************************/

#include "GUIPrefs.h"

#include <wx/defs.h>
#include <mutex> // once_flag

#include "FileNames.h"
#include "Languages.h"
#include "Theme.h"
#include "Prefs.h"
#include "ShuttleGui.h"

#include "Decibels.h"
#include "Beats.h"

#include "AColor.h"
#include "GUISettings.h"
#include "WaveformSettings.h"

GUIPrefs::GUIPrefs(wxWindow* parent, wxWindowID winid)
/* i18n-hint: refers to Audacity's user interface settings */
    :  PrefsPanel(parent, winid, XC("Interface", "GUI"))
{
    Populate();
}

GUIPrefs::~GUIPrefs()
{
}

ComponentInterfaceSymbol GUIPrefs::GetSymbol() const
{
    return GUI_PREFS_PLUGIN_SYMBOL;
}

TranslatableString GUIPrefs::GetDescription() const
{
    return XO("Preferences for GUI");
}

ManualPageID GUIPrefs::HelpPageName()
{
    return "Interface_Preferences";
}

void GUIPrefs::Populate()
{
    // First any pre-processing for constructing the GUI.
    Languages::GetLanguages(
        FileNames::AudacityPathList(), mLangCodes, mLangNames);

    WaveformSettings::GetRangeChoices(
        &mRangeChoices, &mRangeCodes, &mDefaultRangeIndex);

#if 0
    mLangCodes.insert(mLangCodes.end(), {
        // only for testing...
        "kg",
        "ep",
    });

    mLangNames.insert(mLangNames.end(), {
        "Klingon",
        "Esperanto",
    });
#endif

    //------------------------- Main section --------------------
    // Now construct the GUI itself.
    // Use 'eIsCreatingFromPrefs' so that the GUI is
    // initialised with values from gPrefs.
    ShuttleGui S(this, eIsCreatingFromPrefs);
    PopulateOrExchange(S);
    // ----------------------- End of main section --------------
}

void GUIPrefs::PopulateOrExchange(ShuttleGui& S)
{
    ChoiceSetting LanguageSetting{ wxT("/Locale/Language"),
                                   { ByColumns, mLangNames, mLangCodes }
    };
    ChoiceSetting DBSetting{ DecibelScaleCutoff,
                             { ByColumns, mRangeChoices, mRangeCodes },
                             mDefaultRangeIndex
    };

    S.SetBorder(2);
    S.StartScroller();

    S.StartStatic(XO("Display"));
    {
        S.StartMultiColumn(2);
        {
            S.TieChoice(XXO("&Language:"), LanguageSetting);
            // S.TieChoice( XXO("Location of &Manual:"), GUIManualLocation);
            S.TieChoice(XXO("Th&eme:"), GUITheme());
            S.TieChoice(XXO("Meter dB &range:"), DBSetting);
        }
        S.EndMultiColumn();
    }
    S.EndStatic();

    S.StartStatic(XO("Options"));
    {
        // Start wording of options with a verb, if possible.
        S.TieCheckBox(XXO("Show 'How to Get &Help' at launch"),
                      { wxT("/GUI/ShowSplashScreen"),
                        true });
        S.TieCheckBox(XXO("Show e&xtra menus"),
                      { wxT("/GUI/ShowExtraMenus"),
                        false });
        S.TieCheckBox(XXO("&Beep on completion of longer activities"),
                      { wxT("/GUI/BeepOnCompletion"),
                        false });
        S.TieCheckBox(XXO("Re&tain labels if selection snaps to a label"),
                      { wxT("/GUI/RetainLabels"),
                        false });
#ifndef __WXMAC__
        /* i18n-hint: RTL stands for 'Right to Left'  */
        S.TieCheckBox(XXO("Use mostly Left-to-Right layouts in RTL languages"),
                      { "/GUI/RtlWorkaround",
                        true });
#endif
    }
    S.EndStatic();

    S.EndScroller();
}

bool GUIPrefs::Commit()
{
    ShuttleGui S(this, eIsSavingToPrefs);
    PopulateOrExchange(S);

    // If language has changed, we want to change it now, not on the next reboot.
    wxString lang = gPrefs->Read(wxT("/Locale/Language"), wxT(""));
    wxString usedLang = GUISettings::SetLang(lang);
    // Bug 1523: Previously didn't check no-language (=System Language)
    if (!(lang.empty() || lang == L"System") && (lang != usedLang)) {
        // lang was not usable and is not system language.  We got overridden.
        gPrefs->Write(wxT("/Locale/Language"), usedLang);
        gPrefs->Flush();
    }

    // Reads preference GUITheme
    {
        wxBusyCursor busy;
        theTheme.LoadPreferredTheme();
        theTheme.DeleteUnusedThemes();
    }
    AColor::ApplyUpdatedImages();

    DecibelScaleCutoff.Invalidate();

    return true;
}

BoolSetting& ShowRMSPref()
{
    static BoolSetting pref { "/GUI/ShowRMS", false };
    return pref;
}

BoolSetting& ShowClippingPref()
{
    static BoolSetting pref { "/GUI/ShowClipping", false };
    return pref;
}

namespace {
PrefsPanel::Registration sAttachment{ "GUI",
                                      [](wxWindow* parent, wxWindowID winid, AudacityProject*)
    {
        wxASSERT(parent); // to justify safenew
        return safenew GUIPrefs(parent, winid);
    }
};
}
