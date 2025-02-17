/**********************************************************************

  Audacity: A Digital Audio Editor

  QualityPrefs.cpp

  Joshua Haberman
  James Crook


*******************************************************************//**

\class QualityPrefs
\brief A PrefsPanel used for setting audio quality.

*//*******************************************************************/

#include "QualityPrefs.h"
#include "QualitySettings.h"

#include <wx/choice.h>
#include <wx/defs.h>
#include <wx/textctrl.h>

#include "AudioIOBase.h"
#include "Dither.h"
#include "Prefs.h"
#include "Resample.h"
#include "ShuttleGui.h"

//////////
BEGIN_EVENT_TABLE(QualityPrefs, PrefsPanel)
END_EVENT_TABLE()

QualityPrefs::QualityPrefs(wxWindow* parent, wxWindowID winid)
/* i18n-hint: meaning accuracy in reproduction of sounds */
    :  PrefsPanel(parent, winid, XO("Quality"))
{
    Populate();
}

QualityPrefs::~QualityPrefs()
{
}

ComponentInterfaceSymbol QualityPrefs::GetSymbol() const
{
    return QUALITY_PREFS_PLUGIN_SYMBOL;
}

TranslatableString QualityPrefs::GetDescription() const
{
    return XO("Preferences for Quality");
}

ManualPageID QualityPrefs::HelpPageName()
{
    return "Quality_Preferences";
}

void QualityPrefs::Populate()
{
    //------------------------- Main section --------------------
    // Now construct the GUI itself.
    // Use 'eIsCreatingFromPrefs' so that the GUI is
    // initialised with values from gPrefs.
    ShuttleGui S(this, eIsCreatingFromPrefs);
    PopulateOrExchange(S);
    // ----------------------- End of main section --------------
}

void QualityPrefs::PopulateOrExchange(ShuttleGui& S)
{
    S.SetBorder(2);
    S.StartScroller();

    S.StartStatic(XO("Real-time Conversion"));
    {
        S.StartMultiColumn(2, wxEXPAND);
        {
            S.TieChoice(XXO("Sample Rate Con&verter:"),
                        Resample::FastMethodSetting);

            /* i18n-hint: technical term for randomization to reduce undesirable resampling artifacts */
            S.TieChoice(XXO("&Dither:"),
                        Dither::FastSetting);
        }
        S.EndMultiColumn();
    }
    S.EndStatic();

    S.StartStatic(XO("High-quality Conversion"));
    {
        S.StartMultiColumn(2);
        {
            S.TieChoice(XXO("Sample Rate Conver&ter:"),
                        Resample::BestMethodSetting);

            /* i18n-hint: technical term for randomization to reduce undesirable resampling artifacts */
            S.TieChoice(XXO("Dit&her:"),
                        Dither::BestSetting);
        }
        S.EndMultiColumn();
    }
    S.EndStatic();
    S.EndScroller();
}

bool QualityPrefs::Commit()
{
    ShuttleGui S(this, eIsSavingToPrefs);
    PopulateOrExchange(S);

    // Tell CopySamples() to use these ditherers now
    InitDitherers();

    return true;
}

namespace {
PrefsPanel::Registration sAttachment{ "Quality",
                                      [](wxWindow* parent, wxWindowID winid, AudacityProject*)
    {
        wxASSERT(parent); // to justify safenew
        return safenew QualityPrefs(parent, winid);
    }
};
}
