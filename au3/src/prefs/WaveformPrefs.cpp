/**********************************************************************

Audacity: A Digital Audio Editor

WaveformPrefs.cpp

Paul Licameli

*******************************************************************//**

\class WaveformPrefs
\brief A PrefsPanel for spectrum settings.

*//*******************************************************************/

#include "WaveformPrefs.h"

#include "GUIPrefs.h"
#include "Decibels.h"

#include <wx/checkbox.h>
#include <wx/choice.h>

#include "Project.h"

#include "../TrackPanel.h"
#include "ShuttleGui.h"
#include "WaveTrack.h"
#include "../tracks/playabletrack/wavetrack/ui/WaveChannelView.h"
#include "WaveChannelViewConstants.h"

WaveformPrefs::WaveformPrefs(wxWindow* parent, wxWindowID winid,
                             AudacityProject* pProject, WaveChannel* wc)
/* i18n-hint: A waveform is a visual representation of vibration */
    : PrefsPanel(parent, winid, XO("Waveforms"))
    , mProject{pProject}
    , mWc(wc)
    , mPopulating(false)
{
    if (mWc) {
        auto& settings = WaveformSettings::Get(*wc);
        mDefaulted = (&WaveformSettings::defaults() == &settings);
        mTempSettings = settings;
    } else {
        mTempSettings = WaveformSettings::defaults();
        mDefaulted = false;
    }

    mTempSettings.ConvertToEnumeratedDBRange();
    Populate();
}

WaveformPrefs::~WaveformPrefs()
{
}

ComponentInterfaceSymbol WaveformPrefs::GetSymbol() const
{
    return WAVEFORM_PREFS_PLUGIN_SYMBOL;
}

TranslatableString WaveformPrefs::GetDescription() const
{
    return XO("Preferences for Waveforms");
}

ManualPageID WaveformPrefs::HelpPageName()
{
    return "Waveform_Preferences";
}

enum {
    ID_DEFAULTS = 10001,

    ID_SCALE,
    ID_RANGE,
};

void WaveformPrefs::Populate()
{
    // Reuse the same choices and codes as for Interface prefs
    WaveformSettings::GetRangeChoices(&mRangeChoices, &mRangeCodes);

    //------------------------- Main section --------------------
    // Now construct the GUI itself.
    ShuttleGui S(this, eIsCreatingFromPrefs);
    PopulateOrExchange(S);
    // ----------------------- End of main section --------------
}

void WaveformPrefs::PopulateOrExchange(ShuttleGui& S)
{
    mPopulating = true;

    S.SetBorder(2);
    S.StartScroller();

    // S.StartStatic(XO("Track Settings"));
    {
        mDefaultsCheckbox = 0;
        if (mWc) {
            /* i18n-hint: use is a verb */
            mDefaultsCheckbox = S.Id(ID_DEFAULTS).TieCheckBox(XXO("&Use Preferences"), mDefaulted);
        }

        S.StartStatic(XO("Display"));
        {
            S.StartTwoColumn();
            {
                mScaleChoice
                    =S.Id(ID_SCALE).TieChoice(XXO("S&cale:"),
                                              mTempSettings.scaleType,
                                              Msgids(WaveformSettings::GetScaleNames()));

                mRangeChoice
                    =S.Id(ID_RANGE).TieChoice(XXO("Waveform dB &range:"),
                                              mTempSettings.dBRange,
                                              mRangeChoices);
            }
            S.EndTwoColumn();
        }
        S.EndStatic();
    }
    // S.EndStatic();

    /*
    S.StartStatic(XO("Global settings"));
    {
    }
    S.EndStatic();
    */

    S.EndScroller();

    EnableDisableRange();

    mPopulating = false;
}

bool WaveformPrefs::Validate()
{
    // Do checking for whole numbers

    // ToDo: use wxIntegerValidator<unsigned> when available

    ShuttleGui S(this, eIsGettingFromDialog);
    PopulateOrExchange(S);

    // Delegate range checking to WaveformSettings class
    mTempSettings.ConvertToActualDBRange();
    const bool result = mTempSettings.Validate(false);
    mTempSettings.ConvertToEnumeratedDBRange();
    return result;
}

bool WaveformPrefs::Commit()
{
    const bool isOpenPage = this->IsShown();

    ShuttleGui S(this, eIsGettingFromDialog);
    PopulateOrExchange(S);

    mTempSettings.ConvertToActualDBRange();
    WaveformSettings::Globals::Get().SavePrefs();

    if (mWc) {
        if (mDefaulted) {
            WaveformSettings::Set(*mWc, {});
        } else {
            auto& settings = WaveformSettings::Get(*mWc);
            settings = mTempSettings;
        }
    }

    WaveformSettings* const pSettings = &WaveformSettings::defaults();
    if (!mWc || mDefaulted) {
        *pSettings = mTempSettings;
        pSettings->SavePrefs();
    }
    pSettings->LoadPrefs(); // always; in case Globals changed

    mTempSettings.ConvertToEnumeratedDBRange();

    if (mWc && isOpenPage) {
        WaveChannelView::Get(*mWc).SetDisplay(WaveChannelViewConstants::Waveform);
    }

    if (isOpenPage) {
        if (mProject) {
            auto& tp = TrackPanel::Get(*mProject);
            tp.UpdateVRulers();
            tp.Refresh(false);
        }
    }

    return true;
}

bool WaveformPrefs::ShowsPreviewButton()
{
    return true;
}

void WaveformPrefs::OnControl(wxCommandEvent&)
{
    // Common routine for most controls
    // If any per-track setting is changed, break the association with defaults
    // Skip this, and View Settings... will be able to change defaults instead
    // when the checkbox is on, as in the original design.

    if (mDefaultsCheckbox && !mPopulating) {
        mDefaulted = false;
        mDefaultsCheckbox->SetValue(false);
    }
}

void WaveformPrefs::OnScale(wxCommandEvent& e)
{
    EnableDisableRange();

    // do the common part
    OnControl(e);
}

void WaveformPrefs::OnDefaults(wxCommandEvent&)
{
    if (mDefaultsCheckbox->IsChecked()) {
        mTempSettings = WaveformSettings::defaults();
        mTempSettings.ConvertToEnumeratedDBRange();
        mDefaulted = true;
        ShuttleGui S(this, eIsSettingToDialog);
        PopulateOrExchange(S);
    }
}

void WaveformPrefs::EnableDisableRange()
{
    mRangeChoice->Enable(
        mScaleChoice->GetSelection() == WaveformSettings::stLogarithmicDb);
}

BEGIN_EVENT_TABLE(WaveformPrefs, PrefsPanel)

EVT_CHOICE(ID_SCALE, WaveformPrefs::OnScale)
EVT_CHOICE(ID_RANGE, WaveformPrefs::OnControl)

EVT_CHECKBOX(ID_DEFAULTS, WaveformPrefs::OnDefaults)
END_EVENT_TABLE()

PrefsPanel::Factory
WaveformPrefsFactory(WaveChannel* wc)
{
    return [=](wxWindow* parent, wxWindowID winid, AudacityProject* pProject)
    {
        wxASSERT(parent); // to justify safenew
        return safenew WaveformPrefs(parent, winid, pProject, wc);
    };
}

#if 0
namespace {
PrefsPanel::Registration sAttachment{ "Waveform",
                                      WaveformPrefsFactory(nullptr),
                                      false,
                                      // Register with an explicit ordering hint because this one is
                                      // only conditionally compiled; and place it at a lower tree level
                                      { "Tracks", { Registry::OrderingHint::Before, "Spectrum" } }
};
}
#endif
