/**********************************************************************

  Audacity: A Digital Audio Editor

  ChangeSpeed.cpp

  Vaughan Johnson, Dominic Mazzoni

*******************************************************************//**

\class ChangeSpeedBase
\brief An Effect that affects both pitch & speed.

*//*******************************************************************/

#include "ChangeSpeed.h"
#include "ConfigInterface.h"
#include "EffectEditor.h"
#include "LoadEffects.h"

#include <wx/choice.h>
#include <wx/slider.h>

#include "ShuttleGui.h"
#include "../widgets/NumericTextCtrl.h"
#include "../widgets/valnum.h"

#include "NumericConverterFormats.h"

enum
{
    ID_PercentChange = 10000,
    ID_Multiplier,
    ID_FromVinyl,
    ID_ToVinyl,
    ID_ToLength
};

static const TranslatableStrings kVinylStrings {
    XO("33\u2153"),
    XO("45"),
    XO("78"),
    /* i18n-hint: n/a is an English abbreviation meaning "not applicable". */
    XO("n/a"),
};

// We warp the slider to go up to 400%, but user can enter higher values
static const double kSliderMax = 100.0;         // warped above zero to actually go up to 400%
static const double kSliderWarp = 1.30105;      // warp power takes max from 100 to 400.

namespace {
BuiltinEffectsModule::Registration< EffectChangeSpeed > reg;
}

BEGIN_EVENT_TABLE(EffectChangeSpeed, wxEvtHandler)
EVT_TEXT(ID_PercentChange, EffectChangeSpeed::OnText_PercentChange)
EVT_TEXT(ID_Multiplier, EffectChangeSpeed::OnText_Multiplier)
EVT_SLIDER(ID_PercentChange, EffectChangeSpeed::OnSlider_PercentChange)
EVT_CHOICE(ID_FromVinyl, EffectChangeSpeed::OnChoice_Vinyl)
EVT_CHOICE(ID_ToVinyl, EffectChangeSpeed::OnChoice_Vinyl)
EVT_TEXT(ID_ToLength, EffectChangeSpeed::OnTimeCtrl_ToLength)
EVT_COMMAND(ID_ToLength, EVT_TIMETEXTCTRL_UPDATED, EffectChangeSpeed::OnTimeCtrlUpdate)
END_EVENT_TABLE()

std::unique_ptr<EffectEditor> EffectChangeSpeed::PopulateOrExchange(
    ShuttleGui& S, EffectInstance&, EffectSettingsAccess&,
    const EffectOutputs*)
{
    mUIParent = S.GetParent();

    {
        wxString formatId;
        GetConfig(GetDefinition(), PluginSettings::Private,
                  CurrentSettingsGroup(),
                  wxT("TimeFormat"), formatId, mFormat.GET());
        mFormat = NumericConverterFormats::Lookup(
            FormatterContext::SampleRateContext(mProjectRate),
            NumericConverterType_TIME(), formatId).Internal();
    }
    GetConfig(GetDefinition(), PluginSettings::Private,
              CurrentSettingsGroup(),
              wxT("VinylChoice"), mFromVinyl, mFromVinyl);

    S.SetBorder(5);

    S.StartVerticalLay(0);
    {
        // Speed multiplier and percent change controls.
        S.StartMultiColumn(4, wxCENTER);
        {
            mpTextCtrl_Multiplier = S.Id(ID_Multiplier)
                                    .Validator<FloatingPointValidator<double> >(
                3, &mMultiplier,
                NumValidatorStyle::THREE_TRAILING_ZEROES,
                Percentage.min / 100.0, ((Percentage.max / 100.0) + 1))
                                    .AddTextBox(XXO("&Speed Multiplier:"), L"", 12);

            mpTextCtrl_PercentChange = S.Id(ID_PercentChange)
                                       .Validator<FloatingPointValidator<double> >(
                3, &m_PercentChange,
                NumValidatorStyle::THREE_TRAILING_ZEROES,
                Percentage.min, Percentage.max)
                                       .AddTextBox(XXO("Percent C&hange:"), L"", 12);
        }
        S.EndMultiColumn();

        // Percent change slider.
        S.StartHorizontalLay(wxEXPAND);
        {
            mpSlider_PercentChange = S.Id(ID_PercentChange)
                                     .Name(XO("Percent Change"))
                                     .Style(wxSL_HORIZONTAL)
                                     .AddSlider({}, 0, (int)kSliderMax, (int)Percentage.min);
        }
        S.EndHorizontalLay();

        // Vinyl rpm controls.
        S.StartMultiColumn(5, wxCENTER);
        {
            /* i18n-hint: "rpm" is an English abbreviation meaning "revolutions per minute".
               "vinyl" refers to old-fashioned phonograph records */
            S.AddUnits(XO("Standard Vinyl rpm:"));

            mpChoice_FromVinyl = S.Id(ID_FromVinyl)
                                 /* i18n-hint: changing speed of audio "from" one value "to" another
                                  "rpm" means "revolutions per minute" as on a vinyl record turntable
                                  */
                                 .Name(XO("From rpm"))
                                 .MinSize({ 100, -1 })
                                 /* i18n-hint: changing speed of audio "from" one value "to" another */
                                 .AddChoice(XXC("&from", "change speed"), kVinylStrings);

            mpChoice_ToVinyl = S.Id(ID_ToVinyl)
                               /* i18n-hint: changing speed of audio "from" one value "to" another
                                "rpm" means "revolutions per minute" as on a vinyl record turntable
                                */
                               .Name(XO("To rpm"))
                               .MinSize({ 100, -1 })
                               /* i18n-hint: changing speed of audio "from" one value "to" another */
                               .AddChoice(XXC("&to", "change speed"), kVinylStrings);
        }
        S.EndMultiColumn();

        // From/To time controls.
        S.StartStatic(XO("Selection Length"), 0);
        {
            S.StartMultiColumn(2, wxALIGN_LEFT);
            {
                S.AddPrompt(XXO("C&urrent Length:"));

                mpFromLengthCtrl = safenew
                                       NumericTextCtrl(FormatterContext::SampleRateContext(mProjectRate),
                                                       S.GetParent(), wxID_ANY,
                                                       NumericConverterType_TIME(),
                                                       mFormat,
                                                       mFromLength,
                                                       NumericTextCtrl::Options{}
                                                       .ReadOnly(true)
                                                       .MenuEnabled(false));

                S.ToolTip(XO("Current length of selection."))
                /* i18n-hint: changing speed of audio "from" one value "to" another */
                .Name(XC("from", "change speed"))
                .Position(wxALIGN_LEFT)
                .AddWindow(mpFromLengthCtrl);

                S.AddPrompt(XXO("&New Length:"));

                mpToLengthCtrl = safenew
                                     NumericTextCtrl(FormatterContext::SampleRateContext(mProjectRate),
                                                     S.GetParent(), ID_ToLength,
                                                     NumericConverterType_TIME(),
                                                     mFormat,
                                                     mToLength);

                /* i18n-hint: changing speed of audio "from" one value "to" another */
                S.Name(XC("to", "change speed"))
                .Position(wxALIGN_LEFT)
                .AddWindow(mpToLengthCtrl);
            }
            S.EndMultiColumn();
        }
        S.EndStatic();
    }
    S.EndVerticalLay();
    return nullptr;
}

bool EffectChangeSpeed::TransferDataToWindow(const EffectSettings&)
{
    mbLoopDetect = true;

    if (!mUIParent->TransferDataToWindow()) {
        return false;
    }

    if (mFromVinyl == kVinyl_NA) {
        mFromVinyl = kVinyl_33AndAThird;
    }

    Update_Text_PercentChange();
    Update_Text_Multiplier();
    Update_Slider_PercentChange();
    Update_TimeCtrl_ToLength();

    // Set from/to Vinyl controls - mFromVinyl must be set first.
    mpChoice_FromVinyl->SetSelection(mFromVinyl);
    // Then update to get correct mToVinyl.
    Update_Vinyl();
    // Then update ToVinyl control.
    mpChoice_ToVinyl->SetSelection(mToVinyl);

    // Set From Length control.
    // Set the format first so we can get sample accuracy.
    mpFromLengthCtrl->SetFormatName(mFormat);
    mpFromLengthCtrl->SetValue(mFromLength);

    mbLoopDetect = false;

    return true;
}

bool EffectChangeSpeed::TransferDataFromWindow(EffectSettings&)
{
    // mUIParent->TransferDataFromWindow() loses some precision, so save and restore it.
    double exactPercent = m_PercentChange;
    if (!mUIParent->Validate() || !mUIParent->TransferDataFromWindow()) {
        return false;
    }
    m_PercentChange = exactPercent;

    // TODO: just visit these effect settings the default way
    SetConfig(GetDefinition(), PluginSettings::Private,
              CurrentSettingsGroup(), wxT("TimeFormat"), mFormat.GET());
    SetConfig(GetDefinition(), PluginSettings::Private,
              CurrentSettingsGroup(), wxT("VinylChoice"), mFromVinyl);

    return true;
}

// handler implementations for ChangeSpeedBase

void EffectChangeSpeed::OnText_PercentChange(wxCommandEvent& WXUNUSED(evt))
{
    if (mbLoopDetect) {
        return;
    }

    mpTextCtrl_PercentChange->GetValidator()->TransferFromWindow();
    UpdateUI();

    mbLoopDetect = true;
    Update_Text_Multiplier();
    Update_Slider_PercentChange();
    Update_Vinyl();
    Update_TimeCtrl_ToLength();
    mbLoopDetect = false;
}

void EffectChangeSpeed::OnText_Multiplier(wxCommandEvent& WXUNUSED(evt))
{
    if (mbLoopDetect) {
        return;
    }

    mpTextCtrl_Multiplier->GetValidator()->TransferFromWindow();
    m_PercentChange = 100 * (mMultiplier - 1);
    UpdateUI();

    mbLoopDetect = true;
    Update_Text_PercentChange();
    Update_Slider_PercentChange();
    Update_Vinyl();
    Update_TimeCtrl_ToLength();
    mbLoopDetect = false;
}

void EffectChangeSpeed::OnSlider_PercentChange(wxCommandEvent& WXUNUSED(evt))
{
    if (mbLoopDetect) {
        return;
    }

    m_PercentChange = (double)(mpSlider_PercentChange->GetValue());
    // Warp positive values to actually go up faster & further than negatives.
    if (m_PercentChange > 0.0) {
        m_PercentChange = pow(m_PercentChange, kSliderWarp);
    }
    UpdateUI();

    mbLoopDetect = true;
    Update_Text_PercentChange();
    Update_Text_Multiplier();
    Update_Vinyl();
    Update_TimeCtrl_ToLength();
    mbLoopDetect = false;
}

void EffectChangeSpeed::OnChoice_Vinyl(wxCommandEvent& WXUNUSED(evt))
{
    // Treat mpChoice_FromVinyl and mpChoice_ToVinyl as one control since we need
    // both to calculate Percent Change.
    mFromVinyl = mpChoice_FromVinyl->GetSelection();
    mToVinyl = mpChoice_ToVinyl->GetSelection();
    // Use this as the 'preferred' choice.
    if (mFromVinyl != kVinyl_NA) {
        SetConfig(GetDefinition(), PluginSettings::Private,
                  CurrentSettingsGroup(), wxT("VinylChoice"), mFromVinyl);
    }

    // If mFromVinyl & mToVinyl are set, then there's a NEW percent change.
    if ((mFromVinyl != kVinyl_NA) && (mToVinyl != kVinyl_NA)) {
        double fromRPM;
        double toRPM;
        switch (mFromVinyl) {
        default:
        case kVinyl_33AndAThird:   fromRPM = 33.0 + (1.0 / 3.0);
            break;
        case kVinyl_45:            fromRPM = 45.0;
            break;
        case kVinyl_78:            fromRPM = 78;
            break;
        }
        switch (mToVinyl) {
        default:
        case kVinyl_33AndAThird:   toRPM = 33.0 + (1.0 / 3.0);
            break;
        case kVinyl_45:            toRPM = 45.0;
            break;
        case kVinyl_78:            toRPM = 78;
            break;
        }
        m_PercentChange = ((toRPM * 100.0) / fromRPM) - 100.0;
        UpdateUI();

        mbLoopDetect = true;
        Update_Text_PercentChange();
        Update_Text_Multiplier();
        Update_Slider_PercentChange();
        Update_TimeCtrl_ToLength();
    }
    mbLoopDetect = false;
}

void EffectChangeSpeed::OnTimeCtrl_ToLength(wxCommandEvent& WXUNUSED(evt))
{
    if (mbLoopDetect) {
        return;
    }

    mToLength = mpToLengthCtrl->GetValue();
    // Division by (double) 0.0 is not an error and we want to show "infinite" in
    // text controls, so take care that we handle infinite values when they occur.
    m_PercentChange = ((mFromLength * 100.0) / mToLength) - 100.0;
    UpdateUI();

    mbLoopDetect = true;

    Update_Text_PercentChange();
    Update_Text_Multiplier();
    Update_Slider_PercentChange();
    Update_Vinyl();

    mbLoopDetect = false;
}

void EffectChangeSpeed::OnTimeCtrlUpdate(wxCommandEvent& evt)
{
    mFormat = NumericConverterFormats::Lookup(
        FormatterContext::SampleRateContext(mProjectRate),
        NumericConverterType_TIME(), evt.GetString()).Internal();

    mpFromLengthCtrl->SetFormatName(mFormat);
    // Update From/To Length controls (precision has changed).
    mpToLengthCtrl->SetValue(mToLength);
    mpFromLengthCtrl->SetValue(mFromLength);
}

// helper functions

void EffectChangeSpeed::Update_Text_PercentChange()
// Update Text Percent control from percent change.
{
    mpTextCtrl_PercentChange->GetValidator()->TransferToWindow();
}

void EffectChangeSpeed::Update_Text_Multiplier()
// Update Multiplier control from percent change.
{
    mMultiplier =  1 + (m_PercentChange) / 100.0;
    mpTextCtrl_Multiplier->GetValidator()->TransferToWindow();
}

void EffectChangeSpeed::Update_Slider_PercentChange()
// Update Slider Percent control from percent change.
{
    auto unwarped = std::min<double>(m_PercentChange, Percentage.max);
    if (unwarped > 0.0) {
        // Un-warp values above zero to actually go up to kSliderMax.
        unwarped = pow(m_PercentChange, (1.0 / kSliderWarp));
    }

    // Caution: m_PercentChange could be infinite.
    int unwarpedi = (int)(unwarped + 0.5);
    unwarpedi = std::min<int>(unwarpedi, (int)kSliderMax);

    mpSlider_PercentChange->SetValue(unwarpedi);
}

void EffectChangeSpeed::Update_Vinyl()
// Update Vinyl controls from percent change.
{
    // Match Vinyl rpm when within 0.01% of a standard ratio.
    // Ratios calculated as: ((toRPM / fromRPM) - 1) * 100 * 100

    // Caution: m_PercentChange could be infinite
    int ratio = (int)((m_PercentChange * 100) + 0.5);

    switch (ratio) {
    case 0:   // toRPM is the same as fromRPM
        if (mFromVinyl != kVinyl_NA) {
            mpChoice_ToVinyl->SetSelection(mpChoice_FromVinyl->GetSelection());
        } else {
            // Use the last saved option.
            GetConfig(GetDefinition(), PluginSettings::Private,
                      CurrentSettingsGroup(), wxT("VinylChoice"), mFromVinyl, 0);
            mpChoice_FromVinyl->SetSelection(mFromVinyl);
            mpChoice_ToVinyl->SetSelection(mFromVinyl);
        }
        break;
    case 3500:
        mpChoice_FromVinyl->SetSelection(kVinyl_33AndAThird);
        mpChoice_ToVinyl->SetSelection(kVinyl_45);
        break;
    case 13400:
        mpChoice_FromVinyl->SetSelection(kVinyl_33AndAThird);
        mpChoice_ToVinyl->SetSelection(kVinyl_78);
        break;
    case -2593:
        mpChoice_FromVinyl->SetSelection(kVinyl_45);
        mpChoice_ToVinyl->SetSelection(kVinyl_33AndAThird);
        break;
    case 7333:
        mpChoice_FromVinyl->SetSelection(kVinyl_45);
        mpChoice_ToVinyl->SetSelection(kVinyl_78);
        break;
    case -5727:
        mpChoice_FromVinyl->SetSelection(kVinyl_78);
        mpChoice_ToVinyl->SetSelection(kVinyl_33AndAThird);
        break;
    case -4231:
        mpChoice_FromVinyl->SetSelection(kVinyl_78);
        mpChoice_ToVinyl->SetSelection(kVinyl_45);
        break;
    default:
        mpChoice_ToVinyl->SetSelection(kVinyl_NA);
    }
    // and update variables.
    mFromVinyl = mpChoice_FromVinyl->GetSelection();
    mToVinyl = mpChoice_ToVinyl->GetSelection();
}

void EffectChangeSpeed::Update_TimeCtrl_ToLength()
// Update ToLength control from percent change.
{
    mToLength = (mFromLength * 100.0) / (100.0 + m_PercentChange);

    // Set the format first so we can get sample accuracy.
    mpToLengthCtrl->SetFormatName(mFormat);
    // Negative times do not make sense.
    // 359999 = 99h:59m:59s which is a little less disturbing than overflow characters
    // though it may still look a bit strange with some formats.
    mToLength = std::clamp<double>(mToLength, 0.0, 359999.0);
    mpToLengthCtrl->SetValue(mToLength);
}

void EffectChangeSpeed::UpdateUI()
// Disable OK and Preview if not in sensible range.
{
    EffectEditor::EnableApply(mUIParent,
                              m_PercentChange >= Percentage.min && m_PercentChange <= Percentage.max);
}
