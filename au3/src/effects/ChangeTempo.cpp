/**********************************************************************

  Audacity: A Digital Audio Editor

  ChangeTempo.cpp

  Vaughan Johnson,
  Dominic Mazzoni

*******************************************************************//**

\class EffectChangeTempo
\brief A SoundTouchBase provides speeding up or
  slowing down tempo without changing pitch.

*//*******************************************************************/
#if USE_SOUNDTOUCH
#include "ChangeTempo.h"
#include "EffectEditor.h"

#if USE_SBSMS
#include <wx/valgen.h>
#endif

#include <wx/checkbox.h>
#include <wx/slider.h>

#include "ShuttleGui.h"
#include "../widgets/valnum.h"

#include "LoadEffects.h"

enum
{
    ID_PercentChange = 10000,
    ID_FromBPM,
    ID_ToBPM,
    ID_FromLength,
    ID_ToLength
};

// We warp the slider to go up to 400%, but user can enter higher values.
static const double kSliderMax = 100.0;         // warped above zero to actually go up to 400%
static const double kSliderWarp = 1.30105;      // warp power takes max from 100 to 400.

namespace {
BuiltinEffectsModule::Registration< EffectChangeTempo > reg;
}

BEGIN_EVENT_TABLE(EffectChangeTempo, wxEvtHandler)
EVT_TEXT(ID_PercentChange, EffectChangeTempo::OnText_PercentChange)
EVT_SLIDER(ID_PercentChange, EffectChangeTempo::OnSlider_PercentChange)
EVT_TEXT(ID_FromBPM, EffectChangeTempo::OnText_FromBPM)
EVT_TEXT(ID_ToBPM, EffectChangeTempo::OnText_ToBPM)
EVT_TEXT(ID_ToLength, EffectChangeTempo::OnText_ToLength)
END_EVENT_TABLE()

std::unique_ptr<EffectEditor> EffectChangeTempo::PopulateOrExchange(
    ShuttleGui& S, EffectInstance&, EffectSettingsAccess&,
    const EffectOutputs*)
{
    mUIParent = S.GetParent();

    enum {
        precision = 2
    };

    S.StartVerticalLay(0);
    {
        //
        S.StartMultiColumn(2, wxCENTER);
        {
            m_pTextCtrl_PercentChange = S.Id(ID_PercentChange)
                                        .Validator<FloatingPointValidator<double> >(
                3, &m_PercentChange, NumValidatorStyle::THREE_TRAILING_ZEROES,
                Percentage.min, Percentage.max)
                                        .AddTextBox(XXO("Percent C&hange:"), L"", 12);
        }
        S.EndMultiColumn();

        //
        S.StartHorizontalLay(wxEXPAND);
        {
            m_pSlider_PercentChange = S.Id(ID_PercentChange)
                                      .Name(XO("Percent Change"))
                                      .Style(wxSL_HORIZONTAL)
                                      .AddSlider({}, 0, (int)kSliderMax, (int)Percentage.min);
        }
        S.EndHorizontalLay();

        S.StartStatic(XO("Beats per minute"));
        {
            S.StartHorizontalLay(wxALIGN_CENTER);
            {
                m_pTextCtrl_FromBPM = S.Id(ID_FromBPM)
                                      /* i18n-hint: changing tempo "from" one value "to" another */
                                      .Name(XO("Beats per minute, from"))
                                      .Validator<FloatingPointValidator<double> >(
                    3, &m_FromBPM,
                    NumValidatorStyle::THREE_TRAILING_ZEROES
                    | NumValidatorStyle::ZERO_AS_BLANK)
                                      /* i18n-hint: changing tempo "from" one value "to" another */
                                      .AddTextBox(XXC("&from", "change tempo"), wxT(""), 12);

                m_pTextCtrl_ToBPM = S.Id(ID_ToBPM)
                                    /* i18n-hint: changing tempo "from" one value "to" another */
                                    .Name(XO("Beats per minute, to"))
                                    .Validator<FloatingPointValidator<double> >(
                    3, &m_ToBPM,
                    NumValidatorStyle::THREE_TRAILING_ZEROES
                    | NumValidatorStyle::ZERO_AS_BLANK)
                                    /* i18n-hint: changing tempo "from" one value "to" another */
                                    .AddTextBox(XXC("&to", "change tempo"), wxT(""), 12);
            }
            S.EndHorizontalLay();
        }
        S.EndStatic();

        //
        S.StartStatic(XO("Length (seconds)"));
        {
            S.StartHorizontalLay(wxALIGN_CENTER);
            {
                m_pTextCtrl_FromLength = S.Id(ID_FromLength)
                                         .Disable() // Disable because the value comes from the
                                                    // user selection.
                                         .Validator<FloatingPointValidator<double> >(
                    precision, &m_FromLength,
                    NumValidatorStyle::TWO_TRAILING_ZEROES)
                                         /* i18n-hint: changing tempo "from" one value "to" another */
                                         .AddTextBox(XXC("from", "change tempo"), wxT(""), 12);
                m_pTextCtrl_ToLength = S.Id(ID_ToLength)
                                       .Validator<FloatingPointValidator<double> >(
                    2, &m_ToLength, NumValidatorStyle::TWO_TRAILING_ZEROES,
                    // min and max need same precision as what we're validating (bug 963)
                    RoundValue(precision,
                               (m_FromLength * 100.0) / (100.0 + Percentage.max)),
                    RoundValue(precision,
                               (m_FromLength * 100.0) / (100.0 + Percentage.min)))
                                       /* i18n-hint: changing tempo "from" one value "to" another */
                                       .AddTextBox(XXC("t&o", "change tempo"), wxT(""), 12);
            }
            S.EndHorizontalLay();
        }
        S.EndStatic();

#if USE_SBSMS
        S.StartMultiColumn(2);
        {
            mUseSBSMSCheckBox = S.Validator<wxGenericValidator>(&mUseSBSMS)
                                .AddCheckBox(XXO("&Use high quality stretching (slow)"),
                                             mUseSBSMS);
        }
        S.EndMultiColumn();
#endif
    }
    S.EndVerticalLay();

    return nullptr;
}

bool EffectChangeTempo::TransferDataToWindow(const EffectSettings&)
{
    // Reset from length because it can be changed by Preview
    m_FromLength = mT1 - mT0;

    m_bLoopDetect = true;

    if (!mUIParent->TransferDataToWindow()) {
        return false;
    }

    // percent change controls
    Update_Slider_PercentChange();
    Update_Text_ToBPM();
    Update_Text_ToLength();

    m_bLoopDetect = false;

    // Set the accessibility name here because we need m_pTextCtrl_FromLength to have had its value set
    m_pTextCtrl_ToLength->SetName(
        wxString::Format(_("Length in seconds from %s, to"),
                         m_pTextCtrl_FromLength->GetValue()));

    return true;
}

bool EffectChangeTempo::TransferDataFromWindow(EffectSettings&)
{
    if (!mUIParent->Validate() || !mUIParent->TransferDataFromWindow()) {
        return false;
    }

    return true;
}

// handler implementations for EffectChangeTempo

void EffectChangeTempo::OnText_PercentChange(wxCommandEvent& WXUNUSED(evt))
{
    if (m_bLoopDetect) {
        return;
    }

    m_pTextCtrl_PercentChange->GetValidator()->TransferFromWindow();

    m_bLoopDetect = true;
    Update_Slider_PercentChange();
    Update_Text_ToBPM();
    Update_Text_ToLength();
    m_bLoopDetect = false;
}

void EffectChangeTempo::OnSlider_PercentChange(wxCommandEvent& WXUNUSED(evt))
{
    if (m_bLoopDetect) {
        return;
    }

    m_PercentChange = (double)(m_pSlider_PercentChange->GetValue());
    // Warp positive values to actually go up faster & further than negatives.
    if (m_PercentChange > 0.0) {
        m_PercentChange = pow(m_PercentChange, kSliderWarp);
    }

    m_bLoopDetect = true;
    Update_Text_PercentChange();
    Update_Text_ToBPM();
    Update_Text_ToLength();
    m_bLoopDetect = false;
}

void EffectChangeTempo::OnText_FromBPM(wxCommandEvent& WXUNUSED(evt))
{
    if (m_bLoopDetect) {
        return;
    }

    m_pTextCtrl_FromBPM->GetValidator()->TransferFromWindow();

    m_bLoopDetect = true;

    Update_Text_ToBPM();

    m_bLoopDetect = false;
}

void EffectChangeTempo::OnText_ToBPM(wxCommandEvent& WXUNUSED(evt))
{
    if (m_bLoopDetect) {
        return;
    }

    m_pTextCtrl_ToBPM->GetValidator()->TransferFromWindow();

    m_bLoopDetect = true;

    // If FromBPM has already been set, then there's a NEW percent change.
    if (m_FromBPM != 0.0 && m_ToBPM != 0.0) {
        m_PercentChange = ((m_ToBPM * 100.0) / m_FromBPM) - 100.0;

        Update_Text_PercentChange();
        Update_Slider_PercentChange();

        Update_Text_ToLength();
    }

    m_bLoopDetect = false;
}

void EffectChangeTempo::OnText_ToLength(wxCommandEvent& WXUNUSED(evt))
{
    if (m_bLoopDetect) {
        return;
    }

    m_pTextCtrl_ToLength->GetValidator()->TransferFromWindow();

    if (m_ToLength != 0.0) {
        m_PercentChange = ((m_FromLength * 100.0) / m_ToLength) - 100.0;
    }

    m_bLoopDetect = true;

    Update_Text_PercentChange();
    Update_Slider_PercentChange();

    Update_Text_ToBPM();

    m_bLoopDetect = false;
}

// helper fns

void EffectChangeTempo::Update_Text_PercentChange()
{
    m_pTextCtrl_PercentChange->GetValidator()->TransferToWindow();
}

void EffectChangeTempo::Update_Slider_PercentChange()
{
    double unwarped = m_PercentChange;
    if (unwarped > 0.0) {
        // Un-warp values above zero to actually go up to kSliderMax.
        unwarped = pow(m_PercentChange, (1.0 / kSliderWarp));
    }

    // Add 0.5 to unwarped so trunc -> round.
    m_pSlider_PercentChange->SetValue((int)(unwarped + 0.5));
}

void EffectChangeTempo::Update_Text_ToBPM()
// Use m_FromBPM & m_PercentChange to set NEW m_ToBPM & control.
{
    m_ToBPM = (((m_FromBPM * (100.0 + m_PercentChange)) / 100.0));
    m_pTextCtrl_ToBPM->GetValidator()->TransferToWindow();
}

void EffectChangeTempo::Update_Text_ToLength()
// Use m_FromLength & m_PercentChange to set NEW m_ToLength & control.
{
    m_ToLength = (m_FromLength * 100.0) / (100.0 + m_PercentChange);
    m_pTextCtrl_ToLength->GetValidator()->TransferToWindow();
}

#endif // USE_SOUNDTOUCH
