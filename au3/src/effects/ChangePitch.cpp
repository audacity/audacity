/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2013 Audacity Team.
   License: GPL v2 or later.  See License.txt.

  ChangePitch.cpp
  Vaughan Johnson, Dominic Mazzoni, Steve Daulton

******************************************************************//**

\file ChangePitch.cpp
\brief Change Pitch effect provides raising or lowering
the pitch without changing the tempo.

*//*******************************************************************/

#if USE_SOUNDTOUCH
#include "ChangePitch.h"
#include "EffectEditor.h"
#include "LoadEffects.h"
#include "PitchName.h"

#if USE_SBSMS
#include <wx/valgen.h>
#endif

#include <wx/checkbox.h>
#include <wx/choice.h>
#include <wx/slider.h>
#include <wx/spinctrl.h>
#include <wx/valtext.h>

#include "ShuttleGui.h"
#include "../widgets/valnum.h"

enum {
    ID_PercentChange = 10000,
    ID_FromPitch,
    ID_FromOctave,
    ID_ToPitch,
    ID_ToOctave,
    ID_SemitonesChange,
    ID_FromFrequency,
    ID_ToFrequency
};

// We warp the slider to go up to 400%, but user can enter up to 3000%
static const double kSliderMax
    =100.0; // warped above zero to actually go up to 400%
static const double kSliderWarp
    =1.30105; // warp power takes max from 100 to 400.

namespace {
BuiltinEffectsModule::Registration< EffectChangePitch > reg;
}

BEGIN_EVENT_TABLE(EffectChangePitch, wxEvtHandler)
EVT_CHOICE(ID_FromPitch, EffectChangePitch::OnChoice_FromPitch)
EVT_TEXT(ID_FromOctave, EffectChangePitch::OnSpin_FromOctave)
EVT_CHOICE(ID_ToPitch, EffectChangePitch::OnChoice_ToPitch)
EVT_TEXT(ID_ToOctave, EffectChangePitch::OnSpin_ToOctave)

EVT_TEXT(ID_SemitonesChange, EffectChangePitch::OnText_SemitonesChange)

EVT_TEXT(ID_FromFrequency, EffectChangePitch::OnText_FromFrequency)
EVT_TEXT(ID_ToFrequency, EffectChangePitch::OnText_ToFrequency)

EVT_TEXT(ID_PercentChange, EffectChangePitch::OnText_PercentChange)
EVT_SLIDER(ID_PercentChange, EffectChangePitch::OnSlider_PercentChange)
END_EVENT_TABLE()

std::unique_ptr<EffectEditor> EffectChangePitch::PopulateOrExchange(
    ShuttleGui& S, EffectInstance&, EffectSettingsAccess&,
    const EffectOutputs*)
{
    mUIParent = S.GetParent();
    DeduceFrequencies(); // Set frequency-related control values based on sample.

    TranslatableStrings pitch;
    for (int ii = 0; ii < 12; ++ii) {
        pitch.push_back(PitchName(ii, PitchNameChoice::Both));
    }

    S.SetBorder(5);

    S.StartVerticalLay(0);
    {
        S.StartVerticalLay();
        {
            S.AddTitle(
                XO("Estimated Start Pitch: %s%d (%.3f Hz)")
                .Format(pitch[m_nFromPitch], m_nFromOctave, m_FromFrequency));
        }
        S.EndVerticalLay();

        /* i18n-hint: (noun) Musical pitch.*/
        S.StartStatic(XO("Pitch"));
        {
            S.StartMultiColumn(6, wxALIGN_CENTER); // 6 controls, because each AddChoice adds a wxStaticText and a wxChoice.
            {
                m_pChoice_FromPitch = S.Id(ID_FromPitch)
                                      /* i18n-hint: changing musical pitch "from" one value "to" another */
                                      .Name(XC("from", "change pitch"))
                                      .MinSize({ 80, -1 })
                                      /* i18n-hint: changing musical pitch "from" one value "to" another */
                                      .AddChoice(XXC("&from", "change pitch"), pitch);

                m_pSpin_FromOctave = S.Id(ID_FromOctave)
                                     .Name(XO("from Octave"))
                                     .MinSize({ 50, -1 })
                                     .AddSpinCtrl({}, m_nFromOctave, INT_MAX, INT_MIN);

                m_pChoice_ToPitch = S.Id(ID_ToPitch)
                                    /* i18n-hint: changing musical pitch "from" one value "to" another */
                                    .Name(XC("to", "change pitch"))
                                    .MinSize({ 80, -1 })
                                    /* i18n-hint: changing musical pitch "from" one value "to" another */
                                    .AddChoice(XXC("&to", "change pitch"), pitch);

                m_pSpin_ToOctave = S.Id(ID_ToOctave)
                                   .Name(XO("to Octave"))
                                   .MinSize({ 50, -1 })
                                   .AddSpinCtrl({}, m_nToOctave, INT_MAX, INT_MIN);
            }
            S.EndMultiColumn();

            S.StartHorizontalLay(wxALIGN_CENTER);
            {
                m_pTextCtrl_SemitonesChange = S.Id(ID_SemitonesChange)
                                              .Name(XO("Semitones (half-steps)"))
                                              .Validator<FloatingPointValidator<double> >(
                    2, &m_dSemitonesChange,
                    NumValidatorStyle::TWO_TRAILING_ZEROES
                    )
                                              .AddTextBox(XXO("&Semitones (half-steps):"), wxT(""), 12);
            }
            S.EndHorizontalLay();
        }
        S.EndStatic();

        S.StartStatic(XO("Frequency"));
        {
            S.StartMultiColumn(5, wxALIGN_CENTER); // 5, because AddTextBox adds a wxStaticText and a wxTextCtrl.
            {
                m_pTextCtrl_FromFrequency = S.Id(ID_FromFrequency)
                                            .Name(XO("from (Hz)"))
                                            .Validator<FloatingPointValidator<double> >(
                    3, &m_FromFrequency,
                    NumValidatorStyle::THREE_TRAILING_ZEROES,
                    0.0
                    )
                                            .AddTextBox(XXO("f&rom"), wxT(""), 12);

                m_pTextCtrl_ToFrequency = S.Id(ID_ToFrequency)
                                          .Name(XO("to (Hz)"))
                                          .Validator<FloatingPointValidator<double> >(
                    3, &m_ToFrequency,
                    NumValidatorStyle::THREE_TRAILING_ZEROES,
                    0.0
                    )
                                          .AddTextBox(XXO("t&o"), wxT(""), 12);

                S.AddUnits(XO("Hz"));
            }
            S.EndMultiColumn();

            S.StartHorizontalLay(wxALIGN_CENTER);
            {
                m_pTextCtrl_PercentChange = S.Id(ID_PercentChange)
                                            .Validator<FloatingPointValidator<double> >(
                    3, &m_dPercentChange,
                    NumValidatorStyle::THREE_TRAILING_ZEROES,
                    Percentage.min, Percentage.max)
                                            .AddTextBox(XXO("Percent C&hange:"), L"", 12);
            }
            S.EndHorizontalLay();

            S.StartHorizontalLay(wxEXPAND);
            {
                m_pSlider_PercentChange = S.Id(ID_PercentChange)
                                          .Name(XO("Percent Change"))
                                          .Style(wxSL_HORIZONTAL)
                                          .AddSlider({}, 0, (int)kSliderMax, (int)Percentage.min);
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

bool EffectChangePitch::TransferDataToWindow(const EffectSettings&)
{
    m_bLoopDetect = true;

    if (!mUIParent->TransferDataToWindow()) {
        return false;
    }

    Calc_SemitonesChange_fromPercentChange();
    Calc_ToPitch(); // Call *after* m_dSemitonesChange is updated.
    Calc_ToFrequency();
    Calc_ToOctave(); // Call after Calc_ToFrequency().

    Update_Choice_FromPitch();
    Update_Choice_ToPitch();
    Update_Spin_FromOctave();
    Update_Spin_ToOctave();
    Update_Text_SemitonesChange();
    Update_Text_FromFrequency();
    Update_Text_ToFrequency();
    Update_Text_PercentChange();
    Update_Slider_PercentChange();

    m_bLoopDetect = false;

    return true;
}

bool EffectChangePitch::TransferDataFromWindow(EffectSettings&)
{
    if (!mUIParent->Validate() || !mUIParent->TransferDataFromWindow()) {
        return false;
    }

    // from/to pitch controls
    m_nFromPitch = m_pChoice_FromPitch->GetSelection();
    m_nFromOctave = m_pSpin_FromOctave->GetValue();

    m_nToPitch = m_pChoice_ToPitch->GetSelection();

    // No need to update Slider_PercentChange here because TextCtrl_PercentChange
    // always tracks it & is more precise (decimal points).

    return true;
}

// handlers
void EffectChangePitch::OnChoice_FromPitch(wxCommandEvent& WXUNUSED(evt))
{
    if (m_bLoopDetect) {
        return;
    }

    m_nFromPitch = m_pChoice_FromPitch->GetSelection();
    m_FromFrequency = PitchToFreq(m_nFromPitch, m_nFromOctave);

    Calc_ToPitch();
    Calc_ToFrequency();
    Calc_ToOctave(); // Call after Calc_ToFrequency().

    m_bLoopDetect = true;
    {
        Update_Choice_ToPitch();
        Update_Spin_ToOctave();
        Update_Text_FromFrequency();
        Update_Text_ToFrequency();
    }
    m_bLoopDetect = false;
}

void EffectChangePitch::OnSpin_FromOctave(wxCommandEvent& WXUNUSED(evt))
{
    if (m_bLoopDetect) {
        return;
    }

    m_nFromOctave = m_pSpin_FromOctave->GetValue();
    //vvv If I change this code to not keep semitones and percent constant,
    // will need validation code as in OnSpin_ToOctave.
    m_FromFrequency = PitchToFreq(m_nFromPitch, m_nFromOctave);

    Calc_ToFrequency();
    Calc_ToOctave(); // Call after Calc_ToFrequency().

    m_bLoopDetect = true;
    {
        Update_Spin_ToOctave();
        Update_Text_FromFrequency();
        Update_Text_ToFrequency();
    }
    m_bLoopDetect = false;
}

void EffectChangePitch::OnChoice_ToPitch(wxCommandEvent& WXUNUSED(evt))
{
    if (m_bLoopDetect) {
        return;
    }

    m_nToPitch = m_pChoice_ToPitch->GetSelection();

    Calc_SemitonesChange_fromPitches();
    Calc_PercentChange(); // Call *after* m_dSemitonesChange is updated.
    Calc_ToFrequency(); // Call *after* m_dPercentChange is updated.

    m_bLoopDetect = true;
    {
        Update_Text_SemitonesChange();
        Update_Text_ToFrequency();
        Update_Text_PercentChange();
        Update_Slider_PercentChange();
    }
    m_bLoopDetect = false;
}

void EffectChangePitch::OnSpin_ToOctave(wxCommandEvent& WXUNUSED(evt))
{
    if (m_bLoopDetect) {
        return;
    }

    int nNewValue = m_pSpin_ToOctave->GetValue();
    // Validation: Rather than set a range for octave numbers, enforce a range that
    // keeps m_dPercentChange above -99%, per Soundtouch constraints.
    if ((nNewValue + 3) < m_nFromOctave) {
        ::wxBell();
        m_pSpin_ToOctave->SetValue(m_nFromOctave - 3);
        return;
    }
    m_nToOctave = nNewValue;

    m_ToFrequency = PitchToFreq(m_nToPitch, m_nToOctave);

    Calc_SemitonesChange_fromPitches();
    Calc_PercentChange(); // Call *after* m_dSemitonesChange is updated.

    m_bLoopDetect = true;
    {
        Update_Text_SemitonesChange();
        Update_Text_ToFrequency();
        Update_Text_PercentChange();
        Update_Slider_PercentChange();
    }
    m_bLoopDetect = false;
}

void EffectChangePitch::OnText_SemitonesChange(wxCommandEvent& WXUNUSED(evt))
{
    if (m_bLoopDetect) {
        return;
    }

    if (!m_pTextCtrl_SemitonesChange->GetValidator()->TransferFromWindow()) {
        EffectEditor::EnableApply(mUIParent, false);
        return;
    }

    Calc_PercentChange();
    Calc_ToFrequency(); // Call *after* m_dPercentChange is updated.
    Calc_ToPitch();
    Calc_ToOctave(); // Call after Calc_ToFrequency().

    m_bLoopDetect = true;
    {
        Update_Choice_ToPitch();
        Update_Spin_ToOctave();
        Update_Text_ToFrequency();
        Update_Text_PercentChange();
        Update_Slider_PercentChange();
    }
    m_bLoopDetect = false;

    // If m_dSemitonesChange is a big enough negative, we can go to or below 0 freq.
    // If m_dSemitonesChange is a big enough positive, we can go to 1.#INF (Windows) or inf (Linux).
    // But practically, these are best limits for Soundtouch.
    bool bIsGoodValue = (m_dSemitonesChange > -80.0) && (m_dSemitonesChange <= 60.0);
    EffectEditor::EnableApply(mUIParent, bIsGoodValue);
}

void EffectChangePitch::OnText_FromFrequency(wxCommandEvent& WXUNUSED(evt))
{
    if (m_bLoopDetect) {
        return;
    }

    // Empty string causes unpredictable results with ToDouble() and later calculations.
    // Non-positive frequency makes no sense, but user might still be editing,
    // so it's not an error, but we do not want to update the values/controls.
    if (!m_pTextCtrl_FromFrequency->GetValidator()->TransferFromWindow()) {
        EffectEditor::EnableApply(mUIParent, false);
        return;
    }

    double newFromMIDInote = FreqToMIDInote(m_FromFrequency);
    m_nFromPitch = PitchIndex(newFromMIDInote);
    m_nFromOctave = PitchOctave(newFromMIDInote);
    Calc_ToPitch();
    Calc_ToFrequency();
    Calc_ToOctave(); // Call after Calc_ToFrequency().

    m_bLoopDetect = true;
    {
        Update_Choice_FromPitch();
        Update_Spin_FromOctave();
        Update_Choice_ToPitch();
        Update_Spin_ToOctave();
        Update_Text_ToFrequency();
    }
    m_bLoopDetect = false;

    // Success. Make sure OK and Preview are enabled, in case we disabled above during editing.
    EffectEditor::EnableApply(mUIParent, true);
}

void EffectChangePitch::OnText_ToFrequency(wxCommandEvent& WXUNUSED(evt))
{
    if (m_bLoopDetect) {
        return;
    }

    // Empty string causes unpredictable results with ToDouble() and later calculations.
    // Non-positive frequency makes no sense, but user might still be editing,
    // so it's not an error, but we do not want to update the values/controls.
    if (!m_pTextCtrl_ToFrequency->GetValidator()->TransferFromWindow()) {
        EffectEditor::EnableApply(mUIParent, false);
        return;
    }

    m_dPercentChange = ((m_ToFrequency * 100.0) / m_FromFrequency) - 100.0;

    Calc_ToOctave(); // Call after Calc_ToFrequency().
    Calc_SemitonesChange_fromPercentChange();
    Calc_ToPitch(); // Call *after* m_dSemitonesChange is updated.

    m_bLoopDetect = true;
    {
        Update_Choice_ToPitch();
        Update_Spin_ToOctave();
        Update_Text_SemitonesChange();
        Update_Text_PercentChange();
        Update_Slider_PercentChange();
    }
    m_bLoopDetect = false;

    // Success. Make sure OK and Preview are disabled if percent change is out of bounds.
    // Can happen while editing.
    // If the value is good, might also need to re-enable because of above clause.
    bool bIsGoodValue = (m_dPercentChange > Percentage.min) && (m_dPercentChange <= Percentage.max);
    EffectEditor::EnableApply(mUIParent, bIsGoodValue);
}

void EffectChangePitch::OnText_PercentChange(wxCommandEvent& WXUNUSED(evt))
{
    if (m_bLoopDetect) {
        return;
    }

    if (!m_pTextCtrl_PercentChange->GetValidator()->TransferFromWindow()) {
        EffectEditor::EnableApply(mUIParent, false);
        return;
    }

    Calc_SemitonesChange_fromPercentChange();
    Calc_ToPitch(); // Call *after* m_dSemitonesChange is updated.
    Calc_ToFrequency();
    Calc_ToOctave(); // Call after Calc_ToFrequency().

    m_bLoopDetect = true;
    {
        Update_Choice_ToPitch();
        Update_Spin_ToOctave();
        Update_Text_SemitonesChange();
        Update_Text_ToFrequency();
        Update_Slider_PercentChange();
    }
    m_bLoopDetect = false;

    // Success. Make sure OK and Preview are enabled, in case we disabled above during editing.
    EffectEditor::EnableApply(mUIParent, true);
}

void EffectChangePitch::OnSlider_PercentChange(wxCommandEvent& WXUNUSED(evt))
{
    if (m_bLoopDetect) {
        return;
    }

    m_dPercentChange = (double)(m_pSlider_PercentChange->GetValue());
    // Warp positive values to actually go up faster & further than negatives.
    if (m_dPercentChange > 0.0) {
        m_dPercentChange = pow(m_dPercentChange, kSliderWarp);
    }

    Calc_SemitonesChange_fromPercentChange();
    Calc_ToPitch(); // Call *after* m_dSemitonesChange is updated.
    Calc_ToFrequency();
    Calc_ToOctave(); // Call after Calc_ToFrequency().

    m_bLoopDetect = true;
    {
        Update_Choice_ToPitch();
        Update_Spin_ToOctave();
        Update_Text_SemitonesChange();
        Update_Text_ToFrequency();
        Update_Text_PercentChange();
    }
    m_bLoopDetect = false;
}

// helper fns for controls

void EffectChangePitch::Update_Choice_FromPitch()
{
    m_pChoice_FromPitch->SetSelection(m_nFromPitch);
}

void EffectChangePitch::Update_Spin_FromOctave()
{
    m_pSpin_FromOctave->SetValue(m_nFromOctave);
}

void EffectChangePitch::Update_Choice_ToPitch()
{
    m_pChoice_ToPitch->SetSelection(m_nToPitch);
}

void EffectChangePitch::Update_Spin_ToOctave()
{
    m_pSpin_ToOctave->SetValue(m_nToOctave);
}

void EffectChangePitch::Update_Text_SemitonesChange()
{
    m_pTextCtrl_SemitonesChange->GetValidator()->TransferToWindow();
}

void EffectChangePitch::Update_Text_FromFrequency()
{
    m_pTextCtrl_FromFrequency->GetValidator()->TransferToWindow();
}

void EffectChangePitch::Update_Text_ToFrequency()
{
    m_pTextCtrl_ToFrequency->GetValidator()->TransferToWindow();
}

void EffectChangePitch::Update_Text_PercentChange()
{
    m_pTextCtrl_PercentChange->GetValidator()->TransferToWindow();
}

void EffectChangePitch::Update_Slider_PercentChange()
{
    double unwarped = m_dPercentChange;
    if (unwarped > 0.0) {
        // Un-warp values above zero to actually go up to kSliderMax.
        unwarped = pow(m_dPercentChange, (1.0 / kSliderWarp));
    }

    // Add 0.5 to unwarped so trunc -> round.
    m_pSlider_PercentChange->SetValue((int)(unwarped + 0.5));
}

#endif // USE_SOUNDTOUCH
