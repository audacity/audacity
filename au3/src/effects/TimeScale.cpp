/**********************************************************************

  Audacity: A Digital Audio Editor

  TimeScale.cpp

  Clayton Otey

**********************************************************************/
#if USE_SBSMS
#include "TimeScale.h"
#include "EffectEditor.h"
#include "LoadEffects.h"

#include <wx/slider.h>

#include "ShuttleGui.h"
#include "../widgets/valnum.h"

enum
{
    ID_RatePercentChangeStart = 10000,
    ID_RatePercentChangeEnd,
    ID_PitchHalfStepsStart,
    ID_PitchHalfStepsEnd,
    ID_PitchPercentChangeStart,
    ID_PitchPercentChangeEnd
};

namespace {
BuiltinEffectsModule::Registration< EffectTimeScale > reg;
}

BEGIN_EVENT_TABLE(EffectTimeScale, wxEvtHandler)
EVT_TEXT(ID_RatePercentChangeStart, EffectTimeScale::OnText_RatePercentChangeStart)
EVT_TEXT(ID_RatePercentChangeEnd, EffectTimeScale::OnText_RatePercentChangeEnd)
EVT_TEXT(ID_PitchHalfStepsStart, EffectTimeScale::OnText_PitchHalfStepsStart)
EVT_TEXT(ID_PitchHalfStepsEnd, EffectTimeScale::OnText_PitchHalfStepsEnd)
EVT_TEXT(ID_PitchPercentChangeStart, EffectTimeScale::OnText_PitchPercentChangeStart)
EVT_TEXT(ID_PitchPercentChangeEnd, EffectTimeScale::OnText_PitchPercentChangeEnd)
EVT_SLIDER(ID_RatePercentChangeStart, EffectTimeScale::OnSlider_RatePercentChangeStart)
EVT_SLIDER(ID_RatePercentChangeEnd, EffectTimeScale::OnSlider_RatePercentChangeEnd)
END_EVENT_TABLE()

std::unique_ptr<EffectEditor> EffectTimeScale::PopulateOrExchange(
    ShuttleGui& S, EffectInstance&, EffectSettingsAccess&,
    const EffectOutputs*)
{
    mUIParent = S.GetParent();
    S.SetBorder(5);
    S.AddSpace(0, 5);

    S.StartMultiColumn(2, wxALIGN_CENTER);
    {
        // Rate Start
        S.StartStatic(XO("Initial Tempo Change (%)"));
        {
            S.StartMultiColumn(1, wxCENTER);
            {
                m_pTextCtrl_RatePercentChangeStart = S.Id(ID_RatePercentChangeStart)
                                                     .Validator<FloatingPointValidator<double> >(
                    3, &m_RatePercentChangeStart,
                    NumValidatorStyle::NO_TRAILING_ZEROES,
                    RatePercentStart.min, RatePercentStart.max)
                                                     .AddTextBox({}, L"", 12);
            }
            S.EndMultiColumn();
            S.StartHorizontalLay(wxEXPAND, 0);
            {
                m_pSlider_RatePercentChangeStart = S.Id(ID_RatePercentChangeStart)
                                                   .Style(wxSL_HORIZONTAL)
                                                   .AddSlider({}, RatePercentStart.def, RatePercentStart.max, RatePercentStart.min);
            }
            S.EndHorizontalLay();
        }
        S.EndStatic();

        S.StartStatic(XO("Final Tempo Change (%)"));
        {
            S.StartMultiColumn(1, wxCENTER);
            {
                m_pTextCtrl_RatePercentChangeEnd = S.Id(ID_RatePercentChangeEnd)
                                                   .Validator<FloatingPointValidator<double> >(
                    3, &m_RatePercentChangeEnd,
                    NumValidatorStyle::NO_TRAILING_ZEROES,
                    RatePercentEnd.min, RatePercentEnd.max)
                                                   .AddTextBox({}, L"", 12);
            }
            S.EndMultiColumn();
            S.StartHorizontalLay(wxEXPAND, 0);
            {
                m_pSlider_RatePercentChangeEnd = S.Id(ID_RatePercentChangeEnd)
                                                 .Style(wxSL_HORIZONTAL)
                                                 .AddSlider({}, RatePercentEnd.def, RatePercentEnd.max, RatePercentEnd.min);
            }
            S.EndHorizontalLay();
        }
        S.EndStatic();

        // Pitch Start
        S.StartStatic(XO("Initial Pitch Shift"));
        {
            S.StartMultiColumn(2, wxCENTER);
            {
                m_pTextCtrl_PitchHalfStepsStart = S.Id(ID_PitchHalfStepsStart)
                                                  .Validator<FloatingPointValidator<double> >(
                    3, &m_PitchHalfStepsStart,
                    NumValidatorStyle::NO_TRAILING_ZEROES,
                    HalfStepsStart.min, HalfStepsStart.max)
                                                  .AddTextBox(XXO("(&semitones) [-12 to 12]:"), L"", 12);

                m_pTextCtrl_PitchPercentChangeStart = S.Id(ID_PitchPercentChangeStart)
                                                      .Validator<FloatingPointValidator<double> >(
                    3, &m_PitchPercentChangeStart,
                    NumValidatorStyle::NO_TRAILING_ZEROES,
                    PitchPercentStart.min, PitchPercentStart.max)
                                                      .AddTextBox(XXO("(%) [-50 to 100]:"), L"", 12);
            }
            S.EndMultiColumn();
        }
        S.EndStatic();

        // Pitch End
        S.StartStatic(XO("Final Pitch Shift"));
        {
            S.StartMultiColumn(2, wxCENTER);
            {
                m_pTextCtrl_PitchHalfStepsEnd = S.Id(ID_PitchHalfStepsEnd)
                                                .Validator<FloatingPointValidator<double> >(
                    3, &m_PitchHalfStepsEnd,
                    NumValidatorStyle::NO_TRAILING_ZEROES,
                    HalfStepsEnd.min, HalfStepsEnd.max)
                                                .AddTextBox(XXO("(s&emitones) [-12 to 12]:"), L"", 12);

                m_pTextCtrl_PitchPercentChangeEnd = S.Id(ID_PitchPercentChangeEnd)
                                                    .Validator<FloatingPointValidator<double> >(
                    3, &m_PitchPercentChangeEnd,
                    NumValidatorStyle::NO_TRAILING_ZEROES,
                    PitchPercentStart.min, PitchPercentStart.max)
                                                    .AddTextBox(XXO("(%) [-50 to 100]:"), L"", 12);
            }
            S.EndMultiColumn();
        }
        S.EndStatic();
    }
    S.EndMultiColumn();

    return nullptr;
}

bool EffectTimeScale::TransferDataToWindow(const EffectSettings&)
{
    if (!mUIParent->TransferDataToWindow()) {
        return false;
    }

    Update_Slider_RatePercentChangeStart();
    Update_Slider_RatePercentChangeEnd();

    return true;
}

bool EffectTimeScale::TransferDataFromWindow(EffectSettings&)
{
    if (!mUIParent->Validate() || !mUIParent->TransferDataFromWindow()) {
        return false;
    }

    return true;
}

void EffectTimeScale::Update_Text_RatePercentChangeStart()
{
    m_pTextCtrl_RatePercentChangeStart->GetValidator()->TransferToWindow();
}

void EffectTimeScale::Update_Text_RatePercentChangeEnd()
{
    m_pTextCtrl_RatePercentChangeEnd->GetValidator()->TransferToWindow();
}

void EffectTimeScale::Update_Slider_RatePercentChangeStart()
{
    m_pSlider_RatePercentChangeStart->SetValue((int)(m_RatePercentChangeStart + 0.5));
}

void EffectTimeScale::Update_Slider_RatePercentChangeEnd()
{
    m_pSlider_RatePercentChangeEnd->SetValue((int)(m_RatePercentChangeEnd + 0.5));
}

void EffectTimeScale::Update_Text_PitchHalfStepsStart()
{
    m_pTextCtrl_PitchHalfStepsStart->GetValidator()->TransferToWindow();
}

void EffectTimeScale::Update_Text_PitchHalfStepsEnd()
{
    m_pTextCtrl_PitchHalfStepsEnd->GetValidator()->TransferToWindow();
}

void EffectTimeScale::Update_Text_PitchPercentChangeStart()
{
    m_pTextCtrl_PitchPercentChangeStart->GetValidator()->TransferToWindow();
}

void EffectTimeScale::Update_Text_PitchPercentChangeEnd()
{
    m_pTextCtrl_PitchPercentChangeEnd->GetValidator()->TransferToWindow();
}

void EffectTimeScale::OnText_RatePercentChangeStart(wxCommandEvent& WXUNUSED(evt))
{
    if (!EffectEditor::EnableApply(
            mUIParent, mUIParent->TransferDataFromWindow())) {
        return;
    }

    Update_Slider_RatePercentChangeStart();
}

void EffectTimeScale::OnText_RatePercentChangeEnd(wxCommandEvent& WXUNUSED(evt))
{
    if (!EffectEditor::EnableApply(
            mUIParent, mUIParent->TransferDataFromWindow())) {
        return;
    }

    Update_Slider_RatePercentChangeEnd();
}

void EffectTimeScale::OnSlider_RatePercentChangeStart(wxCommandEvent& evt)
{
    m_RatePercentChangeStart = (double)evt.GetInt();

    Update_Text_RatePercentChangeStart();
}

void EffectTimeScale::OnSlider_RatePercentChangeEnd(wxCommandEvent& evt)
{
    m_RatePercentChangeEnd = (double)evt.GetInt();

    Update_Text_RatePercentChangeEnd();
}

void EffectTimeScale::OnText_PitchHalfStepsStart(wxCommandEvent& WXUNUSED(evt))
{
    if (!EffectEditor::EnableApply(
            mUIParent, mUIParent->TransferDataFromWindow())) {
        return;
    }

    m_PitchPercentChangeStart = HalfStepsToPercentChange(m_PitchHalfStepsStart);
    Update_Text_PitchPercentChangeStart();
}

void EffectTimeScale::OnText_PitchHalfStepsEnd(wxCommandEvent& WXUNUSED(evt))
{
    if (!EffectEditor::EnableApply(
            mUIParent, mUIParent->TransferDataFromWindow())) {
        return;
    }

    m_PitchPercentChangeEnd = HalfStepsToPercentChange(m_PitchHalfStepsEnd);
    Update_Text_PitchPercentChangeEnd();
}

void EffectTimeScale::OnText_PitchPercentChangeStart(wxCommandEvent& WXUNUSED(evt))
{
    if (!EffectEditor::EnableApply(
            mUIParent, mUIParent->TransferDataFromWindow())) {
        return;
    }

    m_PitchHalfStepsStart = PercentChangeToHalfSteps(m_PitchPercentChangeStart);
    Update_Text_PitchHalfStepsStart();
}

void EffectTimeScale::OnText_PitchPercentChangeEnd(wxCommandEvent& WXUNUSED(evt))
{
    if (!EffectEditor::EnableApply(
            mUIParent, mUIParent->TransferDataFromWindow())) {
        return;
    }

    m_PitchHalfStepsEnd = PercentChangeToHalfSteps(m_PitchPercentChangeEnd);
    Update_Text_PitchHalfStepsEnd();
}

#endif
