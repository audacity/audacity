/**********************************************************************

  Audacity: A Digital Audio Editor

  Distortion.cpp

  Steve Daulton

*//*******************************************************************/

#include "Distortion.h"
#include "EffectEditor.h"
#include "LoadEffects.h"

#include <cmath>
#include <algorithm>
//#define _USE_MATH_DEFINES

// Belt and braces
#ifndef M_PI
#define M_PI 3.1415926535897932384626433832795
#endif
#ifndef M_PI_2
#define M_PI_2 1.57079632679489661923132169163975
#endif

#include <wx/checkbox.h>
#include <wx/choice.h>
#include <wx/valgen.h>
#include <wx/log.h>
#include <wx/slider.h>
#include <wx/stattext.h>
#include <wx/weakref.h>

#include "Prefs.h"
#include "ShuttleGui.h"
#include "../widgets/valnum.h"

TranslatableString defaultLabel(int index)
{
    static const TranslatableString names[] = {
        XO("Upper Threshold"),
        XO("Noise Floor"),
        XO("Parameter 1"),
        XO("Parameter 2"),
        XO("Number of repeats"),
    };

    return names[ index ];
}

//
// DistortionBase
//

namespace {
BuiltinEffectsModule::Registration< EffectDistortion > reg;
}

struct EffectDistortion::Editor : EffectEditor
{
    Editor(const EffectUIServices& services,
           DistortionBase::Instance& instance,
           EffectSettingsAccess& access, const EffectDistortionSettings& settings)
        : EffectEditor{services, access}
        , mInstance(instance)
        , mSettings{settings}
    {}
    virtual ~Editor() = default;

    bool ValidateUI() override;
    bool UpdateUI() override;

    void PopulateOrExchange(ShuttleGui& S);

    // Control Handlers
    void OnTypeChoice(wxCommandEvent& evt);
    void OnDCBlockCheckbox(wxCommandEvent& evt);
    void OnThresholdText(wxCommandEvent& evt);
    void OnThresholdSlider(wxCommandEvent& evt);
    void OnNoiseFloorText(wxCommandEvent& evt);
    void OnNoiseFloorSlider(wxCommandEvent& evt);
    void OnParam1Text(wxCommandEvent& evt);
    void OnParam1Slider(wxCommandEvent& evt);
    void OnParam2Text(wxCommandEvent& evt);
    void OnParam2Slider(wxCommandEvent& evt);
    void OnRepeatsText(wxCommandEvent& evt);
    void OnRepeatsSlider(wxCommandEvent& evt);

    wxChoice* mTypeChoiceCtrl;
    wxTextCtrl* mThresholdT;
    wxTextCtrl* mNoiseFloorT;
    wxTextCtrl* mParam1T;
    wxTextCtrl* mParam2T;
    wxTextCtrl* mRepeatsT;

    wxSlider* mThresholdS;
    wxSlider* mNoiseFloorS;
    wxSlider* mParam1S;
    wxSlider* mParam2S;
    wxSlider* mRepeatsS;

    wxCheckBox* mDCBlockCheckBox;

    wxStaticText* mThresholdTxt;
    wxStaticText* mNoiseFloorTxt;
    wxStaticText* mParam1Txt;
    wxStaticText* mParam2Txt;
    wxStaticText* mRepeatsTxt;

    wxString mOldThresholdTxt;
    wxString mOldmNoiseFloorTxt;
    wxString mOldParam1Txt;
    wxString mOldParam2Txt;
    wxString mOldRepeatsTxt;

    EffectDistortionSettings mSettings;

    EffectDistortionState& GetState();

    void UpdateControl(control id, bool enable, TranslatableString name);
    void UpdateUIControls();
    void UpdateControlText(wxTextCtrl* textCtrl, wxString& string, bool enabled);

    wxWeakRef<wxWindow> mUIParent{};
    DistortionBase::Instance& mInstance;
};

bool EffectDistortion::Editor::ValidateUI()
{
    {
        // This section was copied from the original
        // DistortionBase::TransferDataFromWindow
        //
        // However, the call to ->Validate would bring up an error dialog
        // saying "Empty value"

        if (/*!mUIParent()->Validate() ||*/ !mUIParent->TransferDataFromWindow()) {
            return false;
        }
    }

    mAccess.ModifySettings
    (
        [this](EffectSettings& settings)
    {
        // pass back the modified settings to the MessageBuffer

        GetSettings(settings) = mSettings;

        return nullptr;
    }
    );

    return true;
}

EffectDistortionState& EffectDistortion::Editor::GetState()
{
    return mInstance.mMaster;
}

// Effect implementation

std::unique_ptr<EffectEditor>
EffectDistortion::MakeEditor(ShuttleGui& S, EffectInstance& instance,
                             EffectSettingsAccess& access, const EffectOutputs* pOutputs) const
{
    auto& settings = access.Get();
    auto& myEffSettings = GetSettings(settings);

    auto result = std::make_unique<Editor>(*this, dynamic_cast<DistortionBase::Instance&>(instance), access, myEffSettings);
    result->PopulateOrExchange(S);
    return result;
}

void EffectDistortion::Editor::PopulateOrExchange(ShuttleGui& S)
{
    mUIParent = S.GetParent();
    auto& ms = mSettings;

    S.AddSpace(0, 5);
    S.StartVerticalLay();
    {
        S.StartMultiColumn(4, wxCENTER);
        {
            mTypeChoiceCtrl = S
                              .MinSize({ -1, -1 })
                              .Validator<wxGenericValidator>(&ms.mTableChoiceIndx)
                              .AddChoice(XXO("Distortion type:"),
                                         Msgids(kTableTypeStrings, nTableTypes));

            BindTo(*mTypeChoiceCtrl, wxEVT_CHOICE, &Editor::OnTypeChoice);

            mDCBlockCheckBox = S.AddCheckBox(XXO("DC blocking filter"),
                                             DCBlock.def);

            BindTo(*mDCBlockCheckBox, wxEVT_CHECKBOX, &Editor::OnDCBlockCheckbox);
        }
        S.EndMultiColumn();
        S.AddSpace(0, 10);

        S.StartStatic(XO("Threshold controls"));
        {
            S.StartMultiColumn(4, wxEXPAND);
            S.SetStretchyCol(2);
            {
                // Allow space for first Column
                S.AddSpace(250, 0);
                S.AddSpace(0, 0);
                S.AddSpace(0, 0);
                S.AddSpace(0, 0);

                // Upper threshold control
                mThresholdTxt = S.AddVariableText(defaultLabel(0),
                                                  false, wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT);
                mThresholdT = S
                              .Name(defaultLabel(0))
                              .Validator<FloatingPointValidator<double> >(
                    2, &ms.mThreshold_dB, NumValidatorStyle::DEFAULT,
                    Threshold_dB.min, Threshold_dB.max)
                              .AddTextBox({}, wxT(""), 10);

                BindTo(*mThresholdT, wxEVT_TEXT, &Editor::OnThresholdText);

                mThresholdS = S
                              .Name(defaultLabel(0))
                              .Style(wxSL_HORIZONTAL)
                              .AddSlider({}, 0,
                                         DB_TO_LINEAR(Threshold_dB.max) * Threshold_dB.scale,
                                         DB_TO_LINEAR(Threshold_dB.min) * Threshold_dB.scale);
                S.AddSpace(20, 0);

                BindTo(*mThresholdS, wxEVT_SLIDER, &Editor::OnThresholdSlider);

                // Noise floor control
                mNoiseFloorTxt = S.AddVariableText(defaultLabel(1),
                                                   false, wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT);
                mNoiseFloorT = S
                               .Name(defaultLabel(1))
                               .Validator<FloatingPointValidator<double> >(
                    2, &ms.mNoiseFloor, NumValidatorStyle::DEFAULT,
                    NoiseFloor.min, NoiseFloor.max
                    )
                               .AddTextBox({}, wxT(""), 10);

                BindTo(*mNoiseFloorT, wxEVT_TEXT, &Editor::OnNoiseFloorText);

                mNoiseFloorS = S
                               .Name(defaultLabel(1))
                               .Style(wxSL_HORIZONTAL)
                               .AddSlider({}, 0, NoiseFloor.max, NoiseFloor.min);
                S.AddSpace(20, 0);

                BindTo(*mNoiseFloorS, wxEVT_SLIDER, &Editor::OnNoiseFloorSlider);
            }
            S.EndMultiColumn();
        }
        S.EndStatic();

        S.StartStatic(XO("Parameter controls"));
        {
            S.StartMultiColumn(4, wxEXPAND);
            S.SetStretchyCol(2);
            {
                // Allow space for first Column
                S.AddSpace(250, 0);
                S.AddSpace(0, 0);
                S.AddSpace(0, 0);
                S.AddSpace(0, 0);

                // Parameter1 control
                mParam1Txt = S.AddVariableText(defaultLabel(2),
                                               false, wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT);
                mParam1T = S
                           .Name(defaultLabel(2))
                           .Validator<FloatingPointValidator<double> >(
                    2, &ms.mParam1, NumValidatorStyle::DEFAULT,
                    Param1.min, Param1.max
                    )
                           .AddTextBox({}, wxT(""), 10);

                BindTo(*mParam1T, wxEVT_TEXT, &Editor::OnParam1Text);

                mParam1S = S
                           .Name(defaultLabel(2))
                           .Style(wxSL_HORIZONTAL)
                           .AddSlider({}, 0, Param1.max, Param1.min);
                S.AddSpace(20, 0);

                BindTo(*mParam1S, wxEVT_SLIDER, &Editor::OnParam1Slider);

                // Parameter2 control
                mParam2Txt = S.AddVariableText(defaultLabel(3),
                                               false, wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT);
                mParam2T = S
                           .Name(defaultLabel(3))
                           .Validator<FloatingPointValidator<double> >(
                    2, &ms.mParam2, NumValidatorStyle::DEFAULT,
                    Param2.min, Param2.max
                    )
                           .AddTextBox({}, wxT(""), 10);

                BindTo(*mParam2T, wxEVT_TEXT, &Editor::OnParam2Text);

                mParam2S = S
                           .Name(defaultLabel(3))
                           .Style(wxSL_HORIZONTAL)
                           .AddSlider({}, 0, Param2.max, Param2.min);

                BindTo(*mParam2S, wxEVT_SLIDER, &Editor::OnParam2Slider);

                S.AddSpace(20, 0);

                // Repeats control
                mRepeatsTxt = S.AddVariableText(defaultLabel(4),
                                                false, wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT);
                mRepeatsT = S
                            .Name(defaultLabel(4))
                            .Validator<IntegerValidator<int> >(
                    &ms.mRepeats, NumValidatorStyle::DEFAULT,
                    Repeats.min, Repeats.max
                    )
                            .AddTextBox({}, wxT(""), 10);

                BindTo(*mRepeatsT, wxEVT_TEXT, &Editor::OnRepeatsText);

                mRepeatsS = S
                            .Name(defaultLabel(4))
                            .Style(wxSL_HORIZONTAL)
                            .AddSlider({}, Repeats.def, Repeats.max, Repeats.min);

                BindTo(*mRepeatsS, wxEVT_SLIDER, &Editor::OnRepeatsSlider);

                S.AddSpace(20, 0);
            }
            S.EndMultiColumn();
        }
        S.EndStatic();
    }
    S.EndVerticalLay();
}

bool EffectDistortion::Editor::UpdateUI()
{
    const auto& ms = mSettings;

    if (!mUIParent->TransferDataToWindow()) {
        return false;
    }

    const double threshold = DB_TO_LINEAR(ms.mThreshold_dB);

    mThresholdS->SetValue((int)(threshold * Threshold_dB.scale + 0.5));
    mDCBlockCheckBox->SetValue(ms.mDCBlock);
    mNoiseFloorS->SetValue((int)ms.mNoiseFloor + 0.5);
    mParam1S->SetValue((int)ms.mParam1 + 0.5);
    mParam2S->SetValue((int)ms.mParam2 + 0.5);
    mRepeatsS->SetValue(ms.mRepeats);

    GetState().mbSavedFilterState = ms.mDCBlock;

    UpdateUIControls();

    return true;
}

void EffectDistortion::Editor::OnTypeChoice(wxCommandEvent& /*evt*/)
{
    mTypeChoiceCtrl->GetValidator()->TransferFromWindow();

    UpdateUIControls();

    ValidateUI();
    Publish(EffectSettingChanged {});
}

void EffectDistortion::Editor::OnDCBlockCheckbox(wxCommandEvent& /*evt*/)
{
    auto& ms = mSettings;

    ms.mDCBlock = mDCBlockCheckBox->GetValue();

    GetState().mbSavedFilterState = ms.mDCBlock;

    ValidateUI();
    Publish(EffectSettingChanged {});
}

void EffectDistortion::Editor::OnThresholdText(wxCommandEvent& /*evt*/)
{
    const auto& ms = mSettings;

    mThresholdT->GetValidator()->TransferFromWindow();
    const double threshold = DB_TO_LINEAR(ms.mThreshold_dB);
    mThresholdS->SetValue((int)(threshold * Threshold_dB.scale + 0.5));

    ValidateUI();
    Publish(EffectSettingChanged {});
}

void EffectDistortion::Editor::OnThresholdSlider(wxCommandEvent& evt)
{
    auto& ms = mSettings;

    static const double MIN_Threshold_Linear = DB_TO_LINEAR(Threshold_dB.min);

    const double thresholdDB = (double)evt.GetInt() / Threshold_dB.scale;
    ms.mThreshold_dB = wxMax(LINEAR_TO_DB(thresholdDB), Threshold_dB.min);

    mThresholdT->GetValidator()->TransferToWindow();

    ValidateUI();
    Publish(EffectSettingChanged {});
}

void EffectDistortion::Editor::OnNoiseFloorText(wxCommandEvent& /*evt*/)
{
    const auto& ms = mSettings;

    mNoiseFloorT->GetValidator()->TransferFromWindow();
    mNoiseFloorS->SetValue((int)floor(ms.mNoiseFloor + 0.5));

    ValidateUI();
    Publish(EffectSettingChanged {});
}

void EffectDistortion::Editor::OnNoiseFloorSlider(wxCommandEvent& evt)
{
    auto& ms = mSettings;

    ms.mNoiseFloor = (double)evt.GetInt();
    mNoiseFloorT->GetValidator()->TransferToWindow();

    ValidateUI();
    Publish(EffectSettingChanged {});
}

void EffectDistortion::Editor::OnParam1Text(wxCommandEvent& /*evt*/)
{
    const auto& ms = mSettings;

    mParam1T->GetValidator()->TransferFromWindow();
    mParam1S->SetValue((int)floor(ms.mParam1 + 0.5));

    ValidateUI();
    Publish(EffectSettingChanged {});
}

void EffectDistortion::Editor::OnParam1Slider(wxCommandEvent& evt)
{
    auto& ms = mSettings;

    ms.mParam1 = (double)evt.GetInt();
    mParam1T->GetValidator()->TransferToWindow();

    ValidateUI();
    Publish(EffectSettingChanged {});
}

void EffectDistortion::Editor::OnParam2Text(wxCommandEvent& /*evt*/)
{
    const auto& ms = mSettings;

    mParam2T->GetValidator()->TransferFromWindow();
    mParam2S->SetValue((int)floor(ms.mParam2 + 0.5));

    ValidateUI();
    Publish(EffectSettingChanged {});
}

void EffectDistortion::Editor::OnParam2Slider(wxCommandEvent& evt)
{
    auto& ms = mSettings;

    ms.mParam2 = (double)evt.GetInt();
    mParam2T->GetValidator()->TransferToWindow();

    ValidateUI();
    Publish(EffectSettingChanged {});
}

void EffectDistortion::Editor::OnRepeatsText(wxCommandEvent& /*evt*/)
{
    const auto& ms = mSettings;

    mRepeatsT->GetValidator()->TransferFromWindow();
    mRepeatsS->SetValue(ms.mRepeats);

    ValidateUI();
    Publish(EffectSettingChanged {});
}

void EffectDistortion::Editor::OnRepeatsSlider(wxCommandEvent& evt)
{
    auto& ms = mSettings;

    ms.mRepeats = evt.GetInt();
    mRepeatsT->GetValidator()->TransferToWindow();

    ValidateUI();
    Publish(EffectSettingChanged {});
}

void EffectDistortion::Editor::UpdateUIControls()
{
    const auto& ms = mSettings;

    // set control text and names to match distortion type
    switch (ms.mTableChoiceIndx) {
    case kHardClip:
        UpdateControlText(mThresholdT, mOldThresholdTxt, true);
        UpdateControlText(mNoiseFloorT, mOldmNoiseFloorTxt, false);
        UpdateControlText(mParam1T, mOldParam1Txt, true);
        UpdateControlText(mParam2T, mOldParam2Txt, true);
        UpdateControlText(mRepeatsT, mOldRepeatsTxt, false);

        UpdateControl(ID_Threshold, true, XO("Clipping level"));
        UpdateControl(ID_NoiseFloor, false, defaultLabel(1));
        UpdateControl(ID_Param1, true, XO("Drive"));
        UpdateControl(ID_Param2, true, XO("Make-up Gain"));
        UpdateControl(ID_Repeats, false, defaultLabel(4));
        UpdateControl(ID_DCBlock, false, {});
        break;
    case kSoftClip:
        UpdateControlText(mThresholdT, mOldThresholdTxt, true);
        UpdateControlText(mNoiseFloorT, mOldmNoiseFloorTxt, false);
        UpdateControlText(mParam1T, mOldParam1Txt, true);
        UpdateControlText(mParam2T, mOldParam2Txt, true);
        UpdateControlText(mRepeatsT, mOldRepeatsTxt, false);

        UpdateControl(ID_Threshold, true, XO("Clipping threshold"));
        UpdateControl(ID_NoiseFloor, false, defaultLabel(1));
        UpdateControl(ID_Param1, true, XO("Hardness"));
        UpdateControl(ID_Param2, true, XO("Make-up Gain"));
        UpdateControl(ID_Repeats, false, defaultLabel(4));
        UpdateControl(ID_DCBlock, false, {});
        break;
    case kHalfSinCurve:
        UpdateControlText(mThresholdT, mOldThresholdTxt, false);
        UpdateControlText(mNoiseFloorT, mOldmNoiseFloorTxt, false);
        UpdateControlText(mParam1T, mOldParam1Txt, true);
        UpdateControlText(mParam2T, mOldParam2Txt, true);
        UpdateControlText(mRepeatsT, mOldRepeatsTxt, false);

        UpdateControl(ID_Threshold, false, defaultLabel(0));
        UpdateControl(ID_NoiseFloor, false, defaultLabel(1));
        UpdateControl(ID_Param1, true, XO("Distortion amount"));
        UpdateControl(ID_Param2, true, XO("Output level"));
        UpdateControl(ID_Repeats, false, defaultLabel(4));
        UpdateControl(ID_DCBlock, false, {});
        break;
    case kExpCurve:
        UpdateControlText(mThresholdT, mOldThresholdTxt, false);
        UpdateControlText(mNoiseFloorT, mOldmNoiseFloorTxt, false);
        UpdateControlText(mParam1T, mOldParam1Txt, true);
        UpdateControlText(mParam2T, mOldParam2Txt, true);
        UpdateControlText(mRepeatsT, mOldRepeatsTxt, false);

        UpdateControl(ID_Threshold, false, defaultLabel(0));
        UpdateControl(ID_NoiseFloor, false, defaultLabel(1));
        UpdateControl(ID_Param1, true, XO("Distortion amount"));
        UpdateControl(ID_Param2, true, XO("Output level"));
        UpdateControl(ID_Repeats, false, defaultLabel(4));
        UpdateControl(ID_DCBlock, false, {});
        break;
    case kLogCurve:
        UpdateControlText(mThresholdT, mOldThresholdTxt, false);
        UpdateControlText(mNoiseFloorT, mOldmNoiseFloorTxt, false);
        UpdateControlText(mParam1T, mOldParam1Txt, true);
        UpdateControlText(mParam2T, mOldParam2Txt, true);
        UpdateControlText(mRepeatsT, mOldRepeatsTxt, false);

        UpdateControl(ID_Threshold, false, defaultLabel(0));
        UpdateControl(ID_NoiseFloor, false, defaultLabel(1));
        UpdateControl(ID_Param1, true, XO("Distortion amount"));
        UpdateControl(ID_Param2, true, XO("Output level"));
        UpdateControl(ID_Repeats, false, defaultLabel(4));
        UpdateControl(ID_DCBlock, false, {});
        break;
    case kCubic:
        UpdateControlText(mThresholdT, mOldThresholdTxt, false);
        UpdateControlText(mNoiseFloorT, mOldmNoiseFloorTxt, false);
        UpdateControlText(mParam1T, mOldParam1Txt, true);
        UpdateControlText(mParam2T, mOldParam2Txt, true);
        UpdateControlText(mRepeatsT, mOldRepeatsTxt, true);

        UpdateControl(ID_Threshold, false, defaultLabel(0));
        UpdateControl(ID_NoiseFloor, false, defaultLabel(1));
        UpdateControl(ID_Param1, true, XO("Distortion amount"));
        UpdateControl(ID_Param2, true, XO("Output level"));
        UpdateControl(ID_Repeats, true, XO("Repeat processing"));
        UpdateControl(ID_DCBlock, false, {});
        break;
    case kEvenHarmonics:
        UpdateControlText(mThresholdT, mOldThresholdTxt, false);
        UpdateControlText(mNoiseFloorT, mOldmNoiseFloorTxt, false);
        UpdateControlText(mParam1T, mOldParam1Txt, true);
        UpdateControlText(mParam2T, mOldParam2Txt, true);
        UpdateControlText(mRepeatsT, mOldRepeatsTxt, false);

        UpdateControl(ID_Threshold, false, defaultLabel(0));
        UpdateControl(ID_NoiseFloor, false, defaultLabel(1));
        UpdateControl(ID_Param1, true, XO("Distortion amount"));
        UpdateControl(ID_Param2, true, XO("Harmonic brightness"));
        UpdateControl(ID_Repeats, false, defaultLabel(4));
        UpdateControl(ID_DCBlock, true, {});
        break;
    case kSinCurve:
        UpdateControlText(mThresholdT, mOldThresholdTxt, false);
        UpdateControlText(mNoiseFloorT, mOldmNoiseFloorTxt, false);
        UpdateControlText(mParam1T, mOldParam1Txt, true);
        UpdateControlText(mParam2T, mOldParam2Txt, true);
        UpdateControlText(mRepeatsT, mOldRepeatsTxt, false);

        UpdateControl(ID_Threshold, false, defaultLabel(0));
        UpdateControl(ID_NoiseFloor, false, defaultLabel(1));
        UpdateControl(ID_Param1, true, XO("Distortion amount"));
        UpdateControl(ID_Param2, true, XO("Output level"));
        UpdateControl(ID_Repeats, false, defaultLabel(4));
        UpdateControl(ID_DCBlock, false, {});
        break;
    case kLeveller:
        UpdateControlText(mThresholdT, mOldThresholdTxt, false);
        UpdateControlText(mNoiseFloorT, mOldmNoiseFloorTxt, true);
        UpdateControlText(mParam1T, mOldParam1Txt, true);
        UpdateControlText(mParam2T, mOldParam2Txt, false);
        UpdateControlText(mRepeatsT, mOldRepeatsTxt, true);

        UpdateControl(ID_Threshold, false, defaultLabel(0));
        UpdateControl(ID_NoiseFloor, true, defaultLabel(1));
        UpdateControl(ID_Param1, true, XO("Levelling fine adjustment"));
        UpdateControl(ID_Param2, false, defaultLabel(3));
        UpdateControl(ID_Repeats, true, XO("Degree of Levelling"));
        UpdateControl(ID_DCBlock, false, {});
        break;
    case kRectifier:
        UpdateControlText(mThresholdT, mOldThresholdTxt, false);
        UpdateControlText(mNoiseFloorT, mOldmNoiseFloorTxt, false);
        UpdateControlText(mParam1T, mOldParam1Txt, true);
        UpdateControlText(mParam2T, mOldParam2Txt, false);
        UpdateControlText(mRepeatsT, mOldRepeatsTxt, false);

        UpdateControl(ID_Threshold, false, defaultLabel(0));
        UpdateControl(ID_NoiseFloor, false, defaultLabel(1));
        UpdateControl(ID_Param1, true, XO("Distortion amount"));
        UpdateControl(ID_Param2, false, defaultLabel(3));
        UpdateControl(ID_Repeats, false, defaultLabel(4));
        UpdateControl(ID_DCBlock, true, {});
        break;
    case kHardLimiter:
        UpdateControlText(mThresholdT, mOldThresholdTxt, true);
        UpdateControlText(mNoiseFloorT, mOldmNoiseFloorTxt, false);
        UpdateControlText(mParam1T, mOldParam1Txt, true);
        UpdateControlText(mParam2T, mOldParam2Txt, true);
        UpdateControlText(mRepeatsT, mOldRepeatsTxt, false);

        UpdateControl(ID_Threshold, true, XO("dB Limit"));
        UpdateControl(ID_NoiseFloor, false, defaultLabel(1));
        UpdateControl(ID_Param1, true, XO("Wet level"));
        UpdateControl(ID_Param2, true, XO("Residual level"));
        UpdateControl(ID_Repeats, false, defaultLabel(4));
        UpdateControl(ID_DCBlock, false, {});
        break;
    default:
        UpdateControl(ID_Threshold,   true, defaultLabel(0));
        UpdateControl(ID_NoiseFloor,  true, defaultLabel(1));
        UpdateControl(ID_Param1,      true, defaultLabel(2));
        UpdateControl(ID_Param2,      true, defaultLabel(3));
        UpdateControl(ID_Repeats,     true, defaultLabel(4));
        UpdateControl(ID_DCBlock,     false, {});
    }
}

void EffectDistortion::Editor::UpdateControl(
    control id, bool enabled, TranslatableString name)
{
    auto& ms = mSettings;

    auto suffix = XO("(Not Used):");
    switch (id) {
    case ID_Threshold: {
        /* i18n-hint: Control range. */
        if (enabled) {
            suffix = XO("(-100 to 0 dB):");
        }
        name.Join(suffix, wxT(" "));

        // Logarithmic slider is set indirectly
        const double threshold = DB_TO_LINEAR(ms.mThreshold_dB);
        mThresholdS->SetValue((int)(threshold * Threshold_dB.scale + 0.5));

        auto translated = name.Translation();
        mThresholdTxt->SetLabel(translated);
        mThresholdS->SetName(translated);
        mThresholdT->SetName(translated);
        mThresholdS->Enable(enabled);
        mThresholdT->Enable(enabled);
        break;
    }
    case ID_NoiseFloor: {
        /* i18n-hint: Control range. */
        if (enabled) {
            suffix = XO("(-80 to -20 dB):");
        }
        name.Join(suffix, wxT(" "));

        auto translated = name.Translation();
        mNoiseFloorTxt->SetLabel(translated);
        mNoiseFloorS->SetName(translated);
        mNoiseFloorT->SetName(translated);
        mNoiseFloorS->Enable(enabled);
        mNoiseFloorT->Enable(enabled);
        break;
    }
    case ID_Param1: {
        /* i18n-hint: Control range. */
        if (enabled) {
            suffix = XO("(0 to 100):");
        }
        name.Join(suffix, wxT(" "));

        auto translated = name.Translation();
        mParam1Txt->SetLabel(translated);
        mParam1S->SetName(translated);
        mParam1T->SetName(translated);
        mParam1S->Enable(enabled);
        mParam1T->Enable(enabled);
        break;
    }
    case ID_Param2: {
        /* i18n-hint: Control range. */
        if (enabled) {
            suffix = XO("(0 to 100):");
        }
        name.Join(suffix, wxT(" "));

        auto translated = name.Translation();
        mParam2Txt->SetLabel(translated);
        mParam2S->SetName(translated);
        mParam2T->SetName(translated);
        mParam2S->Enable(enabled);
        mParam2T->Enable(enabled);
        break;
    }
    case ID_Repeats: {
        /* i18n-hint: Control range. */
        if (enabled) {
            suffix = XO("(0 to 5):");
        }
        name.Join(suffix, wxT(" "));

        auto translated = name.Translation();
        mRepeatsTxt->SetLabel(translated);
        mRepeatsS->SetName(translated);
        mRepeatsT->SetName(translated);
        mRepeatsS->Enable(enabled);
        mRepeatsT->Enable(enabled);
        break;
    }
    case ID_DCBlock: {
        if (enabled) {
            mDCBlockCheckBox->SetValue(GetState().mbSavedFilterState);
            ms.mDCBlock = GetState().mbSavedFilterState;
        } else {
            mDCBlockCheckBox->SetValue(false);
            ms.mDCBlock = false;
        }

        mDCBlockCheckBox->Enable(enabled);
        break;
    }
    default:
        break;
    }
}

void EffectDistortion::Editor::UpdateControlText(wxTextCtrl* textCtrl, wxString& string, bool enabled)
{
    if (enabled) {
        if (textCtrl->GetValue().empty()) {
            textCtrl->SetValue(string);
        } else {
            string = textCtrl->GetValue();
        }
    } else {
        if (!textCtrl->GetValue().empty()) {
            string = textCtrl->GetValue();
        }
        textCtrl->SetValue(wxT(""));
    }
}
