/**********************************************************************

  Audacity: A Digital Audio Editor

  Phaser.cpp

  Dominic Mazzoni (with the help of wxDesigner)
  Vaughan Johnson (Preview)

**********************************************************************/
#include "Phaser.h"
#include "EffectEditor.h"
#include "LoadEffects.h"

#include <math.h>

#include <wx/slider.h>

#include "ShuttleGui.h"
#include "../widgets/valnum.h"
#include <wx/weakref.h>

namespace {
BuiltinEffectsModule::Registration< EffectPhaser > reg;
}

struct EffectPhaser::Editor : EffectEditor
{
    Editor(const EffectUIServices& services,
           EffectSettingsAccess& access, const EffectPhaserSettings& settings)
        : EffectEditor{services, access}
        , mSettings{settings}
    {}
    virtual ~Editor() = default;

    bool ValidateUI() override;
    bool UpdateUI() override;

    void PopulateOrExchange(ShuttleGui& S);

    wxWeakRef<wxWindow> mUIParent;
    EffectPhaserSettings mSettings;

    wxTextCtrl* mStagesT;
    wxTextCtrl* mDryWetT;
    wxTextCtrl* mFreqT;
    wxTextCtrl* mPhaseT;
    wxTextCtrl* mDepthT;
    wxTextCtrl* mFeedbackT;
    wxTextCtrl* mOutGainT;

    wxSlider* mStagesS;
    wxSlider* mDryWetS;
    wxSlider* mFreqS;
    wxSlider* mPhaseS;
    wxSlider* mDepthS;
    wxSlider* mFeedbackS;
    wxSlider* mOutGainS;

    void OnStagesSlider(wxCommandEvent& evt);
    void OnDryWetSlider(wxCommandEvent& evt);
    void OnFeedbackSlider(wxCommandEvent& evt);
    void OnDepthSlider(wxCommandEvent& evt);
    void OnPhaseSlider(wxCommandEvent& evt);
    void OnFreqSlider(wxCommandEvent& evt);
    void OnGainSlider(wxCommandEvent& evt);

    void OnStagesText(wxCommandEvent& evt);
    void OnDryWetText(wxCommandEvent& evt);
    void OnFeedbackText(wxCommandEvent& evt);
    void OnDepthText(wxCommandEvent& evt);
    void OnPhaseText(wxCommandEvent& evt);
    void OnFreqText(wxCommandEvent& evt);
    void OnGainText(wxCommandEvent& evt);

    void EnableApplyFromValidate()
    {
        EnableApply(mUIParent, mUIParent->Validate());
    }

    bool EnableApplyFromTransferDataFromWindow()
    {
        return EnableApply(mUIParent, mUIParent->TransferDataFromWindow());
    }
};

// Effect implementation
std::unique_ptr<EffectEditor> EffectPhaser::MakeEditor(
    ShuttleGui& S, EffectInstance&, EffectSettingsAccess& access,
    const EffectOutputs*) const
{
    auto& settings = access.Get();
    auto& myEffSettings = GetSettings(settings);

    auto result = std::make_unique<Editor>(*this, access, myEffSettings);
    result->PopulateOrExchange(S);
    return result;
}

void EffectPhaser::Editor::PopulateOrExchange(ShuttleGui& S)
{
    mUIParent = S.GetParent();
    auto& ms = mSettings;

    S.SetBorder(5);
    S.AddSpace(0, 5);

    S.StartMultiColumn(3, wxEXPAND);
    {
        S.SetStretchyCol(2);

        mStagesT = S
                   .Validator<IntegerValidator<int> >(
            &ms.mStages, NumValidatorStyle::DEFAULT, Stages.min, Stages.max)
                   .AddTextBox(XXO("&Stages:"), L"", 15);
        BindTo(*mStagesT, wxEVT_TEXT, &Editor::OnStagesText);

        mStagesS = S
                   .Name(XO("Stages"))
                   .Style(wxSL_HORIZONTAL)
                   .MinSize({ 100, -1 })
                   .AddSlider({}, Stages.def * Stages.scale, Stages.max * Stages.scale, Stages.min * Stages.scale);
        mStagesS->SetLineSize(2);
        BindTo(*mStagesS, wxEVT_SLIDER, &Editor::OnStagesSlider);

        mDryWetT = S
                   .Validator<IntegerValidator<int> >(
            &ms.mDryWet, NumValidatorStyle::DEFAULT, DryWet.min, DryWet.max)
                   .AddTextBox(XXO("&Dry/Wet:"), L"", 15);
        BindTo(*mDryWetT, wxEVT_TEXT, &Editor::OnDryWetText);

        mDryWetS = S
                   .Name(XO("Dry Wet"))
                   .Style(wxSL_HORIZONTAL)
                   .MinSize({ 100, -1 })
                   .AddSlider({}, DryWet.def * DryWet.scale, DryWet.max * DryWet.scale, DryWet.min * DryWet.scale);
        BindTo(*mDryWetS, wxEVT_SLIDER, &Editor::OnDryWetSlider);

        mFreqT = S
                 .Validator<FloatingPointValidator<double> >(
            5, &ms.mFreq, NumValidatorStyle::ONE_TRAILING_ZERO, Freq.min, Freq.max)
                 .AddTextBox(XXO("LFO Freq&uency (Hz):"), L"", 15);
        BindTo(*mFreqT, wxEVT_TEXT, &Editor::OnFreqText);

        mFreqS = S
                 .Name(XO("LFO frequency in hertz"))
                 .Style(wxSL_HORIZONTAL)
                 .MinSize({ 100, -1 })
                 .AddSlider({}, Freq.def * Freq.scale, Freq.max * Freq.scale, 0.0);
        BindTo(*mFreqS, wxEVT_SLIDER, &Editor::OnFreqSlider);

        mPhaseT = S
                  .Validator<FloatingPointValidator<double> >(
            1, &ms.mPhase, NumValidatorStyle::DEFAULT, Phase.min, Phase.max)
                  .AddTextBox(XXO("LFO Sta&rt Phase (deg.):"), L"", 15);
        BindTo(*mPhaseT, wxEVT_TEXT, &Editor::OnPhaseText);

        mPhaseS = S
                  .Name(XO("LFO start phase in degrees"))
                  .Style(wxSL_HORIZONTAL)
                  .MinSize({ 100, -1 })
                  .AddSlider({}, Phase.def * Phase.scale, Phase.max * Phase.scale, Phase.min * Phase.scale);
        mPhaseS->SetLineSize(10);
        BindTo(*mPhaseS, wxEVT_SLIDER, &Editor::OnPhaseSlider);

        mDepthT = S
                  .Validator<IntegerValidator<int> >(
            &ms.mDepth, NumValidatorStyle::DEFAULT, Depth.min, Depth.max)
                  .AddTextBox(XXO("Dept&h:"), L"", 15);
        BindTo(*mDepthT, wxEVT_TEXT, &Editor::OnDepthText);

        mDepthS = S
                  .Name(XO("Depth in percent"))
                  .Style(wxSL_HORIZONTAL)
                  .MinSize({ 100, -1 })
                  .AddSlider({}, Depth.def * Depth.scale, Depth.max * Depth.scale, Depth.min * Depth.scale);
        BindTo(*mDepthS, wxEVT_SLIDER, &Editor::OnDepthSlider);

        mFeedbackT = S
                     .Validator<IntegerValidator<int> >(
            &ms.mFeedback, NumValidatorStyle::DEFAULT, Feedback.min, Feedback.max)
                     .AddTextBox(XXO("Feedbac&k (%):"), L"", 15);
        BindTo(*mFeedbackT, wxEVT_TEXT, &Editor::OnFeedbackText);

        mFeedbackS = S
                     .Name(XO("Feedback in percent"))
                     .Style(wxSL_HORIZONTAL)
                     .MinSize({ 100, -1 })
                     .AddSlider({}, Feedback.def * Feedback.scale, Feedback.max * Feedback.scale, Feedback.min * Feedback.scale);
        mFeedbackS->SetLineSize(10);
        BindTo(*mFeedbackS, wxEVT_SLIDER, &Editor::OnFeedbackSlider);

        mOutGainT = S
                    .Validator<FloatingPointValidator<double> >(
            1, &ms.mOutGain, NumValidatorStyle::DEFAULT, OutGain.min, OutGain.max)
                    .AddTextBox(XXO("&Output gain (dB):"), L"", 12);
        BindTo(*mOutGainT, wxEVT_TEXT, &Editor::OnGainText);

        mOutGainS = S
                    .Name(XO("Output gain (dB)"))
                    .Style(wxSL_HORIZONTAL)
                    .MinSize({ 100, -1 })
                    .AddSlider({}, OutGain.def * OutGain.scale, OutGain.max * OutGain.scale, OutGain.min * OutGain.scale);
        BindTo(*mOutGainS, wxEVT_SLIDER, &Editor::OnGainSlider);
    }
    S.EndMultiColumn();
}

bool EffectPhaser::Editor::UpdateUI()
{
    // get the settings from the MessageBuffer and write them to our local copy
    const auto& settings = mAccess.Get();

    mSettings = GetSettings(settings);

    if (!mUIParent->TransferDataToWindow()) {
        return false;
    }

    mStagesS->SetValue((int)(mSettings.mStages * Stages.scale));
    mDryWetS->SetValue((int)(mSettings.mDryWet * DryWet.scale));
    mFreqS->SetValue((int)(mSettings.mFreq * Freq.scale));
    mPhaseS->SetValue((int)(mSettings.mPhase * Phase.scale));
    mDepthS->SetValue((int)(mSettings.mDepth * Depth.scale));
    mFeedbackS->SetValue((int)(mSettings.mFeedback * Feedback.scale));
    mOutGainS->SetValue((int)(mSettings.mOutGain * OutGain.scale));

    return true;
}

bool EffectPhaser::Editor::ValidateUI()
{
    // This bit was copied from the original override of TransferDataFromWindow
    if (!mUIParent->Validate() || !mUIParent->TransferDataFromWindow()) {
        return false;
    }

    auto& ms = mSettings;

    if (ms.mStages & 1) { // must be even
        ms.mStages &= ~1;
        mStagesT->GetValidator()->TransferToWindow();
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

void EffectPhaser::Editor::OnStagesSlider(wxCommandEvent& evt)
{
    auto& ms = mSettings;

    ms.mStages = (evt.GetInt() / Stages.scale) & ~1; // must be even;
    mStagesT->GetValidator()->TransferToWindow();
    EnableApplyFromValidate();

    ValidateUI();
    Publish(EffectSettingChanged {});
}

void EffectPhaser::Editor::OnDryWetSlider(wxCommandEvent& evt)
{
    auto& ms = mSettings;

    ms.mDryWet = evt.GetInt() / DryWet.scale;
    mDryWetT->GetValidator()->TransferToWindow();
    EnableApplyFromValidate();

    ValidateUI();
    Publish(EffectSettingChanged {});
}

void EffectPhaser::Editor::OnFreqSlider(wxCommandEvent& evt)
{
    auto& ms = mSettings;

    ms.mFreq = (double)evt.GetInt() / Freq.scale;
    if (ms.mFreq < Freq.min) {
        ms.mFreq = Freq.min;
    }
    mFreqT->GetValidator()->TransferToWindow();
    EnableApplyFromValidate();

    ValidateUI();
    Publish(EffectSettingChanged {});
}

void EffectPhaser::Editor::OnPhaseSlider(wxCommandEvent& evt)
{
    auto& ms = mSettings;

    int val = ((evt.GetInt() + 5) / 10) * 10; // round to nearest multiple of 10
    val = val > Phase.max * Phase.scale ? Phase.max * Phase.scale : val;
    mPhaseS->SetValue(val);
    ms.mPhase =  (double)val / Phase.scale;
    mPhaseT->GetValidator()->TransferToWindow();
    EnableApplyFromValidate();

    ValidateUI();
    Publish(EffectSettingChanged {});
}

void EffectPhaser::Editor::OnDepthSlider(wxCommandEvent& evt)
{
    auto& ms = mSettings;

    ms.mDepth = evt.GetInt() / Depth.scale;
    mDepthT->GetValidator()->TransferToWindow();
    EnableApplyFromValidate();

    ValidateUI();
    Publish(EffectSettingChanged {});
}

void EffectPhaser::Editor::OnFeedbackSlider(wxCommandEvent& evt)
{
    auto& ms = mSettings;

    int val = evt.GetInt();
    val = ((val + (val > 0 ? 5 : -5)) / 10) * 10; // round to nearest multiple of 10
    val = val > Feedback.max * Feedback.scale ? Feedback.max * Feedback.scale : val;
    mFeedbackS->SetValue(val);
    ms.mFeedback = val / Feedback.scale;
    mFeedbackT->GetValidator()->TransferToWindow();
    EnableApplyFromValidate();

    ValidateUI();
    Publish(EffectSettingChanged {});
}

void EffectPhaser::Editor::OnGainSlider(wxCommandEvent& evt)
{
    auto& ms = mSettings;

    ms.mOutGain = evt.GetInt() / OutGain.scale;
    mOutGainT->GetValidator()->TransferToWindow();
    EnableApplyFromValidate();

    ValidateUI();
    Publish(EffectSettingChanged {});
}

void EffectPhaser::Editor::OnStagesText(wxCommandEvent& WXUNUSED(evt))
{
    auto& ms = mSettings;

    if (!EnableApplyFromTransferDataFromWindow()) {
        return;
    }

    mStagesS->SetValue((int)(ms.mStages * Stages.scale));

    ValidateUI();
    Publish(EffectSettingChanged {});
}

void EffectPhaser::Editor::OnDryWetText(wxCommandEvent& WXUNUSED(evt))
{
    auto& ms = mSettings;

    if (!EnableApplyFromTransferDataFromWindow()) {
        return;
    }

    mDryWetS->SetValue((int)(ms.mDryWet * DryWet.scale));

    ValidateUI();
    Publish(EffectSettingChanged {});
}

void EffectPhaser::Editor::OnFreqText(wxCommandEvent& WXUNUSED(evt))
{
    auto& ms = mSettings;

    if (!EnableApplyFromTransferDataFromWindow()) {
        return;
    }

    mFreqS->SetValue((int)(ms.mFreq * Freq.scale));

    ValidateUI();
    Publish(EffectSettingChanged {});
}

void EffectPhaser::Editor::OnPhaseText(wxCommandEvent& WXUNUSED(evt))
{
    auto& ms = mSettings;

    if (!EnableApplyFromTransferDataFromWindow()) {
        return;
    }

    mPhaseS->SetValue((int)(ms.mPhase * Phase.scale));

    ValidateUI();
    Publish(EffectSettingChanged {});
}

void EffectPhaser::Editor::OnDepthText(wxCommandEvent& WXUNUSED(evt))
{
    auto& ms = mSettings;

    if (!EnableApplyFromTransferDataFromWindow()) {
        return;
    }

    mDepthS->SetValue((int)(ms.mDepth * Depth.scale));

    ValidateUI();
    Publish(EffectSettingChanged {});
}

void EffectPhaser::Editor::OnFeedbackText(wxCommandEvent& WXUNUSED(evt))
{
    auto& ms = mSettings;

    if (!EnableApplyFromTransferDataFromWindow()) {
        return;
    }

    mFeedbackS->SetValue((int)(ms.mFeedback * Feedback.scale));

    ValidateUI();
    Publish(EffectSettingChanged {});
}

void EffectPhaser::Editor::OnGainText(wxCommandEvent& WXUNUSED(evt))
{
    auto& ms = mSettings;

    if (!EnableApplyFromTransferDataFromWindow()) {
        return;
    }

    mOutGainS->SetValue((int)(ms.mOutGain * OutGain.scale));

    ValidateUI();
    Publish(EffectSettingChanged {});
}
