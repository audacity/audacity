/**********************************************************************

  Audacity: A Digital Audio Editor

  Wahwah.cpp

  Effect programming:
  Nasca Octavian Paul (Paul Nasca)

  UI programming:
  Dominic Mazzoni (with the help of wxDesigner)
  Vaughan Johnson (Preview)

*******************************************************************//**

\class EffectWahwah
\brief An Effect that adds a 'spectral glide'.

*//*******************************************************************/

#include "Wahwah.h"
#include "EffectEditor.h"
#include "LoadEffects.h"

#include <math.h>

#include <wx/slider.h>
#include <wx/weakref.h>

#include "ShuttleGui.h"
#include "../widgets/valnum.h"

namespace {
BuiltinEffectsModule::Registration< EffectWahwah > reg;
}

struct EffectWahwah::Editor : EffectEditor
{
    Editor(const EffectUIServices& services,
           EffectSettingsAccess& access, const EffectWahwahSettings& settings)
        : EffectEditor{services, access}
        , mSettings{settings}
    {}
    virtual ~Editor() = default;

    bool ValidateUI() override;
    bool UpdateUI() override;

    void PopulateOrExchange(ShuttleGui& S);

    void OnFreqSlider(wxCommandEvent& evt);
    void OnPhaseSlider(wxCommandEvent& evt);
    void OnDepthSlider(wxCommandEvent& evt);
    void OnResonanceSlider(wxCommandEvent& evt);
    void OnFreqOffSlider(wxCommandEvent& evt);
    void OnGainSlider(wxCommandEvent& evt);

    void OnFreqText(wxCommandEvent& evt);
    void OnPhaseText(wxCommandEvent& evt);
    void OnDepthText(wxCommandEvent& evt);
    void OnResonanceText(wxCommandEvent& evt);
    void OnFreqOffText(wxCommandEvent& evt);
    void OnGainText(wxCommandEvent& evt);

    wxTextCtrl* mFreqT;
    wxTextCtrl* mPhaseT;
    wxTextCtrl* mDepthT;
    wxTextCtrl* mResT;
    wxTextCtrl* mFreqOfsT;
    wxTextCtrl* mOutGainT;

    wxSlider* mFreqS;
    wxSlider* mPhaseS;
    wxSlider* mDepthS;
    wxSlider* mResS;
    wxSlider* mFreqOfsS;
    wxSlider* mOutGainS;

    wxWeakRef<wxWindow> mUIParent;
    EffectWahwahSettings mSettings;

    void EnableApplyFromValidate()
    {
        EnableApply(mUIParent, mUIParent->Validate());
    }

    bool EnableApplyFromTransferDataToWindow()
    {
        return EnableApply(mUIParent, mUIParent->TransferDataFromWindow());
    }
};

bool EffectWahwah::Editor::ValidateUI()
{
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

// Effect implementation

std::unique_ptr<EffectEditor> EffectWahwah::MakeEditor(
    ShuttleGui& S, EffectInstance&, EffectSettingsAccess& access,
    const EffectOutputs*) const
{
    auto& settings = access.Get();
    auto& myEffSettings = GetSettings(settings);
    auto result = std::make_unique<Editor>(*this, access, myEffSettings);
    result->PopulateOrExchange(S);
    return result;
}

void EffectWahwah::Editor::PopulateOrExchange(ShuttleGui& S)
{
    mUIParent = S.GetParent();
    auto& ms = mSettings;

    S.SetBorder(5);
    S.AddSpace(0, 5);

    S.StartMultiColumn(3, wxEXPAND);
    {
        S.SetStretchyCol(2);

        mFreqT = S
                 .Validator<FloatingPointValidator<double> >(
            5, &ms.mFreq, NumValidatorStyle::ONE_TRAILING_ZERO, Freq.min, Freq.max)
                 .AddTextBox(XXO("LFO Freq&uency (Hz):"), L"", 12);
        BindTo(*mFreqT, wxEVT_TEXT, &Editor::OnFreqText);

        mFreqS = S
                 .Name(XO("LFO frequency in hertz"))
                 .Style(wxSL_HORIZONTAL)
                 .MinSize({ 100, -1 })
                 .AddSlider({}, Freq.def * Freq.scale, Freq.max * Freq.scale, Freq.min * Freq.scale);
        BindTo(*mFreqS, wxEVT_SLIDER, &Editor::OnFreqSlider);

        mPhaseT = S
                  .Validator<FloatingPointValidator<double> >(
            1, &ms.mPhase, NumValidatorStyle::DEFAULT, Phase.min, Phase.max)
                  .AddTextBox(XXO("LFO Sta&rt Phase (deg.):"), L"", 12);
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
                  .AddTextBox(XXO("Dept&h (%):"), L"", 12);
        BindTo(*mDepthT, wxEVT_TEXT, &Editor::OnDepthText);

        mDepthS = S
                  .Name(XO("Depth in percent"))
                  .Style(wxSL_HORIZONTAL)
                  .MinSize({ 100, -1 })
                  .AddSlider({}, Depth.def * Depth.scale, Depth.max * Depth.scale, Depth.min * Depth.scale);
        BindTo(*mDepthS, wxEVT_SLIDER, &Editor::OnDepthSlider);

        mResT = S
                .Validator<FloatingPointValidator<double> >(
            1, &ms.mRes, NumValidatorStyle::DEFAULT, Res.min, Res.max)
                .AddTextBox(XXO("Reso&nance:"), L"", 12);
        BindTo(*mResT, wxEVT_TEXT, &Editor::OnResonanceText);

        mResS = S
                .Name(XO("Resonance"))
                .Style(wxSL_HORIZONTAL)
                .MinSize({ 100, -1 })
                .AddSlider({}, Res.def * Res.scale, Res.max * Res.scale, Res.min * Res.scale);
        BindTo(*mResS, wxEVT_SLIDER, &Editor::OnResonanceSlider);

        mFreqOfsT = S
                    .Validator<IntegerValidator<int> >(
            &ms.mFreqOfs, NumValidatorStyle::DEFAULT, FreqOfs.min, FreqOfs.max)
                    .AddTextBox(XXO("Wah Frequency Offse&t (%):"), L"", 12);
        BindTo(*mFreqOfsT, wxEVT_TEXT, &Editor::OnFreqOffText);

        mFreqOfsS = S
                    .Name(XO("Wah frequency offset in percent"))
                    .Style(wxSL_HORIZONTAL)
                    .MinSize({ 100, -1 })
                    .AddSlider({}, FreqOfs.def * FreqOfs.scale, FreqOfs.max * FreqOfs.scale, FreqOfs.min * FreqOfs.scale);
        BindTo(*mFreqOfsS, wxEVT_SLIDER, &Editor::OnFreqOffSlider);

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

bool EffectWahwah::Editor::UpdateUI()
{
    // get the settings from the MessageBuffer and write them to our local copy
    const auto& settings = mAccess.Get();

    mSettings = GetSettings(settings);

    auto& ms = mSettings;

    mFreqS->SetValue((int)(ms.mFreq * Freq.scale));
    mPhaseS->SetValue((int)(ms.mPhase * Phase.scale));
    mDepthS->SetValue((int)(ms.mDepth * Depth.scale));
    mResS->SetValue((int)(ms.mRes * Res.scale));
    mFreqOfsS->SetValue((int)(ms.mFreqOfs * FreqOfs.scale));
    mOutGainS->SetValue((int)(ms.mOutGain * OutGain.scale));

    return true;
}

void EffectWahwah::Editor::OnFreqSlider(wxCommandEvent& evt)
{
    auto& ms = mSettings;

    ms.mFreq = (double)evt.GetInt() / Freq.scale;
    mFreqT->GetValidator()->TransferToWindow();

    EnableApplyFromValidate();
    ValidateUI();
    Publish(EffectSettingChanged {});
}

void EffectWahwah::Editor::OnPhaseSlider(wxCommandEvent& evt)
{
    auto& ms = mSettings;

    int val = ((evt.GetInt() + 5) / 10) * 10; // round to nearest multiple of 10
    val = val > Phase.max * Phase.scale ? Phase.max * Phase.scale : val;
    mPhaseS->SetValue(val);
    ms.mPhase = (double)val / Phase.scale;
    mPhaseT->GetValidator()->TransferToWindow();

    EnableApplyFromValidate();
    ValidateUI();
    Publish(EffectSettingChanged {});
}

void EffectWahwah::Editor::OnDepthSlider(wxCommandEvent& evt)
{
    auto& ms = mSettings;

    ms.mDepth = evt.GetInt() / Depth.scale;
    mDepthT->GetValidator()->TransferToWindow();

    EnableApplyFromValidate();
    ValidateUI();
    Publish(EffectSettingChanged {});
}

void EffectWahwah::Editor::OnResonanceSlider(wxCommandEvent& evt)
{
    auto& ms = mSettings;

    ms.mRes = (double)evt.GetInt() / Res.scale;
    mResT->GetValidator()->TransferToWindow();

    EnableApplyFromValidate();
    ValidateUI();
    Publish(EffectSettingChanged {});
}

void EffectWahwah::Editor::OnFreqOffSlider(wxCommandEvent& evt)
{
    auto& ms = mSettings;

    ms.mFreqOfs = evt.GetInt() / FreqOfs.scale;
    mFreqOfsT->GetValidator()->TransferToWindow();

    EnableApplyFromValidate();
    ValidateUI();
    Publish(EffectSettingChanged {});
}

void EffectWahwah::Editor::OnGainSlider(wxCommandEvent& evt)
{
    auto& ms = mSettings;

    ms.mOutGain = evt.GetInt() / OutGain.scale;
    mOutGainT->GetValidator()->TransferToWindow();

    EnableApplyFromValidate();
    ValidateUI();
    Publish(EffectSettingChanged {});
}

void EffectWahwah::Editor::OnFreqText(wxCommandEvent& WXUNUSED(evt))
{
    auto& ms = mSettings;

    if (!EnableApplyFromTransferDataToWindow()) {
        return;
    }

    mFreqS->SetValue((int)(ms.mFreq * Freq.scale));
    ValidateUI();
    Publish(EffectSettingChanged {});
}

void EffectWahwah::Editor::OnPhaseText(wxCommandEvent& WXUNUSED(evt))
{
    auto& ms = mSettings;

    if (!EnableApplyFromTransferDataToWindow()) {
        return;
    }

    mPhaseS->SetValue((int)(ms.mPhase * Phase.scale));
    ValidateUI();
    Publish(EffectSettingChanged {});
}

void EffectWahwah::Editor::OnDepthText(wxCommandEvent& WXUNUSED(evt))
{
    auto& ms = mSettings;

    if (!EnableApplyFromTransferDataToWindow()) {
        return;
    }

    mDepthS->SetValue((int)(ms.mDepth * Depth.scale));
    ValidateUI();
    Publish(EffectSettingChanged {});
}

void EffectWahwah::Editor::OnResonanceText(wxCommandEvent& WXUNUSED(evt))
{
    auto& ms = mSettings;

    if (!EnableApplyFromTransferDataToWindow()) {
        return;
    }

    mResS->SetValue((int)(ms.mRes * Res.scale));
    ValidateUI();
    Publish(EffectSettingChanged {});
}

void EffectWahwah::Editor::OnFreqOffText(wxCommandEvent& WXUNUSED(evt))
{
    auto& ms = mSettings;

    if (!EnableApplyFromTransferDataToWindow()) {
        return;
    }

    mFreqOfsS->SetValue((int)(ms.mFreqOfs * FreqOfs.scale));
    ValidateUI();
    Publish(EffectSettingChanged {});
}

void EffectWahwah::Editor::OnGainText(wxCommandEvent& WXUNUSED(evt))
{
    auto& ms = mSettings;

    if (!EnableApplyFromTransferDataToWindow()) {
        return;
    }

    mOutGainS->SetValue((int)(ms.mOutGain * OutGain.scale));
    ValidateUI();
    Publish(EffectSettingChanged {});
}
