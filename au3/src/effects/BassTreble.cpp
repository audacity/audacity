/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2016 Audacity Team.
   License: GPL v2 or later.  See License.txt.

   BassTreble.cpp
   Steve Daulton

******************************************************************//**

\class EffectBassTreble
\brief A high shelf and low shelf filter.

*//*******************************************************************/
#include "BassTreble.h"
#include "EffectEditor.h"
#include "LoadEffects.h"

#include <math.h>
#include <algorithm>

#include <wx/checkbox.h>
#include <wx/panel.h>
#include <wx/slider.h>
#include <wx/weakref.h>

#include "Prefs.h"
#include "ShuttleGui.h"
#include "WaveTrack.h"
#include "../widgets/valnum.h"

namespace {
BuiltinEffectsModule::Registration< EffectBassTreble > reg;
}

struct EffectBassTreble::Editor : EffectEditor
{
    Editor(const EffectUIServices& services,
           EffectSettingsAccess& access, const BassTrebleSettings& settings)
        : EffectEditor{services, access}
        , mSettings{settings}
    {}
    virtual ~Editor() = default;

    bool ValidateUI() override;
    bool UpdateUI() override;

    void PopulateOrExchange(ShuttleGui& S);

    wxWeakRef<wxWindow> mUIParent{};
    BassTrebleSettings mSettings;

    wxSlider* mBassS;
    wxSlider* mTrebleS;
    wxSlider* mGainS;

    wxTextCtrl* mBassT;
    wxTextCtrl* mTrebleT;
    wxTextCtrl* mGainT;

    wxCheckBox* mLinkCheckBox;

    void OnBassText(wxCommandEvent& evt);
    void OnTrebleText(wxCommandEvent& evt);
    void OnGainText(wxCommandEvent& evt);
    void OnBassSlider(wxCommandEvent& evt);
    void OnTrebleSlider(wxCommandEvent& evt);
    void OnGainSlider(wxCommandEvent& evt);
    void OnLinkCheckbox(wxCommandEvent& evt);

    // Auto-adjust gain to reduce variation in peak level
    void UpdateGain(double oldVal, int control);

    void EnableApplyFromValidate()
    {
        EnableApply(mUIParent, mUIParent->Validate());
    }

    bool EnableApplyFromTransferDataFromWindow()
    {
        return EnableApply(
            mUIParent, mUIParent->TransferDataFromWindow());
    }
};

std::shared_ptr<EffectInstance>
EffectBassTreble::MakeInstance() const
{
    return std::make_shared<BassTrebleBase::Instance>(*this);
}

// Effect implementation

std::unique_ptr<EffectEditor> EffectBassTreble::MakeEditor(
    ShuttleGui& S, EffectInstance&, EffectSettingsAccess& access,
    const EffectOutputs*) const
{
    auto& settings = access.Get();
    auto& myEffSettings = GetSettings(settings);

    auto result = std::make_unique<Editor>(*this, access, myEffSettings);
    result->PopulateOrExchange(S);
    return result;
}

void EffectBassTreble::Editor::PopulateOrExchange(ShuttleGui& S)
{
    mUIParent = S.GetParent();
    auto& ms = mSettings;

    S.SetBorder(5);
    S.AddSpace(0, 5);

    S.StartStatic(XO("Tone controls"));
    {
        S.StartMultiColumn(3, wxEXPAND);
        {
            S.SetStretchyCol(2);

            // Bass control
            mBassT = S
                     .Name(XO("Bass (dB):"))
                     .Validator<FloatingPointValidator<double> >(
                1, &ms.mBass, NumValidatorStyle::DEFAULT, Bass.min, Bass.max)
                     .AddTextBox(XXO("Ba&ss (dB):"), L"", 10);
            BindTo(*mBassT, wxEVT_TEXT, &Editor::OnBassText);

            mBassS = S
                     .Name(XO("Bass"))
                     .Style(wxSL_HORIZONTAL)
                     .AddSlider({}, 0, Bass.max * Bass.scale, Bass.min * Bass.scale);
            BindTo(*mBassS, wxEVT_SLIDER, &Editor::OnBassSlider);

            // Treble control
            mTrebleT = S
                       .Validator<FloatingPointValidator<double> >(
                1, &ms.mTreble, NumValidatorStyle::DEFAULT, Treble.min, Treble.max)
                       .AddTextBox(XXO("&Treble (dB):"), L"", 10);
            BindTo(*mTrebleT, wxEVT_TEXT, &Editor::OnTrebleText);

            mTrebleS = S
                       .Name(XO("Treble"))
                       .Style(wxSL_HORIZONTAL)
                       .AddSlider({}, 0, Treble.max * Treble.scale, Treble.min * Treble.scale);
            BindTo(*mTrebleS, wxEVT_SLIDER, &Editor::OnTrebleSlider);
        }
        S.EndMultiColumn();
    }
    S.EndStatic();

    S.StartStatic(XO("Output"));
    {
        S.StartMultiColumn(3, wxEXPAND);
        {
            S.SetStretchyCol(2);

            // Gain control
            mGainT = S
                     .Validator<FloatingPointValidator<double> >(
                1, &ms.mGain, NumValidatorStyle::DEFAULT, Gain.min, Gain.max)
                     .AddTextBox(XXO("&Volume (dB):"), L"", 10);
            BindTo(*mGainT, wxEVT_TEXT, &Editor::OnGainText);

            mGainS = S
                     .Name(XO("Level"))
                     .Style(wxSL_HORIZONTAL)
                     .AddSlider({}, 0, Gain.max * Gain.scale, Gain.min * Gain.scale);
            BindTo(*mGainS, wxEVT_SLIDER, &Editor::OnGainSlider);
        }
        S.EndMultiColumn();

        S.StartMultiColumn(2, wxCENTER);
        {
            // Link checkbox
            mLinkCheckBox
                =S
                  .AddCheckBox(XXO("&Link Volume control to Tone controls"),
                               Link.def);
            BindTo(*mLinkCheckBox, wxEVT_CHECKBOX, &Editor::OnLinkCheckbox);
        }
        S.EndMultiColumn();
    }
    S.EndStatic();
}

bool EffectBassTreble::Editor::UpdateUI()
{
    // get the settings from the MessageBuffer and write them to our local copy
    const auto& settings = mAccess.Get();

    mSettings = GetSettings(settings);

    if (!mUIParent->TransferDataToWindow()) {
        return false;
    }

    mBassS->SetValue((int)(mSettings.mBass * Bass.scale));
    mTrebleS->SetValue((int)(mSettings.mTreble * Treble.scale));
    mGainS->SetValue((int)(mSettings.mGain * Gain.scale));
    mLinkCheckBox->SetValue(mSettings.mLink);

    return true;
}

void EffectBassTreble::Editor::OnBassText(wxCommandEvent& WXUNUSED(evt))
{
    auto& ms = mSettings;

    double oldBass = ms.mBass;

    if (!EnableApplyFromTransferDataFromWindow()) {
        return;
    }

    if (ms.mLink) {
        UpdateGain(oldBass, kBass);
    }

    mBassS->SetValue((int)(ms.mBass * Bass.scale));

    ValidateUI();
    Publish(EffectSettingChanged {});
}

void EffectBassTreble::Editor::OnTrebleText(wxCommandEvent& WXUNUSED(evt))
{
    auto& ms = mSettings;

    double oldTreble = ms.mTreble;

    if (!EnableApplyFromTransferDataFromWindow()) {
        return;
    }

    if (ms.mLink) {
        UpdateGain(oldTreble, kTreble);
    }

    mTrebleS->SetValue((int)(ms.mTreble * Treble.scale));

    ValidateUI();
    Publish(EffectSettingChanged {});
}

void EffectBassTreble::Editor::OnGainText(wxCommandEvent& WXUNUSED(evt))
{
    auto& ms = mSettings;

    if (!EnableApplyFromTransferDataFromWindow()) {
        return;
    }

    mGainS->SetValue((int)(ms.mGain * Gain.scale));

    ValidateUI();
    Publish(EffectSettingChanged {});
}

void EffectBassTreble::Editor::OnBassSlider(wxCommandEvent& evt)
{
    auto& ms = mSettings;

    double oldBass = ms.mBass;
    ms.mBass = (double)evt.GetInt() / Bass.scale;
    mBassT->GetValidator()->TransferToWindow();

    if (ms.mLink) {
        UpdateGain(oldBass, kBass);
    }

    EnableApplyFromValidate();

    ValidateUI();
    Publish(EffectSettingChanged {});
}

void EffectBassTreble::Editor::OnTrebleSlider(wxCommandEvent& evt)
{
    auto& ms = mSettings;

    double oldTreble = ms.mTreble;
    ms.mTreble = (double)evt.GetInt() / Treble.scale;
    mTrebleT->GetValidator()->TransferToWindow();

    if (ms.mLink) {
        UpdateGain(oldTreble, kTreble);
    }

    EnableApplyFromValidate();

    ValidateUI();
    Publish(EffectSettingChanged {});
}

void EffectBassTreble::Editor::OnGainSlider(wxCommandEvent& evt)
{
    auto& ms = mSettings;

    ms.mGain = (double)evt.GetInt() / Gain.scale;
    mGainT->GetValidator()->TransferToWindow();

    EnableApplyFromValidate();

    ValidateUI();
    Publish(EffectSettingChanged {});
}

void EffectBassTreble::Editor::OnLinkCheckbox(wxCommandEvent& /*evt*/)
{
    auto& ms = mSettings;

    ms.mLink = mLinkCheckBox->GetValue();

    ValidateUI();
    Publish(EffectSettingChanged {});
}

void EffectBassTreble::Editor::UpdateGain(double oldVal, int control)
{
    auto& ms = mSettings;

    double newVal;
    oldVal = (oldVal > 0) ? oldVal / 2.0 : oldVal / 4.0;

    if (control == kBass) {
        newVal = (ms.mBass > 0) ? ms.mBass / 2.0 : ms.mBass / 4.0;
    } else {
        newVal = (ms.mTreble > 0) ? ms.mTreble / 2.0 : ms.mTreble / 4.0;
    }

    ms.mGain -= newVal - oldVal;
    ms.mGain = std::min(Gain.max, std::max(Gain.min, ms.mGain));

    mGainS->SetValue(ms.mGain);
    mGainT->GetValidator()->TransferToWindow();
}

bool EffectBassTreble::Editor::ValidateUI()
{
    // This bit was copied from the original override of the effect's TransferDataFromWindow
    if (!mUIParent->Validate() || !mUIParent->TransferDataFromWindow()) {
        return false;
    }

    mAccess.ModifySettings
    (
        [this](EffectSettings& settings)
    {
        // pass back the modified settings to the MessageBuffer
        //
        EffectBassTreble::GetSettings(settings) = mSettings;

        return nullptr;
    }
    );

    return true;
}
