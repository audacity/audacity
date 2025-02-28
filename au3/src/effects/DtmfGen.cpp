/**********************************************************************

  Audacity: A Digital Audio Editor

  DtmfGen.cpp

  Salvo Ventura - Dec 2006

*******************************************************************//**

\class EffectDtmf
\brief An effect that generates DTMF tones

*//*******************************************************************/

#include "DtmfGen.h"
#include "EffectEditor.h"
#include "LoadEffects.h"

#include <wx/slider.h>
#include <wx/valgen.h>
#include <wx/valtext.h>
#include <wx/stattext.h>

#include "ShuttleGui.h"
#include "../widgets/NumericTextCtrl.h"
#include "../widgets/valnum.h"

namespace {
BuiltinEffectsModule::Registration< EffectDtmf > reg;
}

// Event handler object
struct EffectDtmf::Editor : EffectEditor
{
    Editor(const EffectUIServices& effect,
           EffectSettingsAccess& access, const DtmfSettings& settings)
        : EffectEditor{effect, access}
        // Copy settings
        , mSettings{settings}
    {}
    virtual ~Editor() = default;

    bool ValidateUI() override;
    bool UpdateUI() override;
    void DoUpdateUI();

    void PopulateOrExchange(ShuttleGui& S, const EffectSettings& settings, double projectRate);
    void OnSequence(wxCommandEvent& evt);
    void OnDuration(wxCommandEvent& evt);
    void OnDutyCycle(wxCommandEvent& evt);

    // These settings exist for the lifetime of the validator
    DtmfSettings mSettings;

    wxTextCtrl* mDtmfSequenceT;
    wxSlider* mDtmfDutyCycleS;
    NumericTextCtrl* mDtmfDurationT;
    wxStaticText* mDtmfToneT;
    wxStaticText* mDtmfSilenceT;
    wxStaticText* mDtmfDutyT;
};

void EffectDtmf::Editor::PopulateOrExchange(ShuttleGui& S,
                                            const EffectSettings& settings, double projectRate)
{
    // Reference to our copy of this effect's special settings
    auto& dtmfSettings = mSettings;

    // Do NOT hold a reference to EffectSettings, just use it to find initial
    // duration values.  (It came from EffectSettingsAccess so its stable address
    // can't be relied on.)

    // dialog will be passed values from effect
    // Effect retrieves values from saved config
    // Dialog will take care of using them to initialize controls
    // If there is a selection, use that duration, otherwise use
    // value from saved config: this is useful is user wants to
    // replace selection with dtmf sequence

    S.AddSpace(0, 5);
    S.StartMultiColumn(2, wxCENTER);
    {
        mDtmfSequenceT
            =S
              .Validator([&dtmfSettings]{
            wxTextValidator vldDtmf(
                wxFILTER_INCLUDE_CHAR_LIST, &dtmfSettings.dtmfSequence);
            wxArrayString symbols;
            symbols.Alloc(DtmfBase::kSymbols.size());
            for (auto c : DtmfBase::kSymbols) {
                symbols.Add(wxString::Format(wxT("%c"), c));
            }
            vldDtmf.SetIncludes(symbols);
            return vldDtmf;
        })
              .AddTextBox(XXO("DTMF &sequence:"), wxT(""), 10);
        BindTo(*mDtmfSequenceT, wxEVT_TEXT, &Editor::OnSequence);

        // A control with no event handler but the validator causes updates
        // when TransferData functions are called
        S
        .Validator<FloatingPointValidator<double> >(
            3, &dtmfSettings.dtmfAmplitude, NumValidatorStyle::NO_TRAILING_ZEROES,
            Amplitude.min, Amplitude.max)
        .AddTextBox(XXO("&Amplitude (0-1):"), wxT(""), 10);

        S.AddPrompt(XXO("&Duration:"));
        auto& extra = settings.extra;
        mDtmfDurationT = safenew
                             NumericTextCtrl(FormatterContext::SampleRateContext(projectRate),
                                             S.GetParent(), wxID_ANY,
                                             NumericConverterType_TIME(),
                                             extra.GetDurationFormat(),
                                             extra.GetDuration(),
                                             NumericTextCtrl::Options{}
                                             .AutoPos(true));
        S.Name(XO("Duration"))
        .AddWindow(mDtmfDurationT);
        BindTo(*mDtmfDurationT, wxEVT_TEXT, &Editor::OnDuration);

        S.AddFixedText(XO("&Tone/silence ratio:"), false);
        mDtmfDutyCycleS
            =S
              .Style(wxSL_HORIZONTAL | wxEXPAND)
              .MinSize({ -1, -1 })
              .AddSlider({},
                         dtmfSettings.dtmfDutyCycle * DutyCycle.scale,
                         DutyCycle.max * DutyCycle.scale,
                         DutyCycle.min * DutyCycle.scale);
        BindTo(*mDtmfDutyCycleS, wxEVT_SLIDER, &Editor::OnDutyCycle);
    }
    S.EndMultiColumn();

    S.StartMultiColumn(2, wxCENTER);
    {
        S.AddFixedText(XO("Duty cycle:"), false);
        mDtmfDutyT
            =S.AddVariableText(XO("%.1f %%")
                               .Format(dtmfSettings.dtmfDutyCycle), false);

        S.AddFixedText(XO("Tone duration:"), false);
        mDtmfSilenceT
            =/* i18n-hint milliseconds */
              S.AddVariableText(XO("%.0f ms")
                                .Format(dtmfSettings.dtmfTone * 1000.0), false);

        S.AddFixedText(XO("Silence duration:"), false);
        mDtmfToneT
            =/* i18n-hint milliseconds */
              S.AddVariableText(XO("%0.f ms")
                                .Format(dtmfSettings.dtmfSilence * 1000.0), false);
    }
    S.EndMultiColumn();
}

// Effect implementation

std::unique_ptr<EffectEditor> EffectDtmf::MakeEditor(
    ShuttleGui& S, EffectInstance&, EffectSettingsAccess& access,
    const EffectOutputs*) const
{
    auto& settings = access.Get();
    auto& dtmfSettings = GetSettings(settings);
    auto result = std::make_unique<Editor>(*this, access, dtmfSettings);
    result->PopulateOrExchange(S, settings, mProjectRate);
    return result;
}

bool EffectDtmf::Editor::UpdateUI()
{
    const auto& settings = mAccess.Get();
    auto& dtmfSettings = mSettings;

    // Copy into our settings
    mSettings = GetSettings(settings);

    mDtmfDutyCycleS->SetValue(dtmfSettings.dtmfDutyCycle * DutyCycle.scale);

    mDtmfDurationT->SetValue(settings.extra.GetDuration());

    DoUpdateUI();

    return true;
}

bool EffectDtmf::Editor::ValidateUI()
{
    mAccess.ModifySettings([this](EffectSettings& settings){
        auto& dtmfSettings = mSettings;
        dtmfSettings.dtmfDutyCycle
            =(double)mDtmfDutyCycleS->GetValue() / DutyCycle.scale;
        settings.extra.SetDuration(mDtmfDurationT->GetValue());

        // recalculate to make sure all values are up-to-date. This is especially
        // important if the user did not change any values in the dialog
        dtmfSettings.Recalculate(settings);
        return nullptr;
    });

    return true;
}

void EffectDtmf::Editor::DoUpdateUI()
{
    // Update some texts in response to controls
    auto& dtmfSettings = mSettings;

    mDtmfDutyT
    ->SetLabel(wxString::Format(wxT("%.1f %%"), dtmfSettings.dtmfDutyCycle));
    mDtmfDutyT->SetName(mDtmfDutyT->GetLabel()); // fix for bug 577 (NVDA/Narrator screen readers do not read static text in dialogs)

    mDtmfSilenceT
    ->SetLabel(wxString::Format(_("%.0f ms"), dtmfSettings.dtmfTone * 1000.0));
    mDtmfSilenceT->SetName(mDtmfSilenceT->GetLabel()); // fix for bug 577 (NVDA/Narrator screen readers do not read static text in dialogs)

    mDtmfToneT
    ->SetLabel(wxString::Format(_("%.0f ms"), dtmfSettings.dtmfSilence * 1000.0));
    mDtmfToneT->SetName(mDtmfToneT->GetLabel()); // fix for bug 577 (NVDA/Narrator screen readers do not read static text in dialogs)
}

void EffectDtmf::Editor::OnSequence(wxCommandEvent& WXUNUSED(evt))
{
    mAccess.ModifySettings([this](EffectSettings& settings){
        auto& dtmfSettings = mSettings;
        dtmfSettings.dtmfSequence = mDtmfSequenceT->GetValue();
        dtmfSettings.Recalculate(settings);
        return nullptr;
    });
    DoUpdateUI();
}

void EffectDtmf::Editor::OnDuration(wxCommandEvent& WXUNUSED(evt))
{
    mAccess.ModifySettings([this](EffectSettings& settings){
        auto& dtmfSettings = mSettings;
        settings.extra.SetDuration(mDtmfDurationT->GetValue());
        dtmfSettings.Recalculate(settings);
        return nullptr;
    });
    DoUpdateUI();
}

void EffectDtmf::Editor::OnDutyCycle(wxCommandEvent& evt)
{
    mAccess.ModifySettings([this, &evt](EffectSettings& settings){
        auto& dtmfSettings = mSettings;
        dtmfSettings.dtmfDutyCycle = (double)evt.GetInt() / DutyCycle.scale;
        dtmfSettings.Recalculate(settings);
        return nullptr;
    });
    DoUpdateUI();
}
