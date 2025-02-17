/**********************************************************************

  Audacity: A Digital Audio Editor

  NoiseReduction.cpp

  Dominic Mazzoni

  detailed rewriting by
  Paul Licameli

*******************************************************************//**
*/

#include "NoiseReduction.h"

#include "LoadEffects.h"
#include "EffectManager.h"
#include "EffectPreview.h"
#include "EffectUI.h"

#include "ShuttleGui.h"
#include "HelpSystem.h"

#include "AudacityMessageBox.h"
#include "../widgets/valnum.h"

#if defined(__WXMSW__) && !defined(__CYGWIN__)
#include <float.h>
#define finite(x) _finite(x)
#endif

#include <wx/button.h>
#include <wx/choice.h>
#include <wx/radiobut.h>
#include <wx/slider.h>
#include <wx/valtext.h>
#include <wx/textctrl.h>

/****************************************************************//**

\class EffectNoiseReduction::Dialog
\brief Dialog used with EffectNoiseReduction

**//*****************************************************************/

//----------------------------------------------------------------------------
// EffectNoiseReduction::Dialog
//----------------------------------------------------------------------------

class EffectNoiseReduction::Dialog final : public EffectDialog
{
public:
    // constructors and destructors
    Dialog(EffectNoiseReduction* effect, EffectSettingsAccess& access, Settings* settings, wxWindow* parent, bool bHasProfile,
           bool bAllowTwiddleSettings);

    void PopulateOrExchange(ShuttleGui& S) override;
    bool TransferDataToWindow() override;
    bool TransferDataFromWindow() override;

    const Settings& GetTempSettings() const
    { return mTempSettings; }

private:
    void DisableControlsIfIsolating();

#ifdef ADVANCED_SETTINGS
    void EnableDisableSensitivityControls();
#endif

    // handlers
    void OnGetProfile(wxCommandEvent& event);
    void OnNoiseReductionChoice(wxCommandEvent& event);
#ifdef ADVANCED_SETTINGS
    void OnMethodChoice(wxCommandEvent&);
#endif
    void OnPreview(wxCommandEvent& event) override;
    void OnReduceNoise(wxCommandEvent& event);
    void OnCancel(wxCommandEvent& event);
    void OnHelp(wxCommandEvent& event);

    void OnText(wxCommandEvent& event);
    void OnSlider(wxCommandEvent& event);

    // data members

    EffectNoiseReduction* m_pEffect;
    //! This dialog is modal, so mAccess will live long enough for it
    EffectSettingsAccess& mAccess;
    EffectNoiseReduction::Settings* m_pSettings;
    EffectNoiseReduction::Settings mTempSettings;

    bool mbHasProfile;
    bool mbAllowTwiddleSettings;

    wxRadioButton* mKeepSignal;
#ifdef ISOLATE_CHOICE
    wxRadioButton* mKeepNoise;
#endif
#ifdef RESIDUE_CHOICE
    wxRadioButton* mResidue;
#endif

private:
    DECLARE_EVENT_TABLE()
};

namespace {
BuiltinEffectsModule::Registration< EffectNoiseReduction > reg;
}

namespace {
int PromptUser(
    EffectNoiseReduction::Settings& settings, EffectNoiseReduction* effect,
    EffectSettingsAccess& access, wxWindow& parent, bool bHasProfile,
    bool bAllowTwiddleSettings)
{
    EffectNoiseReduction::Dialog dlog(
        effect, access, &settings, &parent, bHasProfile, bAllowTwiddleSettings);

    dlog.CentreOnParent();
    dlog.ShowModal();

    const auto returnCode = dlog.GetReturnCode();
    if (!returnCode) {
        return 0;
    }

    settings = dlog.GetTempSettings();
    settings.mDoProfile = (returnCode == 1);

    if (!settings.PrefsIO(false)) {
        return 0;
    }
    return returnCode;
}
} // namespace

//! An override still here for historical reasons, ignoring the factory
//! and the access
/*! We would like to make this effect behave more like others, but it does have
 its unusual two-pass nature.  First choose and analyze an example of noise,
 then apply noise reduction to another selection.  That is difficult to fit into
 the framework for managing settings of other effects. */
int EffectNoiseReduction::ShowHostInterface(EffectBase&,
                                            wxWindow& parent, const EffectDialogFactory&,
                                            std::shared_ptr<EffectInstance>& pInstance, EffectSettingsAccess& access,
                                            bool forceModal)
{
    // Assign the out parameter
    pInstance = MakeInstance();

    // to do: use forceModal correctly

    // Doesn't use the factory but substitutes its own dialog

    // We may want to twiddle the levels if we are setting
    // from a macro editing dialog
    return PromptUser(*mSettings, this, access, parent,
                      bool(mStatistics), IsBatchProcessing());
}

//----------------------------------------------------------------------------
// EffectNoiseReduction::Dialog
//----------------------------------------------------------------------------

enum {
    ID_BUTTON_GETPROFILE = 10001,
    ID_RADIOBUTTON_KEEPSIGNAL,
#ifdef ISOLATE_CHOICE
    ID_RADIOBUTTON_KEEPNOISE,
#endif
#ifdef RESIDUE_CHOICE
    ID_RADIOBUTTON_RESIDUE,
#endif

#ifdef ADVANCED_SETTINGS
    ID_CHOICE_METHOD,
#endif

    // Slider/text pairs
    ID_GAIN_SLIDER,
    ID_GAIN_TEXT,

    ID_NEW_SENSITIVITY_SLIDER,
    ID_NEW_SENSITIVITY_TEXT,

#ifdef ATTACK_AND_RELEASE
    ID_ATTACK_TIME_SLIDER,
    ID_ATTACK_TIME_TEXT,

    ID_RELEASE_TIME_SLIDER,
    ID_RELEASE_TIME_TEXT,
#endif

    ID_FREQ_SLIDER,
    ID_FREQ_TEXT,

    END_OF_BASIC_SLIDERS,

#ifdef ADVANCED_SETTINGS
    ID_OLD_SENSITIVITY_SLIDER = END_OF_BASIC_SLIDERS,
    ID_OLD_SENSITIVITY_TEXT,

    END_OF_ADVANCED_SLIDERS,
    END_OF_SLIDERS = END_OF_ADVANCED_SLIDERS,
#else
    END_OF_SLIDERS = END_OF_BASIC_SLIDERS,
#endif

    FIRST_SLIDER = ID_GAIN_SLIDER,
};

namespace {
struct ControlInfo {
    typedef double (EffectNoiseReduction::Settings::* MemberPointer);

    double Value(long sliderSetting) const
    {
        return
            valueMin
            + (double(sliderSetting) / sliderMax) * (valueMax - valueMin);
    }

    long SliderSetting(double value) const
    {
        return std::clamp<long>(
            0.5 + sliderMax * (value - valueMin) / (valueMax - valueMin),
            0, sliderMax);
    }

    wxString Text(double value) const
    {
        if (formatAsInt) {
            return wxString::Format(format, (int)(value));
        } else {
            return wxString::Format(format, value);
        }
    }

    void CreateControls(int id, ShuttleGui& S) const
    {
        wxTextCtrl* const text = S.Id(id + 1)
                                 .Validator<FloatingPointValidator<double> >(
            formatAsInt ? 0 : 2,
            nullptr,
            NumValidatorStyle::DEFAULT,
            valueMin, valueMax
            )
                                 .AddTextBox(textBoxCaption, wxT(""), 0);

        wxSlider* const slider
            =S.Id(id)
              .Name(sliderName)
              .Style(wxSL_HORIZONTAL)
              .MinSize({ 150, -1 })
              .AddSlider({}, 0, sliderMax);
    }

    MemberPointer field;
    double valueMin;
    double valueMax;
    long sliderMax;
    // (valueMin - valueMax) / sliderMax is the value increment of the slider
    const wxChar* format;
    bool formatAsInt;
    const TranslatableString textBoxCaption;
    const TranslatableString sliderName;

    ControlInfo(MemberPointer f, double vMin, double vMax, long sMax, const wxChar* fmt, bool fAsInt,
                const TranslatableString& caption, const TranslatableString& name)
        : field(f), valueMin(vMin), valueMax(vMax), sliderMax(sMax), format(fmt), formatAsInt(fAsInt)
        , textBoxCaption(caption), sliderName(name)
    {
    }
};

const ControlInfo* controlInfo()
{
    static const ControlInfo table[] = {
        ControlInfo(&EffectNoiseReduction::Settings::mNoiseGain,
                    0.0, 48.0, 48, wxT("%d"), true,
                    XXO("&Noise reduction (dB):"), XO("Noise reduction")),
        ControlInfo(&EffectNoiseReduction::Settings::mNewSensitivity,
                    0.01, 24.0, 48, wxT("%.2f"), false,
                    XXO("&Sensitivity:"), XO("Sensitivity")),
#ifdef ATTACK_AND_RELEASE
        ControlInfo(&EffectNoiseReduction::Settings::mAttackTime,
                    0, 1.0, 100, wxT("%.2f"), false,
                    XXO("Attac&k time (secs):"), XO("Attack time")),
        ControlInfo(&EffectNoiseReduction::Settings::mReleaseTime,
                    0, 1.0, 100, wxT("%.2f"), false,
                    XXO("R&elease time (secs):"), XO("Release time")),
#endif
        ControlInfo(&EffectNoiseReduction::Settings::mFreqSmoothingBands,
                    0, 12, 12, wxT("%d"), true,
                    XXO("&Frequency smoothing (bands):"), XO("Frequency smoothing")),

#ifdef ADVANCED_SETTINGS
        ControlInfo(&EffectNoiseReduction::Settings::mOldSensitivity,
                    -20.0, 20.0, 4000, wxT("%.2f"), false,
                    XXO("Sensiti&vity (dB):"), XO("Old Sensitivity")),
        // add here
#endif
    };

    return table;
}
} // namespace

BEGIN_EVENT_TABLE(EffectNoiseReduction::Dialog, wxDialogWrapper)
EVT_BUTTON(wxID_OK, EffectNoiseReduction::Dialog::OnReduceNoise)
EVT_BUTTON(wxID_CANCEL, EffectNoiseReduction::Dialog::OnCancel)
EVT_BUTTON(ID_EFFECT_PREVIEW, EffectNoiseReduction::Dialog::OnPreview)
EVT_BUTTON(ID_BUTTON_GETPROFILE, EffectNoiseReduction::Dialog::OnGetProfile)
EVT_BUTTON(wxID_HELP, EffectNoiseReduction::Dialog::OnHelp)

EVT_RADIOBUTTON(ID_RADIOBUTTON_KEEPSIGNAL, EffectNoiseReduction::Dialog::OnNoiseReductionChoice)
#ifdef ISOLATE_CHOICE
EVT_RADIOBUTTON(ID_RADIOBUTTON_KEEPNOISE, EffectNoiseReduction::Dialog::OnNoiseReductionChoice)
#endif
#ifdef RESIDUE_CHOICE
EVT_RADIOBUTTON(ID_RADIOBUTTON_RESIDUE, EffectNoiseReduction::Dialog::OnNoiseReductionChoice)
#endif

#ifdef ADVANCED_SETTINGS
EVT_CHOICE(ID_CHOICE_METHOD, EffectNoiseReduction::Dialog::OnMethodChoice)
#endif

EVT_SLIDER(ID_GAIN_SLIDER, EffectNoiseReduction::Dialog::OnSlider)
EVT_TEXT(ID_GAIN_TEXT, EffectNoiseReduction::Dialog::OnText)

EVT_SLIDER(ID_NEW_SENSITIVITY_SLIDER, EffectNoiseReduction::Dialog::OnSlider)
EVT_TEXT(ID_NEW_SENSITIVITY_TEXT, EffectNoiseReduction::Dialog::OnText)

EVT_SLIDER(ID_FREQ_SLIDER, EffectNoiseReduction::Dialog::OnSlider)
EVT_TEXT(ID_FREQ_TEXT, EffectNoiseReduction::Dialog::OnText)

#ifdef ATTACK_AND_RELEASE
EVT_SLIDER(ID_ATTACK_TIME_SLIDER, EffectNoiseReduction::Dialog::OnSlider)
EVT_TEXT(ID_ATTACK_TIME_TEXT, EffectNoiseReduction::Dialog::OnText)

EVT_SLIDER(ID_RELEASE_TIME_SLIDER, EffectNoiseReduction::Dialog::OnSlider)
EVT_TEXT(ID_RELEASE_TIME_TEXT, EffectNoiseReduction::Dialog::OnText)
#endif

#ifdef ADVANCED_SETTINGS
EVT_SLIDER(ID_OLD_SENSITIVITY_SLIDER, EffectNoiseReduction::Dialog::OnSlider)
EVT_TEXT(ID_OLD_SENSITIVITY_TEXT, EffectNoiseReduction::Dialog::OnText)
#endif
END_EVENT_TABLE()

EffectNoiseReduction::Dialog::Dialog(EffectNoiseReduction* effect,
                                     EffectSettingsAccess& access,
                                     EffectNoiseReduction::Settings* settings,
                                     wxWindow* parent, bool bHasProfile, bool bAllowTwiddleSettings)
    : EffectDialog(parent, XO("Noise Reduction"), EffectTypeProcess, wxDEFAULT_DIALOG_STYLE, eHelpButton)
    , m_pEffect(effect)
    , mAccess{access}
    , m_pSettings(settings) // point to
    , mTempSettings(*settings) // copy
    , mbHasProfile(bHasProfile)
    , mbAllowTwiddleSettings(bAllowTwiddleSettings)
    // NULL out the control members until the controls are created.
    , mKeepSignal(NULL)
#ifdef ISOLATE_CHOICE
    , mKeepNoise(NULL)
#endif
#ifdef RESIDUE_CHOICE
    , mResidue(NULL)
#endif
{
    EffectDialog::Init();

    wxButton* const pButtonPreview
        =(wxButton*)wxWindow::FindWindowById(ID_EFFECT_PREVIEW, this);
    wxButton* const pButtonReduceNoise
        =(wxButton*)wxWindow::FindWindowById(wxID_OK, this);

    if (mbHasProfile || mbAllowTwiddleSettings) {
        pButtonPreview->Enable(!mbAllowTwiddleSettings);
        pButtonReduceNoise->SetFocus();
    } else {
        pButtonPreview->Enable(false);
        pButtonReduceNoise->Enable(false);
    }
}

void EffectNoiseReduction::Dialog::DisableControlsIfIsolating()
{
    // If Isolate is chosen, disable controls that define
    // "what to do with noise" rather than "what is noise."
    // Else, enable them.
    // This does NOT include sensitivity, NEW or old, nor
    // the choice of window functions, size, or step.
    // The method choice is not included, because it affects
    // which sensitivity slider is operative, and that is part
    // of what defines noise.

    static const int toDisable[] = {
        ID_GAIN_SLIDER,
        ID_GAIN_TEXT,

        ID_FREQ_SLIDER,
        ID_FREQ_TEXT,

#ifdef ATTACK_AND_RELEASE
        ID_ATTACK_TIME_SLIDER,
        ID_ATTACK_TIME_TEXT,

        ID_RELEASE_TIME_SLIDER,
        ID_RELEASE_TIME_TEXT,
#endif
    };
    static const auto nToDisable = sizeof(toDisable) / sizeof(toDisable[0]);

    bool bIsolating =
#ifdef ISOLATE_CHOICE
        mKeepNoise->GetValue();
#else
        false;
#endif
    for (auto ii = nToDisable; ii--;) {
        wxWindow::FindWindowById(toDisable[ii], this)->Enable(!bIsolating);
    }
}

#ifdef ADVANCED_SETTINGS
void EffectNoiseReduction::Dialog::EnableDisableSensitivityControls()
{
    wxChoice* const pChoice
        =static_cast<wxChoice*>(wxWindow::FindWindowById(ID_CHOICE_METHOD, this));
    const bool bOldMethod
        =pChoice->GetSelection() == DM_OLD_METHOD;
    wxWindow::FindWindowById(ID_OLD_SENSITIVITY_SLIDER, this)->Enable(bOldMethod);
    wxWindow::FindWindowById(ID_OLD_SENSITIVITY_TEXT, this)->Enable(bOldMethod);
    wxWindow::FindWindowById(ID_NEW_SENSITIVITY_SLIDER, this)->Enable(!bOldMethod);
    wxWindow::FindWindowById(ID_NEW_SENSITIVITY_TEXT, this)->Enable(!bOldMethod);
}

#endif

void EffectNoiseReduction::Dialog::OnGetProfile(wxCommandEvent& WXUNUSED(event))
{
    // Project has not be changed so skip pushing state
    EffectManager::Get().SetSkipStateFlag(true);

    if (!TransferDataFromWindow()) {
        return;
    }

    // Return code distinguishes this first step from the actual effect
    EndModal(1);
}

// This handles the whole radio group
void EffectNoiseReduction::Dialog::OnNoiseReductionChoice(wxCommandEvent& WXUNUSED(event))
{
    if (mKeepSignal->GetValue()) {
        mTempSettings.mNoiseReductionChoice = NRC_REDUCE_NOISE;
    }
#ifdef ISOLATE_CHOICE
    else if (mKeepNoise->GetValue()) {
        mTempSettings.mNoiseReductionChoice = NRC_ISOLATE_NOISE;
    }
#endif
#ifdef RESIDUE_CHOICE
    else {
        mTempSettings.mNoiseReductionChoice = NRC_LEAVE_RESIDUE;
    }
#endif
    DisableControlsIfIsolating();
}

#ifdef ADVANCED_SETTINGS
void EffectNoiseReduction::Dialog::OnMethodChoice(wxCommandEvent&)
{
    EnableDisableSensitivityControls();
}

#endif

void EffectNoiseReduction::Dialog::OnPreview(wxCommandEvent& WXUNUSED(event))
{
    if (!TransferDataFromWindow()) {
        return;
    }

    // Save & restore parameters around Preview, because we didn't do OK.
    auto cleanup = valueRestorer(*m_pSettings);
    *m_pSettings = mTempSettings;
    m_pSettings->mDoProfile = false;

    EffectPreview(*m_pEffect, mAccess,
                  // Don't need any UI updates for preview
                  {},
                  false);
}

void EffectNoiseReduction::Dialog::OnReduceNoise(wxCommandEvent& WXUNUSED(event))
{
    if (!TransferDataFromWindow()) {
        return;
    }

    EndModal(2);
}

void EffectNoiseReduction::Dialog::OnCancel(wxCommandEvent& WXUNUSED(event))
{
    EndModal(0);
}

void EffectNoiseReduction::Dialog::OnHelp(wxCommandEvent& WXUNUSED(event))
{
    HelpSystem::ShowHelp(this, "Noise_Reduction", true);
}

void EffectNoiseReduction::Dialog::PopulateOrExchange(ShuttleGui& S)
{
    S.StartStatic(XO("Step 1"));
    {
        S.AddVariableText(XO(
                              "Select a few seconds of just noise so Audacity knows what to filter out,\nthen click Get Noise Profile:"));
        //m_pButton_GetProfile =
        S.Id(ID_BUTTON_GETPROFILE).AddButton(XXO("&Get Noise Profile"));
    }
    S.EndStatic();

    S.StartStatic(XO("Step 2"));
    {
        S.AddVariableText(XO(
                              "Select all of the audio you want filtered, choose how much noise you want\nfiltered out, and then click 'OK' to reduce noise.\n"));

        S.StartMultiColumn(3, wxEXPAND);
        S.SetStretchyCol(2);
        {
            for (int id = FIRST_SLIDER; id < END_OF_BASIC_SLIDERS; id += 2) {
                const ControlInfo& info = controlInfo()[(id - FIRST_SLIDER) / 2];
                info.CreateControls(id, S);
            }
        }
        S.EndMultiColumn();

        S.StartMultiColumn(
            2
#ifdef RESIDUE_CHOICE
            + 1
#endif
#ifdef ISOLATE_CHOICE
            + 1
#endif
            ,
            wxALIGN_CENTER_HORIZONTAL);
        {
            S.AddPrompt(XXO("Noise:"));
            mKeepSignal = S.Id(ID_RADIOBUTTON_KEEPSIGNAL)
                          /* i18n-hint: Translate differently from "Residue" ! */
                          .AddRadioButton(XXO("Re&duce"));
#ifdef ISOLATE_CHOICE
            mKeepNoise = S.Id(ID_RADIOBUTTON_KEEPNOISE)
                         .AddRadioButtonToGroup(XXO("&Isolate"));
#endif
#ifdef RESIDUE_CHOICE
            mResidue = S.Id(ID_RADIOBUTTON_RESIDUE)
                       /* i18n-hint: Means the difference between effect and original sound.  Translate differently from "Reduce" ! */
                       .AddRadioButtonToGroup(XXO("Resid&ue"));
#endif
        }
        S.EndMultiColumn();
    }
    S.EndStatic();

#ifdef ADVANCED_SETTINGS
    S.StartStatic(XO("Advanced Settings"));
    {
        S.StartMultiColumn(2);
        {
            S.TieChoice(XXO("&Window types:"),
                        mTempSettings.mWindowTypes,
                        []{
                TranslatableStrings windowTypeChoices;
                for (size_t ii = 0; ii < WT_N_WINDOW_TYPES; ++ii) {
                    windowTypeChoices.push_back(windowTypesInfo[ii].name);
                }
                return windowTypeChoices;
            }()
                        );

            S.TieChoice(XXO("Window si&ze:"),
                        mTempSettings.mWindowSizeChoice,
            {
                XO("8"),
                XO("16"),
                XO("32"),
                XO("64"),
                XO("128"),
                XO("256"),
                XO("512"),
                XO("1024"),
                XO("2048 (default)"),
                XO("4096"),
                XO("8192"),
                XO("16384"),
            }
                        );

            S.TieChoice(XXO("S&teps per window:"),
                        mTempSettings.mStepsPerWindowChoice,
            {
                XO("2"),
                XO("4 (default)"),
                XO("8"),
                XO("16"),
                XO("32"),
                XO("64"),
            }
                        );

            S.Id(ID_CHOICE_METHOD)
            .TieChoice(XXO("Discrimination &method:"),
                       mTempSettings.mMethod,
                       []{
                TranslatableStrings methodChoices;
                auto nn = DM_N_METHODS;
#ifndef OLD_METHOD_AVAILABLE
                --nn;
#endif
                for (auto ii = 0; ii < nn; ++ii) {
                    methodChoices.push_back(discriminationMethodInfo[ii].name);
                }
                return methodChoices;
            }());
        }
        S.EndMultiColumn();

        S.StartMultiColumn(3, wxEXPAND);
        S.SetStretchyCol(2);
        {
            for (int id = END_OF_BASIC_SLIDERS; id < END_OF_ADVANCED_SLIDERS; id += 2) {
                const ControlInfo& info = controlInfo()[(id - FIRST_SLIDER) / 2];
                info.CreateControls(id, S);
            }
        }
        S.EndMultiColumn();
    }
    S.EndStatic();
#endif
}

bool EffectNoiseReduction::Dialog::TransferDataToWindow()
{
    // Do the choice controls:
    if (!EffectDialog::TransferDataToWindow()) {
        return false;
    }

    for (int id = FIRST_SLIDER; id < END_OF_SLIDERS; id += 2) {
        wxSlider* slider
            =static_cast<wxSlider*>(wxWindow::FindWindowById(id, this));
        wxTextCtrl* text
            =static_cast<wxTextCtrl*>(wxWindow::FindWindowById(id + 1, this));
        const ControlInfo& info = controlInfo()[(id - FIRST_SLIDER) / 2];
        const double field = mTempSettings.*(info.field);
        text->SetValue(info.Text(field));
        slider->SetValue(info.SliderSetting(field));
    }

    mKeepSignal->SetValue(mTempSettings.mNoiseReductionChoice == NRC_REDUCE_NOISE);
#ifdef ISOLATE_CHOICE
    mKeepNoise->SetValue(mTempSettings.mNoiseReductionChoice == NRC_ISOLATE_NOISE);
#endif
#ifdef RESIDUE_CHOICE
    mResidue->SetValue(mTempSettings.mNoiseReductionChoice == NRC_LEAVE_RESIDUE);
#endif

    // Set the enabled states of controls
    DisableControlsIfIsolating();
#ifdef ADVANCED_SETTINGS
    EnableDisableSensitivityControls();
#endif

    return true;
}

bool EffectNoiseReduction::Dialog::TransferDataFromWindow()
{
    if (!wxWindow::Validate()) {
        return false;
    }
    // Do the choice controls:
    if (!EffectDialog::TransferDataFromWindow()) {
        return false;
    }

    wxCommandEvent dummy;
    OnNoiseReductionChoice(dummy);

    return mTempSettings.Validate(m_pEffect);
}

void EffectNoiseReduction::Dialog::OnText(wxCommandEvent& event)
{
    int id = event.GetId();
    int idx = (id - FIRST_SLIDER - 1) / 2;
    const ControlInfo& info = controlInfo()[idx];
    wxTextCtrl* text
        =static_cast<wxTextCtrl*>(wxWindow::FindWindowById(id, this));
    wxSlider* slider
        =static_cast<wxSlider*>(wxWindow::FindWindowById(id - 1, this));
    double& field = mTempSettings.*(info.field);

    text->GetValue().ToDouble(&field);
    slider->SetValue(info.SliderSetting(field));
}

void EffectNoiseReduction::Dialog::OnSlider(wxCommandEvent& event)
{
    int id = event.GetId();
    int idx = (id - FIRST_SLIDER) / 2;
    const ControlInfo& info = controlInfo()[idx];
    wxSlider* slider
        =static_cast<wxSlider*>(wxWindow::FindWindowById(id, this));
    wxTextCtrl* text
        =static_cast<wxTextCtrl*>(wxWindow::FindWindowById(id + 1, this));
    double& field = mTempSettings.*(info.field);

    field = info.Value(slider->GetValue());
    text->SetValue(info.Text(field));
}
