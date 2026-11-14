/**********************************************************************

  Audacity: A Digital Audio Editor

  Loudness.cpp

  Max Maisel

***********************************************************************/
#include "Loudness.h"
#include "EffectEditor.h"

#include <wx/simplebook.h>
#include <wx/valgen.h>

#include "Internat.h"
#include "Prefs.h"
#include "ShuttleGui.h"
#include "../widgets/valnum.h"
#include "ProgressDialog.h"

#include "LoadEffects.h"

static const EnumValueSymbol kNormalizeTargetStrings[LoudnessBase::nAlgos] = {
    { XO("perceived loudness") }, { XO("RMS") }
};

BEGIN_EVENT_TABLE(EffectLoudness, wxEvtHandler)
EVT_CHOICE(wxID_ANY, EffectLoudness::OnChoice)
EVT_CHECKBOX(wxID_ANY, EffectLoudness::OnUpdateUI)
EVT_TEXT(wxID_ANY, EffectLoudness::OnUpdateUI)
END_EVENT_TABLE()

namespace {
BuiltinEffectsModule::Registration< EffectLoudness > reg;
}

std::unique_ptr<EffectEditor> EffectLoudness::PopulateOrExchange(
    ShuttleGui& S, EffectInstance&, EffectSettingsAccess&,
    const EffectOutputs*)
{
    mUIParent = S.GetParent();
    S.StartVerticalLay(0);
    {
        S.StartMultiColumn(2, wxALIGN_CENTER);
        {
            S.StartVerticalLay(false);
            {
                S.StartHorizontalLay(wxALIGN_LEFT, false);
                {
                    S.AddVariableText(XO("&Normalize"), false,
                                      wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT);

                    mChoice = S
                              .Validator<wxGenericValidator>(&mNormalizeTo)
                              .AddChoice({},
                                         Msgids(kNormalizeTargetStrings, nAlgos),
                                         mNormalizeTo);
                    S
                    .AddVariableText(XO("t&o"), false,
                                     wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT);

                    // Use a notebook so we can have two controls but show only one
                    // They target different variables with their validators
                    mBook
                        =S
                          .StartSimplebook();
                    {
                        S.StartNotebookPage({});
                        {
                            S.StartHorizontalLay(wxALIGN_LEFT, false);
                            {
                                S
                                /* i18n-hint: LUFS is a particular method for measuring loudnesss */
                                .Name(XO("Loudness LUFS"))
                                .Validator<FloatingPointValidator<double> >(
                                    2, &mLUFSLevel,
                                    NumValidatorStyle::ONE_TRAILING_ZERO,
                                    LUFSLevel.min, LUFSLevel.max)
                                .AddTextBox({}, L"", 10);

                                /* i18n-hint: LUFS is a particular method for measuring loudnesss */
                                S
                                .AddVariableText(XO("LUFS"), false,
                                                 wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT);
                            }
                            S.EndHorizontalLay();
                        }
                        S.EndNotebookPage();

                        S.StartNotebookPage({});
                        {
                            S.StartHorizontalLay(wxALIGN_LEFT, false);
                            {
                                S
                                .Name(XO("RMS dB"))
                                .Validator<FloatingPointValidator<double> >(
                                    2, &mRMSLevel,
                                    NumValidatorStyle::ONE_TRAILING_ZERO,
                                    RMSLevel.min, RMSLevel.max)
                                .AddTextBox({}, L"", 10);

                                S
                                .AddVariableText(XO("dB"), false,
                                                 wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT);
                            }
                            S.EndHorizontalLay();
                        }
                        S.EndNotebookPage();
                    }
                    S.EndSimplebook();

                    mWarning
                        =S
                          .AddVariableText({}, false,
                                           wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT);
                }
                S.EndHorizontalLay();

                mStereoIndCheckBox = S
                                     .Validator<wxGenericValidator>(&mStereoInd)
                                     .AddCheckBox(XXO("Normalize &stereo channels independently"),
                                                  mStereoInd);

                mDualMonoCheckBox = S
                                    .Validator<wxGenericValidator>(&mDualMono)
                                    .AddCheckBox(XXO("&Treat mono as dual-mono (recommended)"),
                                                 mDualMono);
            }
            S.EndVerticalLay();
        }
        S.EndMultiColumn();
    }
    S.EndVerticalLay();
    return nullptr;
}

bool EffectLoudness::TransferDataToWindow(const EffectSettings&)
{
    if (!mUIParent->TransferDataToWindow()) {
        return false;
    }

    // adjust controls which depend on mchoice
    wxCommandEvent dummy;
    OnChoice(dummy);
    return true;
}

bool EffectLoudness::TransferDataFromWindow(EffectSettings&)
{
    if (!mUIParent->Validate() || !mUIParent->TransferDataFromWindow()) {
        return false;
    }
    return true;
}

void EffectLoudness::OnChoice(wxCommandEvent& WXUNUSED(evt))
{
    mChoice->GetValidator()->TransferFromWindow();
    mBook->SetSelection(mNormalizeTo);
    UpdateUI();
    mDualMonoCheckBox->Enable(mNormalizeTo == kLoudness);
}

void EffectLoudness::OnUpdateUI(wxCommandEvent& WXUNUSED(evt))
{
    UpdateUI();
}

void EffectLoudness::UpdateUI()
{
    if (!mUIParent->TransferDataFromWindow()) {
        mWarning->SetLabel(_("(Maximum 0dB)"));
        // TODO: recalculate layout here
        EffectEditor::EnableApply(mUIParent, false);
        return;
    }
    mWarning->SetLabel(wxT(""));
    EffectEditor::EnableApply(mUIParent, true);
}
