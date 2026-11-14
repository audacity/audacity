/**********************************************************************

  Audacity: A Digital Audio Editor

  Normalize.cpp

  Dominic Mazzoni
  Vaughan Johnson (Preview)

*******************************************************************//**

\class NormalizeBase
\brief An Effect to bring the peak level up to a chosen level.

*//*******************************************************************/
#include "Normalize.h"
#include "EffectEditor.h"
#include "LoadEffects.h"

#include <wx/checkbox.h>
#include <wx/stattext.h>
#include <wx/valgen.h>

#include "Prefs.h"
#include "ShuttleGui.h"
#include "../widgets/valnum.h"
#include "ProgressDialog.h"

namespace {
BuiltinEffectsModule::Registration< EffectNormalize > reg;
}

BEGIN_EVENT_TABLE(EffectNormalize, wxEvtHandler)
EVT_CHECKBOX(wxID_ANY, EffectNormalize::OnUpdateUI)
EVT_TEXT(wxID_ANY, EffectNormalize::OnUpdateUI)
END_EVENT_TABLE()

std::unique_ptr<EffectEditor> EffectNormalize::PopulateOrExchange(
    ShuttleGui& S, EffectInstance&, EffectSettingsAccess&,
    const EffectOutputs*)
{
    mUIParent = S.GetParent();
    mCreating = true;

    S.StartVerticalLay(0);
    {
        S.StartMultiColumn(2, wxALIGN_CENTER);
        {
            S.StartVerticalLay(false);
            {
                mDCCheckBox = S.Validator<wxGenericValidator>(&mDC)
                              .AddCheckBox(XXO("&Remove DC offset (center on 0.0 vertically)"),
                                           mDC);

                S.StartHorizontalLay(wxALIGN_LEFT, false);
                {
                    mGainCheckBox = S
                                    .MinSize()
                                    .Validator<wxGenericValidator>(&mGain)
                                    .AddCheckBox(XXO("&Normalize peak amplitude to   "),
                                                 mGain);

                    mLevelTextCtrl = S
                                     .Name(XO("Peak amplitude dB"))
                                     .Validator<FloatingPointValidator<double> >(
                        2,
                        &mPeakLevel,
                        NumValidatorStyle::ONE_TRAILING_ZERO,
                        PeakLevel.min,
                        PeakLevel.max)
                                     .AddTextBox({}, L"", 10);
                    mLeveldB = S.AddVariableText(XO("dB"), false,
                                                 wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT);
                    mWarning = S.AddVariableText({}, false,
                                                 wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT);
                }
                S.EndHorizontalLay();

                mStereoIndCheckBox = S
                                     .Validator<wxGenericValidator>(&mStereoInd)
                                     .AddCheckBox(XXO("N&ormalize stereo channels independently"),
                                                  mStereoInd);
            }
            S.EndVerticalLay();
        }
        S.EndMultiColumn();
    }
    S.EndVerticalLay();
    mCreating = false;
    return nullptr;
}

bool EffectNormalize::TransferDataToWindow(const EffectSettings&)
{
    if (!mUIParent->TransferDataToWindow()) {
        return false;
    }

    UpdateUI();

    return true;
}

bool EffectNormalize::TransferDataFromWindow(EffectSettings&)
{
    if (!mUIParent->Validate() || !mUIParent->TransferDataFromWindow()) {
        return false;
    }

    return true;
}

// NormalizeBase implementation

void EffectNormalize::OnUpdateUI(wxCommandEvent& WXUNUSED(evt))
{
    UpdateUI();
}

void EffectNormalize::UpdateUI()
{
    if (!mUIParent->TransferDataFromWindow()) {
        mWarning->SetLabel(_("(Maximum 0dB)"));
        EffectEditor::EnableApply(mUIParent, false);
        return;
    }
    mWarning->SetLabel(wxT(""));

    // Disallow level stuff if not normalizing
    mLevelTextCtrl->Enable(mGain);
    mLeveldB->Enable(mGain);
    mStereoIndCheckBox->Enable(mGain);

    // Disallow OK/Preview if doing nothing
    EffectEditor::EnableApply(mUIParent, mGain || mDC);
}
