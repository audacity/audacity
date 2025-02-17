/**********************************************************************

  Audacity: A Digital Audio Editor

  TruncSilence.cpp

  Lynn Allan (from DM's Normalize)
  Philip Van Baren (more options and boundary fixes)

**********************************************************************/
#include "TruncSilence.h"
#include "BasicUI.h"
#include "EffectEditor.h"
#include "LoadEffects.h"

#include <wx/checkbox.h>
#include <wx/choice.h>
#include <wx/valgen.h>

#include "ShuttleGui.h"
#include "../widgets/valnum.h"
#include "AudacityMessageBox.h"

namespace {
BuiltinEffectsModule::Registration< EffectTruncSilence > reg;
}

BEGIN_EVENT_TABLE(EffectTruncSilence, wxEvtHandler)
EVT_CHOICE(wxID_ANY, EffectTruncSilence::OnControlChange)
EVT_TEXT(wxID_ANY, EffectTruncSilence::OnControlChange)
END_EVENT_TABLE()

std::unique_ptr<EffectEditor> EffectTruncSilence::PopulateOrExchange(
    ShuttleGui& S, EffectInstance&, EffectSettingsAccess&,
    const EffectOutputs*)
{
    mUIParent = S.GetParent();
    wxASSERT(nActions == WXSIZEOF(kActionStrings));

    S.AddSpace(0, 5);

    S.StartStatic(XO("Detect Silence"));
    {
        S.StartMultiColumn(3, wxALIGN_CENTER_HORIZONTAL);
        {
            // Threshold
            mThresholdText = S
                             .Validator<FloatingPointValidator<double> >(
                3, &mThresholdDB, NumValidatorStyle::NO_TRAILING_ZEROES,
                Threshold.min, Threshold.max)
                             .NameSuffix(XO("db"))
                             .AddTextBox(XXO("&Threshold:"), wxT(""), 0);
            S.AddUnits(XO("dB"));

            // Ignored silence
            mInitialAllowedSilenceT = S.Validator<FloatingPointValidator<double> >(
                3, &mInitialAllowedSilence,
                NumValidatorStyle::NO_TRAILING_ZEROES,
                Minimum.min, Minimum.max)
                                      .NameSuffix(XO("seconds"))
                                      .AddTextBox(XXO("&Duration:"), wxT(""), 12);
            S.AddUnits(XO("seconds"));
        }
        S.EndMultiColumn();
    }
    S.EndStatic();

    S.StartStatic(XO("Action"));
    {
        S.StartHorizontalLay();
        {
            // Action choices
            auto actionChoices = Msgids(kActionStrings, nActions);
            mActionChoice = S
                            .Validator<wxGenericValidator>(&mActionIndex)
                            .MinSize({ -1, -1 })
                            .AddChoice({}, actionChoices);
        }
        S.EndHorizontalLay();
        S.StartMultiColumn(3, wxALIGN_CENTER_HORIZONTAL);
        {
            // Truncation / Compression factor

            mTruncLongestAllowedSilenceT = S.Validator<FloatingPointValidator<double> >(
                3, &mTruncLongestAllowedSilence,
                NumValidatorStyle::NO_TRAILING_ZEROES,
                Truncate.min, Truncate.max)
                                           .NameSuffix(XO("seconds"))
                                           .AddTextBox(XXO("Tr&uncate to:"), wxT(""), 12);
            S.AddUnits(XO("seconds"));

            mSilenceCompressPercentT = S.Validator<FloatingPointValidator<double> >(
                3, &mSilenceCompressPercent,
                NumValidatorStyle::NO_TRAILING_ZEROES,
                Compress.min, Compress.max)
                                       .NameSuffix(XO("%"))
                                       .AddTextBox(XXO("C&ompress to:"), wxT(""), 12);
            S.AddUnits(XO("%"));
        }
        S.EndMultiColumn();

        S.StartMultiColumn(2, wxALIGN_CENTER_HORIZONTAL);
        {
            mIndependent = S.AddCheckBox(XXO("Trunc&ate tracks independently"),
                                         mbIndependent);
        }
        S.EndMultiColumn();
    }
    S.EndStatic();

    UpdateUI();
    return nullptr;
}

bool EffectTruncSilence::TransferDataToWindow(const EffectSettings&)
{
    if (!mUIParent->TransferDataToWindow()) {
        return false;
    }

    return true;
}

bool EffectTruncSilence::TransferDataFromWindow(EffectSettings&)
{
    if (!mUIParent->Validate() || !mUIParent->TransferDataFromWindow()) {
        return false;
    }

    mbIndependent = mIndependent->IsChecked();

    return true;
}

void EffectTruncSilence::UpdateUI()
{
    switch (mActionIndex) {
    case kTruncate:
        mTruncLongestAllowedSilenceT->Enable(true);
        mSilenceCompressPercentT->Enable(false);
        break;
    case kCompress:
        mTruncLongestAllowedSilenceT->Enable(false);
        mSilenceCompressPercentT->Enable(true);
    }
}

void EffectTruncSilence::OnControlChange(wxCommandEvent& WXUNUSED(evt))
{
    mActionChoice->GetValidator()->TransferFromWindow();

    UpdateUI();

    if (!EffectEditor::EnableApply(
            mUIParent, mUIParent->TransferDataFromWindow())) {
        return;
    }
}
