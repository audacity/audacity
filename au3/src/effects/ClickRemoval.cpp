/**********************************************************************

  Audacity: A Digital Audio Editor

  ClickRemoval.cpp

  Craig DeForest

***********************************************************************/
#include "ClickRemoval.h"
#include "EffectEditor.h"
#include "LoadEffects.h"

#include <wx/slider.h>
#include <wx/valgen.h>

#include "ShuttleGui.h"
#include "AudacityMessageBox.h"
#include "../widgets/valnum.h"

enum
{
    ID_Thresh = 10000,
    ID_Width
};

namespace {
BuiltinEffectsModule::Registration< EffectClickRemoval > reg;
}

BEGIN_EVENT_TABLE(EffectClickRemoval, wxEvtHandler)
EVT_SLIDER(ID_Thresh, EffectClickRemoval::OnThreshSlider)
EVT_SLIDER(ID_Width, EffectClickRemoval::OnWidthSlider)
EVT_TEXT(ID_Thresh, EffectClickRemoval::OnThreshText)
EVT_TEXT(ID_Width, EffectClickRemoval::OnWidthText)
END_EVENT_TABLE()

std::unique_ptr<EffectEditor> EffectClickRemoval::PopulateOrExchange(
    ShuttleGui& S, EffectInstance&, EffectSettingsAccess&,
    const EffectOutputs*)
{
    mUIParent = S.GetParent();
    S.AddSpace(0, 5);
    S.SetBorder(10);

    S.StartMultiColumn(3, wxEXPAND);
    S.SetStretchyCol(2);
    {
        // Threshold
        mThreshT = S.Id(ID_Thresh)
                   .Validator<IntegerValidator<int> >(
            &mThresholdLevel, NumValidatorStyle::DEFAULT,
            Threshold.min, Threshold.max)
                   .AddTextBox(XXO("&Threshold (lower is more sensitive):"),
                               wxT(""),
                               10);

        mThreshS = S.Id(ID_Thresh)
                   .Name(XO("Threshold"))
                   .Style(wxSL_HORIZONTAL)
                   .Validator<wxGenericValidator>(&mThresholdLevel)
                   .MinSize({ 150, -1 })
                   .AddSlider({}, mThresholdLevel, Threshold.max, Threshold.min);

        // Click width
        mWidthT = S.Id(ID_Width)
                  .Validator<IntegerValidator<int> >(
            &mClickWidth, NumValidatorStyle::DEFAULT, Width.min, Width.max)
                  .AddTextBox(XXO("Max &Spike Width (higher is more sensitive):"),
                              wxT(""),
                              10);

        mWidthS = S.Id(ID_Width)
                  .Name(XO("Max Spike Width"))
                  .Style(wxSL_HORIZONTAL)
                  .Validator<wxGenericValidator>(&mClickWidth)
                  .MinSize({ 150, -1 })
                  .AddSlider({}, mClickWidth, Width.max, Width.min);
    }
    S.EndMultiColumn();

    return nullptr;
}

bool EffectClickRemoval::TransferDataToWindow(const EffectSettings&)
{
    if (!mUIParent->TransferDataToWindow()) {
        return false;
    }

    return true;
}

bool EffectClickRemoval::TransferDataFromWindow(EffectSettings&)
{
    if (!mUIParent->Validate() || !mUIParent->TransferDataFromWindow()) {
        return false;
    }

    return true;
}

void EffectClickRemoval::OnWidthText(wxCommandEvent& WXUNUSED(evt))
{
    mWidthT->GetValidator()->TransferFromWindow();
    mWidthS->GetValidator()->TransferToWindow();
}

void EffectClickRemoval::OnThreshText(wxCommandEvent& WXUNUSED(evt))
{
    mThreshT->GetValidator()->TransferFromWindow();
    mThreshS->GetValidator()->TransferToWindow();
}

void EffectClickRemoval::OnWidthSlider(wxCommandEvent& WXUNUSED(evt))
{
    mWidthS->GetValidator()->TransferFromWindow();
    mWidthT->GetValidator()->TransferToWindow();
}

void EffectClickRemoval::OnThreshSlider(wxCommandEvent& WXUNUSED(evt))
{
    mThreshS->GetValidator()->TransferFromWindow();
    mThreshT->GetValidator()->TransferToWindow();
}
