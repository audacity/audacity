/**********************************************************************

  Audacity: A Digital Audio Editor

  Repeat.cpp

  Dominic Mazzoni
  Vaughan Johnson

*******************************************************************//**

\class EffectRepeat
\brief An Effect that repeats audio several times over.

*//****************************************************************//**

\class RepeatDialog
\brief Dialog used with EffectRepeat

*//*******************************************************************/
#include "Repeat.h"
#include "EffectEditor.h"

#include <wx/stattext.h>

#include "ShuttleGui.h"
#include "../widgets/NumericTextCtrl.h"
#include "../widgets/valnum.h"

#include "LoadEffects.h"

namespace {
BuiltinEffectsModule::Registration<EffectRepeat> reg;
}

BEGIN_EVENT_TABLE(EffectRepeat, wxEvtHandler)
EVT_TEXT(wxID_ANY, EffectRepeat::OnRepeatTextChange)
END_EVENT_TABLE()

std::unique_ptr<EffectEditor> EffectRepeat::PopulateOrExchange(
    ShuttleGui& S, EffectInstance&, EffectSettingsAccess&,
    const EffectOutputs*)
{
    mUIParent = S.GetParent();
    S.StartHorizontalLay(wxCENTER, false);
    {
        mRepeatCount = S.Validator<IntegerValidator<int> >(
            &repeatCount, NumValidatorStyle::DEFAULT,
            Count.min, 2147483647 / mProjectRate)
                       .AddTextBox(XXO("&Number of repeats to add:"), L"", 12);
    }
    S.EndHorizontalLay();

    S.StartMultiColumn(1, wxCENTER);
    {
        mCurrentTime = S.AddVariableText(
            XO("Current selection length: dd:hh:mm:ss"));
        mTotalTime = S.AddVariableText(XO("New selection length: dd:hh:mm:ss"));
    }
    S.EndMultiColumn();
    return nullptr;
}

bool EffectRepeat::TransferDataToWindow(const EffectSettings&)
{
    mRepeatCount->ChangeValue(wxString::Format(wxT("%d"), repeatCount));

    DisplayNewTime();

    return true;
}

bool EffectRepeat::TransferDataFromWindow(EffectSettings&)
{
    if (!mUIParent->Validate()) {
        return false;
    }

    long l;

    mRepeatCount->GetValue().ToLong(&l);

    repeatCount = (int)l;

    return true;
}

void EffectRepeat::DisplayNewTime()
{
    long l;
    wxString str;
    mRepeatCount->GetValue().ToLong(&l);

    NumericConverter nc(FormatterContext::SampleRateContext(mProjectRate),
                        NumericConverterType_TIME(),
                        GetSelectionFormat(),
                        mT1 - mT0);

    str = wxString::Format(_("Current selection length: %s"), nc.GetString());

    mCurrentTime->SetLabel(str);
    mCurrentTime->SetName(str); // fix for bug 577 (NVDA/Narrator screen readers do not read static text in dialogs)

    if (l > 0) {
        EffectEditor::EnableApply(mUIParent, true);
        repeatCount = l;

        nc.SetValue((mT1 - mT0) * (repeatCount + 1));
        str = wxString::Format(_("New selection length: %s"), nc.GetString());
    } else {
        str = _("Warning: No repeats.");
        EffectEditor::EnableApply(mUIParent, false);
    }
    mTotalTime->SetLabel(str);
    mTotalTime->SetName(str); // fix for bug 577 (NVDA/Narrator screen readers do not read static text in dialogs)
}

void EffectRepeat::OnRepeatTextChange(wxCommandEvent& WXUNUSED(evt))
{
    DisplayNewTime();
}
