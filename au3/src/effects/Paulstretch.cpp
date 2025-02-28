/**********************************************************************

  Audacity: A Digital Audio Editor

  Paulstretch.cpp

  Nasca Octavian Paul (Paul Nasca)
  Some GUI code was taken from the Echo effect

**********************************************************************/
#include "Paulstretch.h"
#include "../widgets/valnum.h"
#include "AudacityMessageBox.h"
#include "EffectEditor.h"
#include "LoadEffects.h"
#include "ShuttleGui.h"
#include <wx/valgen.h>

//
// PaulstretchBase
//

namespace {
BuiltinEffectsModule::Registration< EffectPaulstretch > reg;
}

BEGIN_EVENT_TABLE(EffectPaulstretch, wxEvtHandler)
EVT_TEXT(wxID_ANY, EffectPaulstretch::OnText)
END_EVENT_TABLE()

std::unique_ptr<EffectEditor> EffectPaulstretch::PopulateOrExchange(
    ShuttleGui& S, EffectInstance&, EffectSettingsAccess&,
    const EffectOutputs*)
{
    mUIParent = S.GetParent();
    S.StartMultiColumn(2, wxALIGN_CENTER);
    {
        S
        .Validator<FloatingPointValidator<float> >(
            1, &mAmount, NumValidatorStyle::DEFAULT, Amount.min)
        /* i18n-hint: This is how many times longer the sound will be, e.g. applying
         * the effect to a 1-second sample, with the default Stretch Factor of 10.0
         * will give an (approximately) 10 second sound
         */
        .AddTextBox(XXO("&Stretch Factor:"), wxT(""), 10);

        S
        .Validator<FloatingPointValidator<float> >(
            3, &mTime_resolution, NumValidatorStyle::ONE_TRAILING_ZERO, Time.min)
        .AddTextBox(XXO("&Time Resolution (seconds):"), L"", 10);
    }
    S.EndMultiColumn();
    return nullptr;
}

bool EffectPaulstretch::TransferDataToWindow(const EffectSettings&)
{
    if (!mUIParent->TransferDataToWindow()) {
        return false;
    }

    return true;
}

bool EffectPaulstretch::TransferDataFromWindow(EffectSettings&)
{
    if (!mUIParent->Validate() || !mUIParent->TransferDataFromWindow()) {
        return false;
    }

    return true;
}

// EffectPaulstretch implementation

void EffectPaulstretch::OnText(wxCommandEvent& WXUNUSED(evt))
{
    EffectEditor::EnableApply(
        mUIParent, mUIParent->TransferDataFromWindow());
}
