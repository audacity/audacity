/**********************************************************************

  Audacity: A Digital Audio Editor

  Noise.cpp

  Dominic Mazzoni

*******************************************************************//**

\class EffectNoise
\brief An effect to add white noise.

*//*******************************************************************/
#include "Noise.h"
#include "EffectEditor.h"
#include "LoadEffects.h"

#include <wx/choice.h>
#include <wx/textctrl.h>
#include <wx/valgen.h>

#include "Prefs.h"
#include "ShuttleGui.h"
#include "../widgets/valnum.h"
#include "../widgets/NumericTextCtrl.h"

namespace {
BuiltinEffectsModule::Registration< EffectNoise > reg;
}

std::unique_ptr<EffectEditor> EffectNoise::PopulateOrExchange(
    ShuttleGui& S, EffectInstance&, EffectSettingsAccess& access,
    const EffectOutputs*)
{
    mUIParent = S.GetParent();

    wxASSERT(nTypes == WXSIZEOF(kTypeStrings));

    S.StartMultiColumn(2, wxCENTER);
    {
        S.Validator<wxGenericValidator>(&mType)
        .AddChoice(XXO("&Noise type:"), Msgids(kTypeStrings, nTypes));

        S
        .Validator<FloatingPointValidator<double> >(
            6, &mAmp, NumValidatorStyle::NO_TRAILING_ZEROES, Amp.min, Amp.max)
        .AddTextBox(XXO("&Amplitude (0-1):"), L"", 12);

        S.AddPrompt(XXO("&Duration:"));
        auto& extra = access.Get().extra;
        mNoiseDurationT = safenew
                              NumericTextCtrl(FormatterContext::SampleRateContext(mProjectRate),
                                              S.GetParent(), wxID_ANY,
                                              NumericConverterType_TIME(),
                                              extra.GetDurationFormat(),
                                              extra.GetDuration(),
                                              NumericTextCtrl::Options{}
                                              .AutoPos(true));
        S.Name(XO("Duration"))
        .Position(wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL | wxALL)
        .AddWindow(mNoiseDurationT);
    }
    S.EndMultiColumn();
    return nullptr;
}

bool EffectNoise::TransferDataToWindow(const EffectSettings& settings)
{
    if (!mUIParent->TransferDataToWindow()) {
        return false;
    }

    mNoiseDurationT->SetValue(settings.extra.GetDuration());
    return true;
}

bool EffectNoise::TransferDataFromWindow(EffectSettings& settings)
{
    if (!mUIParent->Validate() || !mUIParent->TransferDataFromWindow()) {
        return false;
    }

    settings.extra.SetDuration(mNoiseDurationT->GetValue());
    return true;
}
