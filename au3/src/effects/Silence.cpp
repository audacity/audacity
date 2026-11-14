/**********************************************************************

  Audacity: A Digital Audio Editor

  Silence.cpp

  Dominic Mazzoni

*******************************************************************//**

\class EffectSilence
\brief An effect to add silence.

*//*******************************************************************/
#include "Silence.h"
#include "EffectEditor.h"
#include "LoadEffects.h"

#include "ShuttleGui.h"
#include "../widgets/NumericTextCtrl.h"

namespace {
BuiltinEffectsModule::Registration< EffectSilence > reg;
}

// Effect implementation

std::unique_ptr<EffectEditor> EffectSilence::PopulateOrExchange(
    ShuttleGui& S, EffectInstance&, EffectSettingsAccess& access,
    const EffectOutputs*)
{
    S.StartVerticalLay();
    {
        S.StartHorizontalLay();
        {
            S.AddPrompt(XXO("&Duration:"));
            auto& extra = access.Get().extra;
            mDurationT = safenew
                             NumericTextCtrl(FormatterContext::SampleRateContext(mProjectRate),
                                             S.GetParent(), wxID_ANY,
                                             NumericConverterType_TIME(),
                                             extra.GetDurationFormat(),
                                             extra.GetDuration(),
                                             NumericTextCtrl::Options{}
                                             .AutoPos(true));
            S.Name(XO("Duration"))
            .Position(wxALIGN_CENTER | wxALL)
            .AddWindow(mDurationT);
        }
        S.EndHorizontalLay();
    }
    S.EndVerticalLay();

    return nullptr;
}

bool EffectSilence::TransferDataToWindow(const EffectSettings& settings)
{
    mDurationT->SetValue(settings.extra.GetDuration());

    return true;
}

bool EffectSilence::TransferDataFromWindow(EffectSettings& settings)
{
    settings.extra.SetDuration(mDurationT->GetValue());

    return true;
}
